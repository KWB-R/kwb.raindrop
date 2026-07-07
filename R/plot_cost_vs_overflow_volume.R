#' Cost vs. overflow-volume scatter with n_overflows-coloured points
#'
#' Companion to \code{\link{plot_wb_tradeoff_overflows}} for cost-aware
#' optimisation. Plots the per-scenario **total construction cost** (EUR) on
#' the x-axis against the **overflow volume** (m3) on the y-axis, with the
#' points coloured discretely by the **number** of overflow events (same
#' 0..x / >x palette used by `plot_wb_tradeoff_overflows`, legend at the top).
#'
#' Overflow volume is computed from `sum_overflows` (in mm on the swale
#' surface, as returned by [`add_overflow_events_and_waterbalance()`])
#' multiplied by `mulde_area` (m2) and converted to m3:
#' `overflow_volume_m3 = sum_overflows * mulde_area / 1000`.
#'
#' The tooltip carries the element water balance
#' (`element.WB_Evapotranspiration_`, `element.WB_InfiltrationNetto_`,
#' `element.WB_Oberflaechenablauf_Ueberlauf_`, all as % of the total water
#' input) and the cost breakdown (`cost_excavation`, `cost_profiling`,
#' `cost_filter`, `cost_storage`, `cost_total`) plus the varying parameters
#' from `param_grid` (excluding `scenario_name`), so the user can hover over a
#' scatter point and see exactly why it landed where it did.
#'
#' The plot language can be switched via `lang = "de"` or `lang = "en"`.
#' Titles / axis labels / legend / tooltip labels follow the choice unless
#' explicit overrides are supplied.
#'
#' @param simulation_results_optimisation Data frame with the columns
#'   `scenario_name`, `n_overflows`, `sum_overflows`, `mulde_area`,
#'   `element.WB_Evapotranspiration_`, `element.WB_InfiltrationNetto_`,
#'   `element.WB_Oberflaechenablauf_Ueberlauf_`, `cost_excavation`,
#'   `cost_profiling`, `cost_filter`, `cost_storage`, `cost_total`,
#'   `storage_type`. Typically the joined output of
#'   [`add_overflow_events_and_waterbalance()`] and [`compute_costs()`].
#' @param param_grid Data frame with parameter grid. Must contain
#'   `scenario_name`.
#' @param x Numeric threshold for the overflow-count colour bucket. Values
#'   greater than `x` are pushed into the red `">x"` category.
#' @param filter_n_gtx Logical. If `TRUE`, scenarios with `n_overflows > x`
#'   are dropped before plotting.
#' @param use_jitter,jitter_width,jitter_height,jitter_seed As in
#'   [`plot_wb_tradeoff_overflows()`].
#' @param digits Integer. Rounding for numeric values in the tooltip.
#' @param digits_params Integer. Rounding for parameter values in the
#'   tooltip.
#' @param lang Character. Plot language: `"de"` or `"en"`.
#' @param param_labels Named character vector translating `param_grid` columns
#'   to tooltip labels, or `NULL` to use [default_param_labels()] for `lang`.
#' @param title,lab_x,lab_y Optional character overrides for the default
#'   language-specific title / axis labels.
#' @param legend_position Character. Legend position, default `"top"`.
#'
#' @return A `ggplot` object. Convert to interactive via
#'   `plotly::ggplotly(p, tooltip = "text")`.
#'
#' @seealso [plot_cost_overflow_boxplot()] for the same data / tooltip shown as
#'   a cost-by-overflow-count boxplot.
#'
#' @export
#'
#' @importFrom dplyr %>% filter mutate left_join case_when
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual labs theme_bw position_jitter theme guides guide_legend
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang .data
plot_cost_vs_overflow_volume <- function(simulation_results_optimisation,
                                         param_grid,
                                         x = 1,
                                         filter_n_gtx = FALSE,
                                         use_jitter = TRUE,
                                         jitter_width = 0.15,
                                         jitter_height = 0.15,
                                         jitter_seed = 1L,
                                         digits = 2L,
                                         digits_params = 4L,
                                         lang = c("de", "en"),
                                         param_labels = NULL,
                                         title = NULL,
                                         lab_x = NULL,
                                         lab_y = NULL,
                                         legend_position = "top") {

  lang <- match.arg(lang)
  if (is.null(param_labels)) param_labels <- default_param_labels(lang)

  txt <- switch(
    lang,
    de = list(
      title = "Kosten vs. \u00dcberlaufvolumen",
      x = "Gesamtkosten [\u20ac]",
      y = "\u00dcberlaufvolumen [m\u00b3]",
      legend = "Anzahl \u00dcberlaufereignisse"
    ),
    en = list(
      title = "Cost vs. overflow volume",
      x = "Total cost [\u20ac]",
      y = "Overflow volume [m\u00b3]",
      legend = "Number of overflow events"
    )
  )
  txt <- c(txt, cost_tooltip_labels(lang))

  if (is.null(lab_x)) lab_x <- txt$x
  if (is.null(lab_y)) lab_y <- txt$y

  req_grid <- c("scenario_name")
  req_res  <- c(
    "scenario_name", "n_overflows", "sum_overflows", "mulde_area",
    "element.WB_Evapotranspiration_", "element.WB_InfiltrationNetto_",
    "element.WB_Oberflaechenablauf_Ueberlauf_",
    "cost_excavation", "cost_profiling", "cost_filter",
    "cost_storage", "cost_total", "storage_type"
  )

  miss_grid <- setdiff(req_grid, names(param_grid))
  miss_res  <- setdiff(req_res, names(simulation_results_optimisation))

  if (length(miss_grid) > 0) {
    stop("param_grid is missing column(s): ", paste(miss_grid, collapse = ", "))
  }
  if (length(miss_res) > 0) {
    stop(
      "simulation_results_optimisation is missing column(s): ",
      paste(miss_res, collapse = ", ")
    )
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    stop("x must be a single non-negative numeric value.")
  }

  x_int <- as.integer(round(x))
  if (!isTRUE(all.equal(x, x_int))) {
    warning("x is not an integer; using x_int = ", x_int,
            " for discrete palette/legend.")
  }

  # Append the share of scenarios meeting the validity criterion
  # (n_overflows <= x) to the auto title (ggplotly drops ggplot subtitles).
  valid_pct <- round(100 * mean(
    simulation_results_optimisation$n_overflows <= x_int, na.rm = TRUE))
  share_txt <- switch(lang,
    de = paste0(valid_pct, " % mit \u2264 ", x_int, " \u00dcberl\u00e4ufen"),
    en = paste0(valid_pct, " % with \u2264 ", x_int, " overflows"))
  if (is.null(title)) title <- paste0(txt$title, " (", share_txt, ")")

  param_tooltip <- build_varying_param_html(param_grid, lang, param_labels,
                                            digits_params)

  df <- simulation_results_optimisation %>%
    dplyr::left_join(param_tooltip, by = "scenario_name") %>%
    dplyr::filter(!isTRUE(filter_n_gtx) |
                    is.na(.data$n_overflows) |
                    .data$n_overflows <= x_int) %>%
    dplyr::mutate(
      overflow_volume_m3 = .data$sum_overflows * .data$mulde_area / 1000
    )

  hi_lab <- paste0(">", x_int)
  df <- df %>%
    dplyr::mutate(
      overflow_cat = dplyr::case_when(
        is.na(.data$n_overflows)  ~ NA_character_,
        .data$n_overflows > x_int ~ hi_lab,
        TRUE                      ~ as.character(.data$n_overflows)
      )
    )

  base_levels <- as.character(0:x_int)
  levs <- c(base_levels, hi_lab)

  df <- df %>%
    dplyr::mutate(
      overflow_cat = factor(.data$overflow_cat, levels = levs)
    )

  df$tooltip_html <- cost_tooltip_text(df, txt, digits)

  if (x_int == 0L) {
    pal <- c("0" = "orange", ">0" = "red")
  } else if (x_int == 1L) {
    pal <- c("0" = "darkgreen", "1" = "orange", ">1" = "red")
  } else {
    pal_green <- grDevices::colorRampPalette(c("darkgreen", "yellowgreen"))(x_int)
    pal_vals <- c(pal_green, "orange", "red")
    pal_names <- c(base_levels, hi_lab)
    pal <- stats::setNames(pal_vals, pal_names)
  }

  legend_breaks <- levs

  pos <- if (isTRUE(use_jitter)) {
    ggplot2::position_jitter(
      width = jitter_width,
      height = jitter_height,
      seed = jitter_seed
    )
  } else {
    "identity"
  }

  legend_direction <- if (legend_position %in% c("top", "bottom")) {
    "horizontal"
  } else {
    "vertical"
  }

  legend_nrow <- if (legend_direction == "horizontal") 1 else NULL
  legend_ncol <- if (legend_direction == "vertical") 1 else NULL

  p <- ggplot2::ggplot(df, ggplot2::aes(
    x = .data$cost_total,
    y = .data$overflow_volume_m3,
    color = .data$overflow_cat,
    text = .data$tooltip_html
  )) +
    ggplot2::geom_point(alpha = 0.7, position = pos) +
    ggplot2::scale_color_manual(
      values = pal,
      breaks = legend_breaks,
      limits = levs,
      drop = FALSE,
      name = txt$legend
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        direction = legend_direction,
        nrow = legend_nrow,
        ncol = legend_ncol,
        byrow = TRUE
      )
    ) +
    ggplot2::labs(
      title = title,
      x = lab_x,
      y = lab_y
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = legend_position,
      legend.direction = legend_direction
    )

  p
}
