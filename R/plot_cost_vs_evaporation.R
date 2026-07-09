#' Cost vs. evapotranspiration scatter with storage-type shapes
#'
#' Second companion to \code{\link{plot_cost_vs_overflow_volume}} for
#' cost-aware optimisation. Plots the per-scenario **total construction cost**
#' (EUR) on the x-axis against the element **evapotranspiration share** (% of
#' the total water input, from `element.WB_Evapotranspiration_`) on the
#' y-axis. Points are coloured discretely by the **number** of overflow events
#' (same 0..x / >x palette used by the sibling plots, legend at the top) and
#' **shaped by the storage type**: filled square = infiltration box
#' (Sickerbox), filled triangle = gravel trench (Schotterrigol).
#'
#' The tooltip is identical to [plot_cost_vs_overflow_volume()]: scenario,
#' overflow count / sum (mm) / volume (m3), the element water balance
#' (`element.WB_Evapotranspiration_`, `element.WB_InfiltrationNetto_`,
#' `element.WB_Oberflaechenablauf_Ueberlauf_`, all as % of the total water
#' input), the storage type, the usable storage volume of the storage layer
#' (m3), the cost breakdown (`cost_excavation`,
#' `cost_profiling`, `cost_filter`, `cost_storage`, `cost_total`), the derived
#' **cost per percentage point of evapotranspiration** (EUR/%) plus the
#' varying parameters from `param_grid` (excluding `scenario_name`).
#'
#' The plot language can be switched via `lang = "de"` or `lang = "en"`.
#' Titles / axis labels / legend / tooltip labels follow the choice unless
#' explicit overrides are supplied.
#'
#' @inheritParams plot_cost_vs_overflow_volume
#'
#' @return A `ggplot` object. Convert to interactive via
#'   `plotly::ggplotly(p, tooltip = "text")`.
#'
#' @seealso [plot_cost_vs_overflow_volume()] for cost vs. overflow volume and
#'   [plot_cost_overflow_boxplot()] for the boxplot views (including
#'   `y_var = "cost_per_evap_pct"`, the cost per percentage point of
#'   evapotranspiration).
#'
#' @export
#'
#' @importFrom dplyr %>% filter mutate left_join case_when
#' @importFrom ggplot2 ggplot aes geom_point scale_color_manual scale_shape_manual labs theme_bw position_jitter theme guides guide_legend
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang .data
plot_cost_vs_evaporation <- function(simulation_results_optimisation,
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
      title = "Kosten vs. Verdunstung",
      x = "Gesamtkosten [\u20ac]",
      y = "Verdunstung [%]",
      legend = "Anzahl \u00dcberlaufereignisse"
    ),
    en = list(
      title = "Cost vs. evapotranspiration",
      x = "Total cost [\u20ac]",
      y = "Evapotranspiration [%]",
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
    de = paste0(valid_pct, " % mit <= ", x_int, " \u00dcberl\u00e4ufen"),
    en = paste0(valid_pct, " % with <= ", x_int, " overflows"))
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

  # Storage type drives the marker shape (square = infiltration box,
  # triangle = gravel trench); shared with the sibling cost plots.
  st <- storage_type_shapes(df$storage_type, txt)
  df$storage_type_disp <- st$display

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
    y = .data[["element.WB_Evapotranspiration_"]],
    color = .data$overflow_cat,
    shape = .data$storage_type_disp,
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
    ggplot2::scale_shape_manual(
      values = st$shape_values,
      drop = FALSE,
      name = txt$tt_storage_type
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        direction = legend_direction,
        nrow = legend_nrow,
        ncol = legend_ncol,
        byrow = TRUE,
        order = 1
      ),
      shape = ggplot2::guide_legend(
        direction = legend_direction,
        nrow = legend_nrow,
        ncol = legend_ncol,
        byrow = TRUE,
        order = 2
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
      legend.direction = legend_direction,
      # stack the colour and shape legends so both fit at the top
      legend.box = if (legend_direction == "horizontal") "vertical" else "horizontal"
    )

  p
}
