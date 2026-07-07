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
#' The tooltip carries the cost breakdown (`cost_excavation`,
#' `cost_profiling`, `cost_filter`, `cost_storage`, `cost_total`) plus the
#' varying parameters from `param_grid` (excluding `scenario_name`), so the
#' user can hover over a scatter point and see exactly why it landed where
#' it did.
#'
#' The plot language can be switched via `lang = "de"` or `lang = "en"`.
#' Titles / axis labels / legend / tooltip labels follow the choice unless
#' explicit overrides are supplied.
#'
#' @param simulation_results_optimisation Data frame with the columns
#'   `scenario_name`, `n_overflows`, `sum_overflows`, `mulde_area`,
#'   `cost_excavation`, `cost_profiling`, `cost_filter`, `cost_storage`,
#'   `cost_total`. Typically the joined output of
#'   [`add_overflow_events_and_waterbalance()`] and
#'   [`compute_costs()`].
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
#' @param title,lab_x,lab_y Optional character overrides for the default
#'   language-specific title / axis labels.
#' @param legend_position Character. Legend position, default `"top"`.
#'
#' @return A `ggplot` object. Convert to interactive via
#'   `plotly::ggplotly(p, tooltip = "text")`.
#'
#' @export
#'
#' @importFrom dplyr %>% select summarise across everything n_distinct filter pull mutate group_by left_join case_when all_of
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_chr
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
                                         title = NULL,
                                         lab_x = NULL,
                                         lab_y = NULL,
                                         legend_position = "top") {

  lang <- match.arg(lang)

  txt <- switch(
    lang,
    de = list(
      title = paste0(
        "Kosten vs. Überlaufvolumen (Anzahl Überläufe ≤ ", x, ")"
      ),
      x = "Gesamtkosten [€]",
      y = "Überlaufvolumen [m³]",
      legend = "Anzahl Überlaufereignisse",
      tt_scenario = "Szenario",
      tt_n_overflows = "Anzahl Überlaufereignisse",
      tt_sum_overflows_mm = "Summe Überläufe [mm]",
      tt_overflow_volume = "Überlaufvolumen [m³]",
      tt_cost_total = "Gesamtkosten",
      tt_cost_excavation = "Aushub",
      tt_cost_profiling = "Profilierung + Begrünung",
      tt_cost_filter = "Bodenfilter",
      tt_cost_storage = "Speicherschicht",
      tt_costs_header = "Kostenaufteilung [€]",
      tt_params = "Variierende Parameter"
    ),
    en = list(
      title = paste0(
        "Cost vs. overflow volume (overflow events ≤ ", x, ")"
      ),
      x = "Total cost [€]",
      y = "Overflow volume [m³]",
      legend = "Number of overflow events",
      tt_scenario = "Scenario",
      tt_n_overflows = "Number of overflow events",
      tt_sum_overflows_mm = "Sum of overflows [mm]",
      tt_overflow_volume = "Overflow volume [m³]",
      tt_cost_total = "Total cost",
      tt_cost_excavation = "Excavation",
      tt_cost_profiling = "Profiling + greening",
      tt_cost_filter = "Soil filter",
      tt_cost_storage = "Storage layer",
      tt_costs_header = "Cost breakdown [€]",
      tt_params = "Varying parameters"
    )
  )

  if (is.null(title)) title <- txt$title
  if (is.null(lab_x)) lab_x <- txt$x
  if (is.null(lab_y)) lab_y <- txt$y

  req_grid <- c("scenario_name")
  req_res  <- c(
    "scenario_name", "n_overflows", "sum_overflows", "mulde_area",
    "cost_excavation", "cost_profiling", "cost_filter",
    "cost_storage", "cost_total"
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

  varying_params <- param_grid %>%
    dplyr::select(-"scenario_name") %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   ~ dplyr::n_distinct(.) > 1)) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "param",
                        values_to = "vary") %>%
    dplyr::filter(.data$vary) %>%
    dplyr::pull("param")

  if (length(varying_params) == 0) {
    param_tooltip <- param_grid %>%
      dplyr::select("scenario_name") %>%
      dplyr::mutate(params_html = "")
  } else {
    param_tooltip <- param_grid %>%
      dplyr::select("scenario_name", dplyr::all_of(varying_params)) %>%
      tidyr::pivot_longer(-"scenario_name",
                          names_to = "param",
                          values_to = "val") %>%
      dplyr::mutate(
        val_chr = purrr::map_chr(.data$val, ~ paste(.x, collapse = ",")),
        val_num = suppressWarnings(as.numeric(.data$val_chr)),
        val_fmt = ifelse(
          is.na(.data$val_num),
          .data$val_chr,
          format(round(.data$val_num, digits_params), trim = TRUE)
        ),
        kv = paste0(.data$param, "=", .data$val_fmt)
      ) %>%
      dplyr::group_by(.data$scenario_name) %>%
      dplyr::summarise(params_html = paste(.data$kv, collapse = "<br>"),
                       .groups = "drop")
  }

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
    text = paste0(
      txt$tt_scenario, ": ", .data$scenario_name,
      "<br>", txt$tt_n_overflows, ": ", .data$n_overflows,
      "<br>", txt$tt_sum_overflows_mm, ": ", round(.data$sum_overflows, digits),
      "<br>", txt$tt_overflow_volume, ": ",
      round(.data$overflow_volume_m3, digits),
      "<br><br><b>", txt$tt_costs_header, "</b>",
      "<br>", txt$tt_cost_excavation, ": ",
      format(round(.data$cost_excavation, 0), big.mark = " ", trim = TRUE),
      "<br>", txt$tt_cost_profiling, ": ",
      format(round(.data$cost_profiling, 0), big.mark = " ", trim = TRUE),
      "<br>", txt$tt_cost_filter, ": ",
      format(round(.data$cost_filter, 0), big.mark = " ", trim = TRUE),
      "<br>", txt$tt_cost_storage, ": ",
      format(round(.data$cost_storage, 0), big.mark = " ", trim = TRUE),
      "<br><b>", txt$tt_cost_total, ": ",
      format(round(.data$cost_total, 0), big.mark = " ", trim = TRUE), "</b>",
      "<br><br><b>", txt$tt_params, "</b><br>", .data$params_html
    )
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
