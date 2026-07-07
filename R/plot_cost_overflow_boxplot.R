#' Cost boxplot per overflow-event count, points sized by overflow volume
#'
#' Companion to [plot_cost_vs_overflow_volume()]. For every number of overflow
#' events (x-axis) it draws a boxplot of the total construction cost (y-axis,
#' EUR) across all scenarios with that count, overlaid with the individual
#' scenarios as jittered points whose **size scales with `size_by`** --
#' the overflow volume (`m3`, `sum_overflows` (`mm`) * `mulde_area` (`m2`) /
#' 1000; the default) or the element evapotranspiration share (%). One best
#' scenario per box is highlighted; `best_by` selects its objective -- cheapest,
#' smallest overflow volume, or highest evapotranspiration (cost as
#' tie-breaker) -- so the three variants trace three different frontier lines.
#' `label_best` annotates the marker.
#'
#' Overflow counts greater than `x` are collapsed into a single `">x"`
#' catch-all box (furthest right, coloured red), keeping the axis readable for
#' the long-tailed 15-year runs (Wien / Bad Aussee reach several hundred
#' overflow events). Its highlighted scenario is the one with the **fewest**
#' overflow events above `x` (closest to the valid region). Set `x` high to
#' resolve more counts individually, or to `max(n_overflows)` to give every
#' count its own box.
#'
#' The point tooltip is **identical** to [plot_cost_vs_overflow_volume()]:
#' scenario, overflow count / sum (`mm`) / volume (`m3`), the element water
#' balance (evapotranspiration / infiltration / overflow, %), the cost breakdown
#' (EUR)
#' and the varying `param_grid` parameters translated via `param_labels`.
#' Points and boxes are coloured with the same green (low counts) to red
#' (`">x"`) palette as the sibling plots; because the colour merely echoes the
#' x-axis it carries no separate legend -- only the point-size legend is shown.
#'
#' The point-size scale is calibrated to the valid region (`0..x`): the extreme
#' overflow volumes of the `">x"` catch-all are capped and a minimum size keeps
#' even zero-volume points (the `0`-overflow box) visible, so the many-overflow
#' outliers no longer shrink every valid-region point to an invisible dot.
#'
#' @inheritParams plot_cost_vs_overflow_volume
#' @param x Numeric threshold. Counts `0..x` each get their own box; counts
#'   `> x` collapse into a single `">x"` box.
#' @param filter_n_gtx Logical. If `TRUE`, scenarios with `n_overflows > x`
#'   are dropped (removing the `">x"` box) before plotting.
#' @param use_jitter Logical. If `TRUE`, points are horizontally jittered.
#' @param jitter_width Numeric. Horizontal jitter half-width.
#' @param jitter_seed Integer. Seed for reproducible jitter.
#' @param max_point_size Numeric. Point size for the largest (valid-region)
#'   `size_by` value; the smallest maps to a fixed minimum so no point vanishes.
#' @param box_alpha,point_alpha Numeric in `[0, 1]`. Box-fill / point opacity.
#' @param lab_size Optional character override for the size-legend title.
#' @param size_by Character. Which variable drives the point area (and its
#'   legend): `"overflow_volume"` (default, m3) or `"evapotranspiration"` (the
#'   element evapotranspiration share in %, from `element.WB_Evapotranspiration_`
#'   -- larger points then mean *more* evapotranspiration, which is desirable).
#' @param best_by Character. Objective for the highlighted best scenario per
#'   box, with cost as the tie-breaker: `"min_cost"` (default; cheapest, ties
#'   broken by `scenario_name`), `"min_overflow"` (smallest overflow volume) or
#'   `"max_evapotranspiration"` (highest evapotranspiration). In the `">x"` box
#'   the fewest-overflow scenario is picked first, `best_by` then breaking ties.
#' @param label_best Logical. If `TRUE`, the best scenario per box is annotated
#'   next to it: overflow volume plus overflow share (`"NN m3 / NN %"`) for
#'   `min_overflow`, the evapotranspiration share (`"NN %"`) for
#'   `max_evapotranspiration`, or the total cost for `min_cost`. Default
#'   `FALSE`.
#' @param mark_best Logical. If `TRUE` (default), the best scenario per box
#'   (see `best_by`) is highlighted with a black-outlined diamond filled in
#'   that box's group colour, so its plotly tooltip inherits the group colour.
#' @param connect_best Logical. If `TRUE` (default), the highlighted best
#'   scenarios of **all** boxes (overflow counts `0..x` plus the `">x"`
#'   catch-all) are connected by a line -- the best-per-overflow-level frontier.
#'
#' @return A `ggplot` object. Convert to interactive via
#'   `plotly::ggplotly(p, tooltip = "text")`.
#'
#' @seealso [plot_cost_vs_overflow_volume()]
#'
#' @export
#'
#' @importFrom dplyr %>% filter mutate left_join case_when group_by arrange desc slice ungroup
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_line geom_point geom_text position_jitter position_nudge scale_size scale_color_manual scale_fill_manual scale_x_discrete labs theme_bw theme element_text
#' @importFrom grDevices colorRampPalette
#' @importFrom rlang .data
plot_cost_overflow_boxplot <- function(simulation_results_optimisation,
                                       param_grid,
                                       x = 5,
                                       filter_n_gtx = FALSE,
                                       use_jitter = TRUE,
                                       jitter_width = 0.2,
                                       jitter_seed = 1L,
                                       max_point_size = 6,
                                       box_alpha = 0.35,
                                       point_alpha = 0.6,
                                       digits = 2L,
                                       digits_params = 4L,
                                       lang = c("de", "en"),
                                       param_labels = NULL,
                                       size_by = c("overflow_volume",
                                                   "evapotranspiration"),
                                       best_by = c("min_cost",
                                                   "min_overflow",
                                                   "max_evapotranspiration"),
                                       label_best = FALSE,
                                       title = NULL,
                                       lab_x = NULL,
                                       lab_y = NULL,
                                       lab_size = NULL,
                                       mark_best = TRUE,
                                       connect_best = TRUE,
                                       legend_position = "right") {

  lang <- match.arg(lang)
  size_by <- match.arg(size_by)
  best_by <- match.arg(best_by)
  if (is.null(param_labels)) param_labels <- default_param_labels(lang)

  size_col <- if (size_by == "evapotranspiration") {
    "element.WB_Evapotranspiration_"
  } else {
    "overflow_volume_m3"
  }

  txt <- switch(
    lang,
    de = list(
      x = "Anzahl \u00dcberlaufereignisse",
      y = "Gesamtkosten [\u20ac]",
      title_cheapest     = "Kosten je \u00dcberlaufanzahl \u2014 g\u00fcnstigste je Kategorie",
      title_min_overflow = "Kosten je \u00dcberlaufanzahl \u2014 geringstes \u00dcberlaufvolumen je Kategorie",
      title_max_evap     = "Kosten je \u00dcberlaufanzahl \u2014 h\u00f6chste Verdunstung je Kategorie",
      size_volume = "\u00dcberlaufvolumen [m\u00b3]",
      size_evap = "Verdunstung [%]",
      best_cheapest     = "G\u00fcnstigste L\u00f6sung",
      best_min_overflow = "Geringstes \u00dcberlaufvolumen",
      best_max_evap     = "H\u00f6chste Verdunstung"
    ),
    en = list(
      x = "Number of overflow events",
      y = "Total cost [\u20ac]",
      title_cheapest     = "Cost by overflow count \u2014 cheapest per class",
      title_min_overflow = "Cost by overflow count \u2014 lowest overflow volume per class",
      title_max_evap     = "Cost by overflow count \u2014 highest evapotranspiration per class",
      size_volume = "Overflow volume [m\u00b3]",
      size_evap = "Evapotranspiration [%]",
      best_cheapest     = "Cheapest solution",
      best_min_overflow = "Lowest overflow volume",
      best_max_evap     = "Highest evapotranspiration"
    )
  )
  txt <- c(txt, cost_tooltip_labels(lang))

  def_title <- switch(best_by,
    min_cost             = txt$title_cheapest,
    min_overflow         = txt$title_min_overflow,
    max_evapotranspiration = txt$title_max_evap)
  def_size  <- if (size_by == "evapotranspiration") txt$size_evap else txt$size_volume
  best_lab  <- switch(best_by,
    min_cost             = txt$best_cheapest,
    min_overflow         = txt$best_min_overflow,
    max_evapotranspiration = txt$best_max_evap)

  if (is.null(title)) title <- def_title
  if (is.null(lab_x)) lab_x <- txt$x
  if (is.null(lab_y)) lab_y <- txt$y
  if (is.null(lab_size)) lab_size <- def_size

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
    stop("simulation_results_optimisation is missing column(s): ",
         paste(miss_res, collapse = ", "))
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x < 0) {
    stop("x must be a single non-negative numeric value.")
  }

  x_int <- as.integer(round(x))
  if (!isTRUE(all.equal(x, x_int))) {
    warning("x is not an integer; using x_int = ", x_int,
            " for discrete axis/palette.")
  }

  param_tooltip <- build_varying_param_html(param_grid, lang, param_labels,
                                            digits_params)

  # Counts greater than x collapse into a single ">x" catch-all box, shown
  # furthest right and coloured red. Its highlighted scenario is the one with
  # the fewest overflow events (closest to the valid region); see the best
  # selection below.
  hi_lab <- paste0(">", x_int)
  base_levels <- as.character(0:x_int)
  levs <- c(base_levels, hi_lab)

  df <- simulation_results_optimisation %>%
    dplyr::left_join(param_tooltip, by = "scenario_name") %>%
    dplyr::filter(!isTRUE(filter_n_gtx) |
                    is.na(.data$n_overflows) |
                    .data$n_overflows <= x_int) %>%
    dplyr::mutate(
      overflow_volume_m3 = .data$sum_overflows * .data$mulde_area / 1000,
      overflow_cat = dplyr::case_when(
        is.na(.data$n_overflows)  ~ NA_character_,
        .data$n_overflows > x_int ~ hi_lab,
        TRUE                      ~ as.character(.data$n_overflows)
      ),
      overflow_cat = factor(.data$overflow_cat, levels = levs)
    )

  df$tooltip_html <- cost_tooltip_text(df, txt, digits)

  # Point size: calibrate the scale to the valid region (0..x) and cap the
  # extreme ">x" values, otherwise the many-overflow outliers (overflow
  # volumes of several thousand m3) shrink every valid-region point to an
  # invisible dot. pmin() caps; scale_size() below adds a minimum size so even
  # zero-volume points (the 0-overflow box) stay visible.
  valid_size <- df[[size_col]][!is.na(df$n_overflows) & df$n_overflows <= x_int]
  size_cap <- suppressWarnings(max(valid_size[is.finite(valid_size)]))
  if (!is.finite(size_cap) || size_cap <= 0) {
    size_cap <- suppressWarnings(max(df[[size_col]], na.rm = TRUE))
  }
  if (!is.finite(size_cap) || size_cap <= 0) size_cap <- 1
  df$size_plot <- pmin(df[[size_col]], size_cap)

  # Best scenario per box. n_overflows is the first sort key, so the ">x"
  # catch-all box highlights the scenario with the fewest overflow events
  # (closest to the valid region); the objective (best_by) then breaks ties,
  # with cost as the final tie-breaker:
  #   min_cost               -> cheapest
  #   min_overflow           -> smallest overflow volume
  #   max_evapotranspiration -> highest evapotranspiration
  # For the single-count boxes 0..x, n_overflows is constant, so only the
  # objective matters there. The frontier line runs through the best of every
  # box, so the three objectives yield three different lines.
  best_grp <- df %>%
    dplyr::filter(!is.na(.data$overflow_cat), !is.na(.data$cost_total)) %>%
    dplyr::group_by(.data$overflow_cat)
  best <- switch(best_by,
    max_evapotranspiration = best_grp %>%
      dplyr::arrange(.data$n_overflows,
                     dplyr::desc(.data[["element.WB_Evapotranspiration_"]]),
                     .data$cost_total, .data$scenario_name, .by_group = TRUE),
    min_overflow = best_grp %>%
      dplyr::arrange(.data$n_overflows, .data$overflow_volume_m3,
                     .data$cost_total, .data$scenario_name, .by_group = TRUE),
    min_cost = best_grp %>%
      dplyr::arrange(.data$n_overflows, .data$cost_total,
                     .data$scenario_name, .by_group = TRUE)
  )
  best <- best %>% dplyr::slice(1L) %>% dplyr::ungroup()
  best$tooltip_best <- paste0("<b>", best_lab, "</b><br>", best$tooltip_html)
  if (isTRUE(label_best)) {
    best$label_text <- switch(best_by,
      min_overflow = paste0(
        format(round(best$overflow_volume_m3, 1), trim = TRUE), " m\u00b3 / ",
        format(round(best[["element.WB_Oberflaechenablauf_Ueberlauf_"]], 1),
               trim = TRUE), " %"),
      max_evapotranspiration = paste0(
        format(round(best[["element.WB_Evapotranspiration_"]], 1),
               trim = TRUE), " %"),
      min_cost = paste0(
        format(round(best$cost_total, 0), big.mark = " ", trim = TRUE), " \u20ac")
    )
  }

  if (x_int == 0L) {
    pal <- stats::setNames(c("orange", "red"), c("0", hi_lab))
  } else if (x_int == 1L) {
    pal <- stats::setNames(c("darkgreen", "orange", "red"),
                           c("0", "1", hi_lab))
  } else {
    pal_green <- grDevices::colorRampPalette(c("darkgreen", "yellowgreen"))(x_int)
    pal <- stats::setNames(c(pal_green, "orange", "red"),
                           c(base_levels, hi_lab))
  }

  pos <- if (isTRUE(use_jitter)) {
    ggplot2::position_jitter(width = jitter_width, height = 0,
                             seed = jitter_seed)
  } else {
    "identity"
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$overflow_cat,
                                        y = .data$cost_total)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data$overflow_cat),
      alpha = box_alpha, outlier.shape = NA, colour = "grey40"
    ) +
    ggplot2::geom_jitter(
      ggplot2::aes(size = .data$size_plot,
                   colour = .data$overflow_cat,
                   text = .data$tooltip_html),
      position = pos, alpha = point_alpha
    ) +
    ggplot2::scale_size(range = c(1.5, max_point_size), name = lab_size) +
    ggplot2::scale_fill_manual(values = pal, limits = levs, drop = FALSE,
                               guide = "none") +
    ggplot2::scale_color_manual(values = pal, limits = levs, drop = FALSE,
                                guide = "none") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(title = title, x = lab_x, y = lab_y) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = legend_position,
      plot.title = ggplot2::element_text(size = 11)
    )

  # Frontier line across the best of every box (all classes), then the
  # group-coloured best-marker on top of everything.
  if (isTRUE(connect_best) && nrow(best) > 1L) {
    p <- p + ggplot2::geom_line(
      data = best,
      ggplot2::aes(x = .data$overflow_cat, y = .data$cost_total, group = 1L),
      colour = "black", linewidth = 0.7, na.rm = TRUE
    )
  }
  if (isTRUE(mark_best) && nrow(best) > 0L) {
    p <- p + ggplot2::geom_point(
      data = best,
      ggplot2::aes(x = .data$overflow_cat, y = .data$cost_total,
                   fill = .data$overflow_cat,
                   text = .data$tooltip_best),
      shape = 23, size = 3.2, colour = "black", stroke = 1.2, na.rm = TRUE
    )
  }
  if (isTRUE(label_best) && "label_text" %in% names(best) && nrow(best) > 0L) {
    # place the label just above the marker (centred), so long labels such as
    # "3515 m3 / 35 %" never run off the right edge of the last box.
    lab_nudge_y <- 0.045 * diff(range(df$cost_total, na.rm = TRUE))
    p <- p + ggplot2::geom_text(
      data = best,
      ggplot2::aes(x = .data$overflow_cat, y = .data$cost_total,
                   label = .data$label_text),
      position = ggplot2::position_nudge(y = lab_nudge_y),
      hjust = 0.5, vjust = 0, size = 2.8, colour = "black", na.rm = TRUE
    )
  }

  p
}
