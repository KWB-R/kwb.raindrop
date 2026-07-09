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
#' x-axis it carries no separate legend. When both storage types share one
#' panel, the overlaid points are additionally **shaped by the storage type**
#' (filled square = infiltration box / Sickerbox, filled triangle = gravel
#' trench / Schotterrigol), matching the scatter siblings, and a storage-type
#' legend is shown next to the point-size legend. With `facet_storage_type =
#' TRUE` the plot splits into two stacked storage-type panels instead; the
#' facet strips then carry that information and the points stay **plain
#' circles** for readability. `y_var = "cost_per_evap_pct"` switches the
#' y-axis to the cost per percentage point of evapotranspiration (EUR/%).
#'
#' The point-size scale is calibrated to the valid region (`0..x`): the extreme
#' overflow volumes of the `">x"` catch-all are capped and a minimum size keeps
#' even zero-volume points (the `0`-overflow box) visible, so the many-overflow
#' outliers no longer shrink every valid-region point to an invisible dot.
#' When the storage-type shapes are in use (no faceting), its legend keys are
#' drawn with the storage-type marker (grey; the single present shape, or the
#' square when both types are shown) instead of the default circle; the
#' faceted variant uses circular points and matching circular keys.
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
#'   "Cost" always refers to the active `y_var`, so with
#'   `y_var = "cost_per_evap_pct"` the `"min_cost"` objective picks the
#'   scenario with the lowest cost per percentage point of evapotranspiration.
#' @param y_var Character. Which cost measure the y-axis (boxes, points, best
#'   markers, frontier) shows: `"cost_total"` (default; total construction
#'   cost, EUR) or `"cost_per_evap_pct"` (total cost divided by the element
#'   evapotranspiration share **above the reference minimum**, EUR per
#'   percentage point -- the marginal cost efficiency of evapotranspiration;
#'   the baseline comes "for free"). The reference is the **lowest
#'   evapotranspiration among the scenarios that satisfy the validity
#'   criterion** (`n_overflows <= x`; fallback: the complete run when none
#'   does) and is named -- share, criterion and scenario id -- on a second
#'   title line. Scenarios at or below the reference (including the reference
#'   scenario itself) have no defined marginal cost and are dropped from the
#'   `"cost_per_evap_pct"` variant; `label_best = TRUE` additionally
#'   annotates the evapotranspiration gain (`"(+NN % Evapotranspiration)"`)
#'   after the price. Titles and the y-axis label switch accordingly.
#' @param facet_storage_type Logical. If `TRUE`, the plot is split by
#'   `storage_type` into two stacked panels (infiltration box on top, gravel
#'   trench below, via `ggplot2::facet_grid()`), each with its own boxes,
#'   best-per-box markers and frontier line; the overlaid points then stay
#'   plain circles (the strips already name the type).
#'   `plotly::ggplotly()` keeps the panel split as stacked subplots.
#'   Default `FALSE`.
#' @param label_best Logical. If `TRUE`, the best scenario per box is annotated
#'   next to it: overflow volume plus overflow share (`"NN m3 / NN %"`) for
#'   `min_overflow`, the evapotranspiration share (`"NN %"`) for
#'   `max_evapotranspiration`, or the active `y_var` value for `min_cost` --
#'   the total cost (`"NN EUR"`) by default, the cost per percentage point of
#'   evapotranspiration (`"NN EUR/%"`) with `y_var = "cost_per_evap_pct"`.
#'   Default `FALSE`.
#' @param legend_position Character. Legend position, default `"right"`.
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
#' @importFrom dplyr %>% filter mutate left_join case_when group_by arrange desc slice ungroup if_else
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter geom_line geom_point geom_text position_jitter position_nudge scale_size scale_color_manual scale_fill_manual scale_shape_manual scale_x_discrete facet_grid vars guides guide_legend labs theme_bw theme element_text
#' @importFrom grDevices colorRampPalette
#' @importFrom utils modifyList
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
                                       y_var = c("cost_total",
                                                 "cost_per_evap_pct"),
                                       facet_storage_type = FALSE,
                                       label_best = FALSE,
                                       title = NULL,
                                       lab_x = NULL,
                                       lab_y = NULL,
                                       caption = NULL,
                                       lab_size = NULL,
                                       mark_best = TRUE,
                                       connect_best = TRUE,
                                       legend_position = "right") {

  lang <- match.arg(lang)
  size_by <- match.arg(size_by)
  best_by <- match.arg(best_by)
  y_var <- match.arg(y_var)
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
      title_max_evap     = "Kosten je \u00dcberlaufanzahl \u2014 h\u00f6chste Evapotranspiration je Kategorie",
      size_volume = "\u00dcberlaufvolumen [m\u00b3]",
      size_evap = "Evapotranspiration [%]",
      best_cheapest     = "G\u00fcnstigste L\u00f6sung",
      best_min_overflow = "Geringstes \u00dcberlaufvolumen",
      best_max_evap     = "H\u00f6chste Evapotranspiration"
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

  # Cost-efficiency variant: y = total cost per percentage point of element
  # evapotranspiration [EUR/%] instead of the plain total cost. Only the
  # y-dependent labels change; palette / boxes / tooltip stay identical.
  y_col <- if (y_var == "cost_per_evap_pct") "cost_per_evap_pct" else "cost_total"
  if (y_var == "cost_per_evap_pct") {
    txt$y <- switch(lang,
      de = "Kosten je Prozent Evapotranspiration \u00fcber Minimum [\u20ac/%]",
      en = "Cost per percent evapotranspiration above minimum [\u20ac/%]")
    evap_prefix <- switch(lang,
      de = "Kosten je % Evapotranspiration",
      en = "Cost per % evapotranspiration")
    txt$title_cheapest <- paste0(evap_prefix, switch(lang,
      de = " \u2014 g\u00fcnstigste je Kategorie",
      en = " \u2014 cheapest per class"))
    txt$title_min_overflow <- paste0(evap_prefix, switch(lang,
      de = " \u2014 geringstes \u00dcberlaufvolumen je Kategorie",
      en = " \u2014 lowest overflow volume per class"))
    txt$title_max_evap <- paste0(evap_prefix, switch(lang,
      de = " \u2014 h\u00f6chste Evapotranspiration je Kategorie",
      en = " \u2014 highest evapotranspiration per class"))
  }

  def_title <- switch(best_by,
    min_cost             = txt$title_cheapest,
    min_overflow         = txt$title_min_overflow,
    max_evapotranspiration = txt$title_max_evap)
  def_size  <- if (size_by == "evapotranspiration") txt$size_evap else txt$size_volume
  best_lab  <- switch(best_by,
    min_cost             = txt$best_cheapest,
    min_overflow         = txt$best_min_overflow,
    max_evapotranspiration = txt$best_max_evap)

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

  # Reference for the cost-per-percent-evapotranspiration measure: the
  # minimum evapotranspiration among the scenarios that SATISFY the validity
  # criterion (n_overflows <= x); computed before any filtering, falls back
  # to the complete run when no scenario is valid. The matching scenario id
  # is named in the title.
  evap_all <- simulation_results_optimisation[["element.WB_Evapotranspiration_"]]
  valid_mask <- !is.na(simulation_results_optimisation$n_overflows) &
    simulation_results_optimisation$n_overflows <= x_int & !is.na(evap_all)
  ref_idx <- if (any(valid_mask)) which(valid_mask) else seq_along(evap_all)
  evap_min <- suppressWarnings(min(evap_all[ref_idx], na.rm = TRUE))
  evap_min_scenario <- simulation_results_optimisation$scenario_name[
    ref_idx[which.min(evap_all[ref_idx])]]

  # Share of scenarios meeting the validity criterion (n_overflows <= x),
  # appended to the auto-generated title (a plotly-safe place -- ggplotly
  # drops ggplot subtitles).
  valid_pct <- round(100 * mean(
    simulation_results_optimisation$n_overflows <= x_int, na.rm = TRUE))
  share_txt <- switch(lang,
    de = paste0(valid_pct, " % mit <= ", x_int, " \u00dcberl\u00e4ufen"),
    en = paste0(valid_pct, " % with <= ", x_int, " overflows"))
  if (is.null(title)) {
    title <- paste0(def_title, " (", share_txt, ")")
    if (y_var == "cost_per_evap_pct") {
      # name the reference of the marginal measure in the title
      title <- paste0(title, "\n", switch(lang,
        de = paste0("Referenz: minimale Evapotranspiration der g\u00fcltigen ",
                    "Szenarien (<= ", x_int, " \u00dcberl\u00e4ufe): ",
                    round(evap_min, 1), " % (Szenario ",
                    evap_min_scenario, ")"),
        en = paste0("Reference: minimum evapotranspiration of the valid ",
                    "scenarios (<= ", x_int, " overflows): ",
                    round(evap_min, 1), " % (scenario ",
                    evap_min_scenario, ")")))
    }
  }
  if (is.null(caption)) caption <- cost_rates_caption(lang)

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

  # Cost per percentage point of evapotranspiration ABOVE the run minimum
  # [EUR/%]: the baseline evapotranspiration comes "for free", only the gain
  # beyond the worst scenario is paid for. Scenarios at the minimum (incl.
  # the reference scenario itself) have no defined marginal cost and are
  # dropped from the cost_per_evap_pct variant (the active y column must not
  # be NA).
  df <- df %>%
    dplyr::mutate(
      cost_per_evap_pct = dplyr::if_else(
        .data[["element.WB_Evapotranspiration_"]] - evap_min > 0,
        .data$cost_total /
          (.data[["element.WB_Evapotranspiration_"]] - evap_min),
        NA_real_
      )
    ) %>%
    dplyr::filter(!is.na(.data[[y_col]]))

  # Storage type: display factor for the facet strips and the point shapes
  # (filled square = infiltration box, filled triangle = gravel trench).
  st <- storage_type_shapes(df$storage_type, lang)
  df$storage_type_disp <- st$display

  df$tooltip_html <- cost_tooltip_text(df, txt, digits, evap_min = evap_min)

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
    dplyr::filter(!is.na(.data$overflow_cat), !is.na(.data[[y_col]]))
  # With storage-type facets every panel gets its own best-per-box marker and
  # frontier line, so the two technologies stay comparable.
  best_grp <- if (isTRUE(facet_storage_type)) {
    best_grp %>% dplyr::group_by(.data$storage_type_disp, .data$overflow_cat)
  } else {
    best_grp %>% dplyr::group_by(.data$overflow_cat)
  }
  best <- switch(best_by,
    max_evapotranspiration = best_grp %>%
      dplyr::arrange(.data$n_overflows,
                     dplyr::desc(.data[["element.WB_Evapotranspiration_"]]),
                     .data[[y_col]], .data$scenario_name, .by_group = TRUE),
    min_overflow = best_grp %>%
      dplyr::arrange(.data$n_overflows, .data$overflow_volume_m3,
                     .data[[y_col]], .data$scenario_name, .by_group = TRUE),
    min_cost = best_grp %>%
      dplyr::arrange(.data$n_overflows, .data[[y_col]],
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
        format(round(best[[y_col]], 0), big.mark = " ", trim = TRUE),
        if (y_var == "cost_per_evap_pct") {
          # ... and the evapotranspiration gain over the reference minimum
          # that this price buys
          paste0(" \u20ac/% (+",
                 format(round(best[["element.WB_Evapotranspiration_"]] -
                                evap_min, 1), trim = TRUE),
                 " % ", switch(lang, de = "Evapotranspiration",
                               en = "evapotranspiration"), ")")
        } else {
          " \u20ac"
        })
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

  # Storage-type shapes only when both types share one panel: in the faceted
  # layout the strips already name the type, so the points stay plain circles
  # (better readable with the size scaling).
  use_shapes <- !isTRUE(facet_storage_type)
  jitter_mapping <- ggplot2::aes(size = .data$size_plot,
                                 colour = .data$overflow_cat,
                                 text = .data$tooltip_html)
  if (use_shapes) {
    jitter_mapping <- utils::modifyList(
      jitter_mapping,
      ggplot2::aes(shape = .data$storage_type_disp)
    )
  }
  # With shapes in use, the size-legend keys would default to circles, which
  # then never occur in the plot; draw them with the storage-type marker
  # instead (neutral grey) -- the single present shape, or the square when
  # both types are shown.
  present_types <- unique(as.character(df$storage_type_disp))
  size_key_shape <- if (length(present_types) == 1L) {
    unname(st$shape_values[present_types])
  } else {
    15
  }

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$overflow_cat,
                                        y = .data[[y_col]])) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data$overflow_cat),
      alpha = box_alpha, outlier.shape = NA, colour = "grey40"
    ) +
    ggplot2::geom_jitter(
      jitter_mapping,
      position = pos, alpha = point_alpha
    ) +
    ggplot2::scale_size(range = c(1.5, max_point_size), name = lab_size) +
    ggplot2::scale_fill_manual(values = pal, limits = levs, drop = FALSE,
                               guide = "none") +
    ggplot2::scale_color_manual(values = pal, limits = levs, drop = FALSE,
                                guide = "none") +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::labs(title = title, x = lab_x, y = lab_y,
                  caption = if (nzchar(caption)) caption else NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = legend_position,
      plot.title = ggplot2::element_text(size = 11)
    )

  # Square = infiltration box, triangle = gravel trench -- only when both
  # types share one panel. The size-legend keys are then drawn with the same
  # marker, so no circle appears in the legend that is absent from the plot.
  if (use_shapes) {
    p <- p +
      ggplot2::scale_shape_manual(
        values = st$shape_values, drop = FALSE,
        name = txt$tt_storage_type
      ) +
      ggplot2::guides(
        size = ggplot2::guide_legend(
          override.aes = list(shape = size_key_shape, colour = "grey30",
                              alpha = 1)
        )
      )
  }

  # Two stacked panels (infiltration box on top, gravel trench below).
  # plotly::ggplotly() converts the facets to stacked subplots, so the
  # interactive HTML keeps the panel split.
  if (isTRUE(facet_storage_type)) {
    p <- p + ggplot2::facet_grid(
      rows = ggplot2::vars(.data$storage_type_disp)
    )
  }

  # Frontier line across the best of every box (all classes; per panel when
  # faceting -- facet_grid subsets `best` by storage type), then the
  # group-coloured best-marker on top of everything.
  if (isTRUE(connect_best) && nrow(best) > 1L) {
    p <- p + ggplot2::geom_line(
      data = best,
      ggplot2::aes(x = .data$overflow_cat, y = .data[[y_col]], group = 1L),
      colour = "black", linewidth = 0.7, na.rm = TRUE
    )
  }
  if (isTRUE(mark_best) && nrow(best) > 0L) {
    p <- p + ggplot2::geom_point(
      data = best,
      ggplot2::aes(x = .data$overflow_cat, y = .data[[y_col]],
                   fill = .data$overflow_cat,
                   text = .data$tooltip_best),
      shape = 23, size = 3.2, colour = "black", stroke = 1.2, na.rm = TRUE
    )
  }
  if (isTRUE(label_best) && "label_text" %in% names(best) && nrow(best) > 0L) {
    # place the label just above the marker (centred), so long labels such as
    # "3515 m3 / 35 %" never run off the right edge of the last box.
    lab_nudge_y <- 0.045 * diff(range(df[[y_col]], na.rm = TRUE))
    p <- p + ggplot2::geom_text(
      data = best,
      ggplot2::aes(x = .data$overflow_cat, y = .data[[y_col]],
                   label = .data$label_text),
      position = ggplot2::position_nudge(y = lab_nudge_y),
      hjust = 0.5, vjust = 0, size = 2.8, colour = "black", na.rm = TRUE
    )
  }

  p
}
