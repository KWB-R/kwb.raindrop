#' Plot the influence of single-parameter variations on a response
#'
#' @description
#' Uses \code{find_single_param_variations()} (by default excluding `^h_` columns
#' from parameter detection) and plots absolute or percentage deviations of the
#' chosen \code{response}. The title shows the reference value in mm.
#'
#' @param data Data frame with at least `scenario_name`, parameter columns, and `response`.
#' @param response Character; the response column to plot (e.g. `"h_pond_max"` or `"h_pond_mean"`).
#' @param ref_scenario Reference scenario ID (default `"s00001"`).
#' @param diff `"abs"` for absolute or `"pct"` for percentage deviation.
#' @param param_cols Optional explicit parameter columns (recommended if you prefilter).
#' @param exclude_cols Extra columns to exclude from parameter detection.
#' @param exclude_cols_regex Regex to auto-exclude non-parameter columns (default `"^h_"`).
#' @param tol Numeric tolerance passed through to \code{find_single_param_variations()}.
#' @param quiet Logical; diagnostics if `FALSE`.
#'
#' @return A `ggplot` object.
#' @export
plot_hpond_vs_ref <- function(
    data,
    response,
    ref_scenario = "s00001",
    diff = c("abs", "pct"),
    param_cols = NULL,
    exclude_cols = NULL,
    exclude_cols_regex = "^h_",
    tol = 1e-9,
    quiet = FALSE
) {
  diff <- match.arg(diff)
  if (!("scenario_name" %in% names(data))) stop("Column `scenario_name` not found in `data`.")
  if (!(response %in% names(data))) stop(sprintf("Response column `%s` not found in `data`.", response))
  
  # 1) Detect hits (incl. reference rows for facets)
  hits_raw <- find_single_param_variations(
    data = data,
    ref_scenario = ref_scenario,
    id_col = "scenario_name",
    param_cols = param_cols,
    exclude_cols = exclude_cols,
    exclude_cols_regex = exclude_cols_regex,
    tol = tol,
    quiet = quiet,
    include_reference = "per_param"
  )
  if (nrow(hits_raw) == 0) stop("No single-parameter variations found.")
  
  # 2) Get reference row robustly (attributes might be dropped in some pipelines)
  ref_row <- attr(hits_raw, "ref_row")
  if (is.null(ref_row)) {
    # Fallback: reconstruct from `data`
    if (!ref_scenario %in% data$scenario_name) {
      warning(sprintf("Reference '%s' not in filtered `data`. Using first row as reference.", ref_scenario))
      ref_scenario <- data$scenario_name[1]
    }
    ref_row <- data[data$scenario_name == ref_scenario, , drop = FALSE]
  }
  if (nrow(ref_row) != 1) stop("Reference scenario must be unique and present in `data`.")
  if (!(response %in% names(ref_row))) {
    stop(sprintf("`response` ('%s') not present in the reference row. Was it filtered out?", response))
  }
  ref_val_y <- ref_row[[response]][1]
  
  # 3) Merge response values to hits
  hits <- merge(hits_raw, data[, c("scenario_name", response)], by = "scenario_name", all.x = TRUE)
  if (!(response %in% names(hits))) {
    stop("Internal error: response could not be joined into hits. Check `scenario_name` and `response`.")
  }
  
  # 4) Deltas
  hits$delta_abs <- hits[[response]] - ref_val_y
  hits$delta_pct <- 100 * (hits[[response]] - ref_val_y) / ref_val_y
  
  # 5) Plot
  y_col <- if (diff == "abs") "delta_abs" else "delta_pct"
  y_lab <- if (diff == "abs") sprintf("%s \u2212 Reference", response) else sprintf("\u0394 %s vs Ref. [%%]", response)
  title_text <- sprintf(
    "Influence of single-parameter variations on %s (Ref: %s, %s = %.3f mm)",
    response, ref_scenario, response, ref_val_y
  )
  
  ggplot2::ggplot(hits, ggplot2::aes(x = param_value, y = .data[[y_col]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(shape = is_reference), size = 2, alpha = 0.9) +
    ggplot2::geom_line(data = subset(hits, !is_reference), alpha = 0.6) +
    ggplot2::facet_wrap(~ param_name, scales = "free_x", ncol = 2) +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 19), guide = "none") +
    ggplot2::labs(
      title = title_text,
      x = "Parameter value",
      y = y_lab,
      caption = "Only scenarios with exactly one differing parameter are included; ▲ marks the reference."
    ) +
    ggplot2::theme_bw(base_size = 12)
}
