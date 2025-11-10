#' Plot the influence of single-parameter variations on pond height results
#'
#' @description
#' Compares simulated scenarios against a reference scenario (default: `"s00001"`)
#' and plots the deviation of either `h_pond_max` or `h_pond_mean` for cases in
#' which *exactly one* input parameter differs from the reference.
#'
#' The plot title automatically includes the reference scenario's
#' corresponding pond height value with unit `"mm"`,
#' e.g. `"Ref: s00001 (h_pond_max = 577.5 mm)"`.
#'
#' @param data A data frame containing the simulation results. Must include
#'   columns `scenario_name`, `h_pond_max`, `h_pond_mean`, and one or more
#'   parameter columns (e.g. `mulde_area`, `mulde_height`,
#'   `filter_hyraulicconductivity`, `filter_height`, `storage_height`).
#' @param response Character string; the response variable to plot.
#'   One of `"h_pond_max"` or `"h_pond_mean"`.
#' @param ref_scenario Character; the scenario ID used as reference
#'   (default: `"s00001"`).
#' @param diff Character; type of difference to plot:
#'   `"abs"` for absolute difference (`value - ref`) or
#'   `"pct"` for percentage difference (`(value - ref)/ref * 100`).
#' @param tol Numeric; tolerance for numerical comparisons when detecting
#'   deviations from the reference (default: `1e-9`).
#' @param quiet Logical; if `FALSE` (default), diagnostic messages about the
#'   number of detected single-parameter variations are printed.
#'
#' @details
#' The function automatically identifies parameter columns as all columns
#' not named `scenario_name`, `h_pond_max`, or `h_pond_mean`.
#' It then selects scenarios where *exactly one* parameter differs from the
#' reference scenario within the given numerical tolerance.
#' Only those scenarios are included in the resulting plot.
#'
#' The output is a `ggplot2` object with one facet per parameter showing the
#' deviation (`Δ`) of the selected `response` variable from the reference.
#'
#' @return A `ggplot` object visualizing the effect of single-parameter
#'   variations on the selected response variable.
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_point geom_line facet_wrap
#'   scale_shape_manual labs theme_bw
#' @importFrom dplyr %>% filter mutate
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   scenario_name = sprintf("s%05d", 1:22),
#'   h_pond_max = 577.5244,
#'   h_pond_mean = 429.5059,
#'   mulde_area = c(1,10,50,100,500,1000,
#'                  1,10,50,100,500,1000,
#'                  1,10,50,100,500,1000,
#'                  1,10,50,100),
#'   mulde_height = rep(c(100,200,300,400), each = 6)[1:22],
#'   filter_hyraulicconductivity = 1,
#'   filter_height = 150,
#'   storage_height = 150
#' )
#'
#' # Plot absolute deviations of h_pond_max
#' plot_hpond_vs_ref(df, response = "h_pond_max", ref_scenario = "s00001")
#'
#' # Plot percentage deviations of h_pond_mean
#' plot_hpond_vs_ref(df, response = "h_pond_mean", diff = "pct")
#' }
#'
#' @export
plot_hpond_vs_ref <- function(data,
                              response = c("h_pond_max", "h_pond_mean"),
                              ref_scenario = "s00001",
                              diff = c("abs", "pct"),
                              tol = 1e-9,
                              quiet = FALSE) {
  
  response <- match.arg(response)
  diff     <- match.arg(diff)
  
  needed_cols <- c("scenario_name", "h_pond_max", "h_pond_mean")
  stopifnot(all(needed_cols %in% names(data)))
  
  # Detect parameter columns automatically
  param_cols <- setdiff(names(data), needed_cols)
  
  # Get reference row
  if (!ref_scenario %in% data$scenario_name) {
    warning(sprintf("Reference '%s' not found. Using first row as reference.", ref_scenario))
    ref_scenario <- data$scenario_name[1]
  }
  ref_row <- data[data$scenario_name == ref_scenario, , drop = FALSE]
  stopifnot(nrow(ref_row) == 1)
  
  # helper: compare numeric/char values to reference with tolerance
  is_diff_col <- function(vec, ref_val) {
    if (is.numeric(vec)) {
      abs(vec - as.numeric(ref_val)) > tol
    } else {
      as.character(vec) != as.character(ref_val)
    }
  }
  
  # Matrix of differences (TRUE if deviates from reference)
  diff_mat <- sapply(param_cols, function(p) is_diff_col(data[[p]], ref_row[[p]]))
  n_diff <- rowSums(diff_mat, na.rm = TRUE)
  
  # Diagnostic summary
  if (!quiet) {
    message(sprintf("Rows with exactly one differing parameter: %d of %d",
                    sum(n_diff == 1), nrow(data)))
    counts <- colSums(diff_mat & (n_diff == 1))
    message("Single-parameter variations detected per parameter: ",
            paste(sprintf("%s=%d", names(counts), counts), collapse = ", "))
  }
  
  # Build sub-data for each parameter
  build_one <- function(p) {
    idx <- (n_diff == 1) & diff_mat[, p]
    if (!any(idx)) return(NULL)
    df <- data[idx, c("scenario_name", response, p), drop = FALSE]
    ref_val_y <- ref_row[[response]]
    df$param_name  <- p
    df$param_value <- df[[p]]
    df$y           <- df[[response]]
    df$delta_abs   <- df$y - ref_val_y
    df$delta_pct   <- 100 * (df$y - ref_val_y) / ref_val_y
    df[, c("scenario_name", "param_name", "param_value", "y", "delta_abs", "delta_pct")]
  }
  
  out_list <- lapply(param_cols, build_one)
  out_list <- Filter(Negate(is.null), out_list)
  if (length(out_list) == 0) {
    stop("No single-parameter variations found relative to the reference scenario.")
  }
  
  df_long <- do.call(rbind, out_list)
  params_with_data <- unique(df_long$param_name)
  
  # Add reference point for each facet
  ref_points <- data.frame(
    scenario_name = ref_scenario,
    param_name    = params_with_data,
    param_value   = as.numeric(ref_row[1, params_with_data]),
    y             = ref_row[[response]],
    delta_abs     = 0,
    delta_pct     = 0
  )
  df_plot <- rbind(df_long, ref_points)
  
  # y-axis selection
  y_col <- if (diff == "abs") "delta_abs" else "delta_pct"
  y_lab <- if (diff == "abs") sprintf("%s − Reference", response) else sprintf("Δ %s vs Ref. [%%]", response)
  
  # Title includes reference value and unit (mm)
  ref_val <- ref_row[[response]]
  title_text <- sprintf(
    "Influence of single-parameter variations on %s (Ref: %s, %s = %.3f mm)",
    response, ref_scenario, response, ref_val
  )
  
  # Plot
  suppressPackageStartupMessages({
    library(ggplot2)
    library(dplyr)
  })
  
  ggplot2::ggplot(df_plot, ggplot2::aes(x = param_value, y = .data[[y_col]])) +
    ggplot2::geom_hline(yintercept = 0, linetype = 2, linewidth = 0.4) +
    ggplot2::geom_point(aes(shape = scenario_name == ref_scenario), size = 2, alpha = 0.9) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::facet_wrap(~ param_name, scales = "free_x", ncol = 2) +
    ggplot2::scale_shape_manual(values = c(`TRUE` = 17, `FALSE` = 19), guide = "none") +
    ggplot2::labs(
      title = title_text,
      x = "Parameter value",
      y = y_lab,
      caption = "Only scenarios with exactly one differing parameter are included."
    ) +
    ggplot2::theme_bw(base_size = 12)
}
