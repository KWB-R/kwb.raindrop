#' Find scenarios that differ from a reference in exactly one parameter
#'
#' @description
#' Identifies rows that differ from a reference in *exactly one* parameter.
#' You may pass `param_cols` explicitly, or let the function infer them by
#' excluding `id_col`, `exclude_cols`, and any columns matching
#' `exclude_cols_regex` (default excludes result columns like `h_*`).
#'
#' @param data A data frame with scenarios.
#' @param ref_scenario Reference scenario ID (default `"s00001"`).
#' @param id_col Name of the scenario ID column (default `"scenario_name"`).
#' @param param_cols Optional character vector of parameter columns. If `NULL`,
#'   parameters are inferred.
#' @param exclude_cols Character vector to exclude from parameter detection.
#' @param exclude_cols_regex Regex to auto-exclude non-parameter columns (default `"^h_"`).
#' @param tol Numeric tolerance for numeric comparisons (default `1e-9`).
#' @param quiet Logical; print diagnostics if `FALSE` (default).
#' @param include_reference `"per_param"` (default) to include one reference row
#'   per parameter with hits, or `"none"`.
#'
#' @return A data frame with columns: `id_col`, `param_name`, `param_value`,
#'   `is_reference`. Attributes: `"param_cols"`, `"ref_row"`, `"diff_mat"`, `"n_diff"`.
#'
#' @export
find_single_param_variations <- function(
    data,
    ref_scenario = "s00001",
    id_col = "scenario_name",
    param_cols = NULL,
    exclude_cols = NULL,
    exclude_cols_regex = "^h_",
    tol = 1e-9,
    quiet = FALSE,
    include_reference = c("per_param", "none")
) {
  include_reference <- match.arg(include_reference)
  stopifnot(id_col %in% names(data))
  
  # Detect parameter columns
  if (is.null(param_cols)) {
    drop_cols <- c(id_col, exclude_cols)
    if (!is.null(exclude_cols_regex) && nzchar(exclude_cols_regex)) {
      drop_cols <- unique(c(drop_cols, grep(exclude_cols_regex, names(data), value = TRUE)))
    }
    param_cols <- setdiff(names(data), drop_cols)
  }
  if (length(param_cols) == 0) {
    stop("No parameter columns detected. Pass `param_cols` explicitly or adjust `exclude_cols(_regex)`.")
  }
  
  # Reference row
  if (!ref_scenario %in% data[[id_col]]) {
    warning(sprintf("Reference '%s' not found in filtered data. Using first row as reference.", ref_scenario))
    ref_scenario <- data[[id_col]][1]
  }
  ref_row_df <- data[data[[id_col]] == ref_scenario, , drop = FALSE]
  if (nrow(ref_row_df) != 1) stop("Reference scenario must be unique.")
  
  # Difference matrix
  is_diff_col <- function(vec, ref_val) if (is.numeric(vec)) abs(vec - as.numeric(ref_val)) > tol else as.character(vec) != as.character(ref_val)
  diff_mat <- sapply(param_cols, function(p) is_diff_col(data[[p]], ref_row_df[[p]]))
  if (is.null(dim(diff_mat))) diff_mat <- matrix(diff_mat, ncol = 1, dimnames = list(NULL, param_cols))
  n_diff <- rowSums(diff_mat, na.rm = TRUE)
  rows_single <- (n_diff == 1) & (data[[id_col]] != ref_scenario)
  
  if (!quiet) {
    message(sprintf("Rows with exactly one differing parameter: %d of %d", sum(rows_single), nrow(data)))
    if (any(rows_single)) {
      counts <- colSums(diff_mat & rows_single)
      message("Single-parameter variations per parameter: ",
              paste(sprintf("%s=%d", names(counts), counts), collapse = ", "))
    }
  }
  
  # Assemble hits
  out_list <- lapply(param_cols, function(p) {
    idx <- rows_single & diff_mat[, p]
    if (!any(idx)) return(NULL)
    df <- data[idx, c(id_col, p), drop = FALSE]
    names(df)[names(df) == p] <- "param_value"
    df$param_name   <- p
    df$is_reference <- FALSE
    df[, c(id_col, "param_name", "param_value", "is_reference")]
  })
  out_list <- Filter(Negate(is.null), out_list)
  
  if (length(out_list) == 0) {
    if (include_reference == "per_param") {
      ref_rows <- data.frame(
        param_name   = param_cols,
        param_value  = as.numeric(ref_row_df[1, param_cols]),
        is_reference = TRUE,
        stringsAsFactors = FALSE
      )
      ref_rows[[id_col]] <- ref_scenario
      ref_rows <- ref_rows[, c(id_col, "param_name", "param_value", "is_reference")]
      return(structure(ref_rows,
                       param_cols = param_cols, ref_row = ref_row_df,
                       diff_mat = diff_mat, n_diff = n_diff))
    }
    return(structure(data.frame(),
                     param_cols = param_cols, ref_row = ref_row_df,
                     diff_mat = diff_mat, n_diff = n_diff))
  }
  
  hits <- do.call(rbind, out_list)
  
  if (include_reference == "per_param") {
    params_with_hits <- unique(hits$param_name)
    ref_points <- data.frame(
      param_name   = params_with_hits,
      param_value  = as.numeric(ref_row_df[1, params_with_hits]),
      is_reference = TRUE,
      stringsAsFactors = FALSE
    )
    ref_points[[id_col]] <- ref_scenario
    ref_points <- ref_points[, c(id_col, "param_name", "param_value", "is_reference")]
    hits <- rbind(hits, ref_points)
  }
  
  structure(hits,
            param_cols = param_cols,
            ref_row = ref_row_df,
            diff_mat = diff_mat,
            n_diff = n_diff)
}
