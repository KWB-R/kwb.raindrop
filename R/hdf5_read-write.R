#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom hdf5r H5File
NULL

# ====================== Helpers ======================

# Robust writer: try multiple possible argument names across hdf5r variants
.dwrite <- function(dset, x) {
  tries <- list(
    list(value = x),
    list(obj   = x),
    list(data  = x),
    list(buf   = x),
    list(object= x),
    list(x)               # positional as very last resort
  )
  for (args in tries) {
    ok <- try(do.call(dset$write, args), silent = TRUE)
    if (!inherits(ok, "try-error")) return(invisible(TRUE))
  }
  stop("Failed to write dataset: none of the write() signatures (value/obj/data/buf/object/positional) worked.")
}

# Coerce value roughly to dataset's existing R type (from a small probe read)
.coerce_like_dataset <- function(val, dset) {
  got <- try(dset$read(), silent = TRUE)
  if (!inherits(got, "try-error")) {
    if (is.character(got)) return(as.character(val))
    if (is.integer(got))   return(as.integer(val))
    if (is.numeric(got))   return(as.numeric(val))
    if (is.logical(got))   return(as.logical(val))
  }
  val
}

# ====================== 1) List ======================

#' List all datasets (recursive)
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @return A tibble with columns: \code{path}, \code{obj_type}, \code{dims}, \code{maxdims}.
#' @examples
#' \dontrun{
#' h5 <- hdf5r::H5File$new("file.h5", mode = "r")
#' list_h5_datasets(h5)
#' }
#' @importFrom tibble tibble
#' @export
list_h5_datasets <- function(h5) {
  stopifnot(inherits(h5, "H5File"))
  ls_tbl <- h5$ls(recursive = TRUE)
  ds <- ls_tbl[ls_tbl$obj_type == "H5I_DATASET", , drop = FALSE]
  ds$path <- file.path("/", ds$name)
  tibble::tibble(
    path     = ds$path,
    obj_type = ds$obj_type,
    dims     = ifelse(is.na(ds$dataset.dims), NA_character_, ds$dataset.dims),
    maxdims  = ifelse(is.na(ds$dataset.maxdims), NA_character_, ds$dataset.maxdims)
  )
}

# ====================== 2) Read ======================

#' Read values of all (or selected) datasets
#' @param h5 An open `hdf5r::H5File`
#' @param paths character vector; if NULL read all
#' @param simplify_scalars logical: simplify H5S_SCALAR to length-1 atom
#' @param timeseries_as_tibble logical: convert 2xN / Nx2 to tibble(time,value)
#' @param ts_names character(2): names for time/value columns
#' @return named list
#' @export
h5_read_values <- function(h5,
                           paths = NULL,
                           simplify_scalars = TRUE,
                           timeseries_as_tibble = TRUE,
                           ts_names = c("time","value")) {
  stopifnot(inherits(h5, "H5File"))
  if (is.null(paths)) {
    paths <- list_h5_datasets(h5)$path
  } else {
    stopifnot(is.character(paths), all(startsWith(paths, "/")))
  }
  out <- stats::setNames(vector("list", length(paths)), paths)
  
  .as_ts_tibble <- function(x, names = c("time","value")) {
    d <- dim(x); stopifnot(length(d) == 2L)
    if (d[1] == 2L) {
      tibble::tibble(!!names[1] := as.vector(x[1, ]),
                     !!names[2] := as.vector(x[2, ]))
    } else if (d[2] == 2L) {
      tibble::tibble(!!names[1] := as.vector(x[, 1]),
                     !!names[2] := as.vector(x[, 2]))
    } else stop("Not 2xN or Nx2.")
  }
  
  for (p in paths) {
    dset <- h5[[p]]
    if (!inherits(dset, "H5D")) stop(sprintf("Not a dataset: %s", p))
    val <- dset$read()
    
    if (simplify_scalars && length(val) == 1L && !is.array(val)) {
      if (is.list(val) && length(val) == 1L) val <- val[[1L]]
      out[[p]] <- val
      next
    }
    if (timeseries_as_tibble && is.array(val) && length(dim(val)) == 2L) {
      d <- dim(val)
      if (2L %in% d) { out[[p]] <- .as_ts_tibble(val, ts_names); next }
    }
    out[[p]] <- val
  }
  out
}


# --- helpers ---

#' @keywords internal
.parse_dims <- function(s) {
  if (is.na(s) || !nzchar(s)) return(integer(0))
  as.integer(strsplit(gsub("\\s", "", s), "x", fixed = TRUE)[[1]])
}

#' @keywords internal
.err_text <- function(x) if (inherits(x, "try-error")) as.character(x) else paste0(x)

#' @keywords internal
#' @importFrom utils tail
.dwrite_diag <- function(dset, x) {
  tries <- list(
    list(args = list(), value  = x),
    list(args = list(), obj    = x),
    list(args = NULL,  value   = x),
    list(args = NULL,  obj     = x),
    list(value = x),
    list(obj    = x),
    list(x)
  )
  errs <- character(0)
  for (a in tries) {
    res <- try(do.call(dset$write, a), silent = TRUE)
    if (!inherits(res, "try-error")) return(invisible(TRUE))
    errs <- c(errs, .err_text(res))
  }
  stop(paste0("write() failed. First: ", errs[1], if (length(errs)>1) paste0("\nLast: ", utils::tail(errs,1)) else ""), call. = FALSE)
}

#' @keywords internal
.dataset_target_kind <- function(dset) {
  got <- try(dset$read(), silent = TRUE)
  if (inherits(got, "try-error")) return("unknown")
  if (is.character(got)) "character" else if (is.integer(got)) "integer"
  else if (is.double(got) || is.numeric(got)) "double"
  else if (is.logical(got)) "logical" else "unknown"
}

#' @keywords internal
.normalize_for_dataset <- function(dset, x) {
  k <- .dataset_target_kind(dset)
  if (k == "character") return(enc2utf8(as.character(if (is.factor(x)) as.character(x) else x)))
  if (k == "integer")   return(as.integer(x))
  if (k == "double")    return(as.numeric(x))
  if (k == "logical")   return(as.logical(x))
  x
}

#' @keywords internal
.shape_for_dataset <- function(x, target_dims, dset) {
  x <- .normalize_for_dataset(dset, x)
  if (length(target_dims) == 0L) { # SCALAR
    if (is.list(x) && length(x) == 1L) x <- x[[1L]]
    if (length(x) != 1L) stop(sprintf("SCALAR expects length 1, got %d.", length(x)), call. = FALSE)
    return(x)
  }
  if (length(target_dims) == 1L) { # 1D
    if (is.null(dim(x))) {
      if (length(x) != target_dims[1]) stop(sprintf("Length mismatch: %d vs. %d", length(x), target_dims[1]), call. = FALSE)
      return(as.vector(x))
    } else {
      if (prod(dim(x)) != target_dims[1]) stop(sprintf("Length mismatch: %d vs. %d", prod(dim(x)), target_dims[1]), call. = FALSE)
      return(as.vector(x))
    }
  }
  # 2D+
  if (is.null(dim(x))) {
    if (length(x) != prod(target_dims)) stop(sprintf("Size mismatch: %d vs. %d", length(x), prod(target_dims)), call. = FALSE)
    return(array(x, dim = target_dims))
  } else {
    if (!identical(as.integer(dim(x)), as.integer(target_dims))) {
      if (prod(dim(x)) != prod(target_dims)) {
        stop(sprintf("Dim/product mismatch: dim(x)=%s vs. target=%s",
                     paste(dim(x), collapse="x"), paste(target_dims, collapse="x")), call. = FALSE)
      }
      return(array(as.vector(x), dim = target_dims))
    }
    return(x)
  }
}

#' @keywords internal
.infer_dims_from_value <- function(v, ts_cols = c("time","value")) {
  if (is.data.frame(v) && ncol(v) == 2L) {
    n <- max(NROW(v[[ ts_cols[1] ]]), NROW(v[[ ts_cols[2] ]]))
    return(c(2L, as.integer(n)))          # 2 × N
  }
  if (!is.null(dim(v))) return(as.integer(dim(v)))
  as.integer(length(v))                    # 1D Länge
}


# ====================== 3) Read ======================

#' Write (updated) values back into existing HDF5 datasets (robust)
#'
#' Writes scalars, vectors, matrices/arrays, and 2-column data frames/tibbles (treated
#' as time series) into existing HDF5 datasets. If the dataset reports SCALAR incorrectly,
#' the function can infer target dimensions from the supplied value and resize accordingly.
#'
#' @param h5 An open \code{hdf5r::H5File} (e.g., \code{mode = "r+"}).
#' @param values Named \code{list}: names are absolute dataset paths, values are R objects to write.
#' @param resize Logical. If \code{TRUE}, resize datasets via \code{set_extent()} when shapes differ.
#' @param strict Logical. If \code{TRUE}, stop on first error; otherwise warn and skip.
#' @param prefer_rows Logical(1) or \code{NA}. For 2-column time series:
#'   \code{NA} keeps dataset orientation (2xN if first dim == 2), \code{TRUE} forces 2xN,
#'   \code{FALSE} forces Nx2.
#' @param ts_cols Character(2). Column names to pull from time-series data frames (default \code{c("time","value")}).
#' @param scalar_strategy One of \code{"error"}, \code{"first"}, \code{"collapse"}. Controls how non-length-1 values
#'   are handled for true SCALAR datasets.
#' @param collapse_sep Character. Separator used when \code{scalar_strategy = "collapse"}.
#' @param verbose Logical. If \code{TRUE}, prints per-path dimension info.
#'
#' @return Invisibly returns the character vector of written dataset paths.
#' @examples
#' \dontrun{
#' vals <- h5_read_values(h5)
#' vals[["/Parameters/OutputPath"]] <- "C:/temp/out.h5"
#' h5_write_values(h5, vals, resize = TRUE, scalar_strategy = "first", verbose = TRUE)
#' }
#' @importFrom stats setNames
#' @export
h5_write_values <- function(h5, values,
                            resize = TRUE,
                            strict = TRUE,
                            prefer_rows = NA,
                            ts_cols = c("time","value"),
                            scalar_strategy = c("error","first","collapse"),
                            collapse_sep = ";",
                            verbose = FALSE) {
  scalar_strategy <- match.arg(scalar_strategy)
  stopifnot(inherits(h5, "H5File"))
  stopifnot(is.list(values), !is.null(names(values)), all(nzchar(names(values))))
  
  # Pfade normalisieren
  names(values) <- sub("^//+", "/", names(values))
  
  # ls()-Dims als Fallback
  lst <- list_h5_datasets(h5)
  dims_hint <- setNames(lapply(lst$dims, .parse_dims), lst$path)
  
  written <- character(0)
  bail <- function(msg) if (strict) stop(msg, call. = FALSE) else { warning(msg, call. = FALSE); NULL }
  
  for (p in names(values)) {
    v <- values[[p]]
    dset <- try(h5[[p]], silent = TRUE)
    if (inherits(dset, "try-error") || !inherits(dset, "H5D")) { bail(sprintf("No dataset: %s", p)); next }
    
    ext <- dset$get_space()$get_simple_extent_dims()
    cur_size <- as.numeric(ext$size)
    inferred_from_value <- FALSE
    
    # Fallback 1: ls() Dims
    if (length(cur_size) == 0L) {
      hint <- dims_hint[[p]]
      if (length(hint)) cur_size <- hint
    }
    # Fallback 2: aus dem Wert inferieren (Fix für „falsch-SCALAR“)
    if (length(cur_size) == 0L) {
      cur_size <- .infer_dims_from_value(v, ts_cols)
      inferred_from_value <- length(cur_size) > 0L
    }
    
    max_size <- suppressWarnings(as.numeric(ext$max_size))
    if (verbose) cat(sprintf("-> %s | dims=%s%s\n", p,
                             if (length(cur_size)) paste(cur_size, collapse="x") else "SCALAR",
                             if (isTRUE(inferred_from_value)) " (inferred)" else ""))
    
    # --- 2-spaltige Timeseries ---
    if (is.data.frame(v) && ncol(v) == 2L) {
      time  <- if (all(ts_cols %in% names(v))) v[[ ts_cols[1] ]] else v[[1]]
      value <- if (all(ts_cols %in% names(v))) v[[ ts_cols[2] ]] else v[[2]]
      n <- length(time); if (length(value) != n) { bail(sprintf("%s: `time` and `value` length differ.", p)); next }
      
      # Orientierung (Default: 2xN, außer explizit oder vorhandene 1. Dim != 2)
      if (isTRUE(prefer_rows) || (is.na(prefer_rows) && length(cur_size) == 2L && cur_size[1] == 2L)) {
        new_size <- c(2L, n); mat <- rbind(time, value)
      } else {
        new_size <- c(n, 2L); mat <- cbind(time, value)
      }
      
      if (length(cur_size) == 0L || !all(cur_size == new_size)) {
        if (!resize) { bail(sprintf("%s: dims %s -> %s. Set resize=TRUE.",
                                    p, if (length(cur_size)) paste(cur_size, collapse="x") else "SCALAR",
                                    paste(new_size, collapse="x"))); next }
        dset$set_extent(new_size)
      }
      
      obj <- .shape_for_dataset(mat, new_size, dset)
      .dwrite_diag(dset, obj)
      written <- c(written, p)
      next
    }
    
    # --- echte SCALAR (nur wenn nicht aus Wert inferiert) ---
    if (length(cur_size) == 0L && !isTRUE(inferred_from_value)) {
      val2 <- v
      if (is.list(val2) && length(val2) == 1L) val2 <- val2[[1L]]
      if (length(val2) != 1L) {
        if (scalar_strategy == "first") {
          val2 <- val2[1L]
        } else if (scalar_strategy == "collapse") {
          if (!is.character(val2)) { bail(sprintf("%s: collapse only for character vectors.", p)); next }
          val2 <- paste(val2, collapse = collapse_sep)
        } else {
          bail(sprintf("%s: SCALAR expects length 1, got %d.", p, length(val2))); next
        }
      }
      obj <- .shape_for_dataset(val2, integer(0), dset)
      .dwrite_diag(dset, obj)
      written <- c(written, p)
      next
    }
    
    # --- Vektor/Matrix/Array ---
    if (is.data.frame(v)) { bail(sprintf("%s: data.frame with %d cols unsupported (only 2-col timeseries).", p, ncol(v))); next }
    
    new_size <- if (is.null(dim(v))) as.integer(length(v)) else as.integer(dim(v))
    
    if (length(cur_size) == 0L || !all(cur_size == new_size)) {
      if (!resize) {
        bail(sprintf("%s: dims %s -> %s. Set resize=TRUE.",
                     p, if (length(cur_size)) paste(cur_size, collapse="x") else "SCALAR",
                     paste(new_size, collapse="x"))); next
      }
      dset$set_extent(new_size)
    }
    
    obj <- .shape_for_dataset(v, new_size, dset)
    .dwrite_diag(dset, obj)
    written <- c(written, p)
  }
  
  invisible(written)
}


#' Validate what would be written where (pre-flight check)
#'
#' Checks a named list of values against an open HDF5 file and summarizes,
#' per dataset path, the current dimensions (with a fallback via `h5$ls()`),
#' the length/column count of the supplied value, and a suggested handling
#' (`SCALAR`/`1D`/`ND` or `TS(2-col)` for 2-column time series).
#'
#' @param h5 An open \code{hdf5r::H5File} (e.g., opened with \code{mode = "r+"}).
#' @param values A named \code{list}. Names are absolute dataset paths (starting
#'   with \code{"/"}); values are the R objects to be written (scalar, vector,
#'   matrix/array, or a 2-column \code{data.frame}/\code{tibble} for time series).
#'
#' @return A \code{tibble} with columns:
#' \itemize{
#'   \item \code{path} – dataset path (after normalizing \code{"//"} → \code{"/"}).
#'   \item \code{cur_dims} – detected dims as \code{"2x4"} or \code{"SCALAR"}.
#'   \item \code{val_len} – length of the supplied value (for data frames: \code{nrow}).
#'   \item \code{df_cols} – number of columns (for data frames/tibbles; otherwise \code{NA}).
#'   \item \code{decision} – heuristic label: \code{"SCALAR"}, \code{"1D"}, \code{"ND"}, or \code{"TS(2-col)"}.
#'   \item \code{note} – additional note (e.g., \code{"not found"}).
#' }
#'
#' @details
#' If \code{get_simple_extent_dims()} incorrectly reports a dataset as \emph{SCALAR},
#' the function falls back to dimensions derived from \code{list_h5_datasets(h5)}
#' (i.e., \code{h5$ls(recursive = TRUE)}). Names in \code{values} are normalized so
#' that multiple leading slashes collapse to a single leading slash.
#'
#' @examples
#' \dontrun{
#' library(hdf5r)
#' h5 <- H5File$new("input.h5", mode = "r+")
#'
#' vals <- list(
#'   "/Parameters/OutputPath" = "C:/temp/out.h5",
#'   "/Rain/Hyetograph" = tibble::tibble(time = c(0, 10, 20, 30),
#'                                       value = c(0, 5, 12, 0)),
#'   "/Measures/.../LayerThickness" = c(150L, 150L)
#' )
#'
#' h5_validate_write(h5, vals)
#' }
#'
#' @seealso \code{\link{list_h5_datasets}}, \code{hdf5r}, and a matching writer like \code{h5_write_values()}.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
#' @export
h5_validate_write <- function(h5, values) {
  stopifnot(inherits(h5, "H5File"), is.list(values))
  names_norm <- sub("^//+", "/", names(values))
  lst <- list_h5_datasets(h5)
  dims_hint <- setNames(lapply(lst$dims, .parse_dims), lst$path)
  
  rows <- lapply(seq_along(values), function(i) {
    p <- names_norm[i]; v <- values[[i]]
    dset <- try(h5[[p]], silent = TRUE)
    if (inherits(dset, "try-error") || !inherits(dset, "H5D")) {
      return(tibble::tibble(path = p, cur_dims = NA, 
                            val_len = length(v),
                            df_cols = if (is.data.frame(v)) ncol(v) else NA_integer_,
                            decision = "SKIP (no dataset)", 
                            note = "not found"))
    }
    ext <- dset$get_space()$get_simple_extent_dims()
    cur <- as.numeric(ext$size)
    if (length(cur) == 0L) { # fallback per ls()
      hint <- dims_hint[[p]]
      if (length(hint)) cur <- hint
    }
    is_df <- is.data.frame(v)
    decision <- if (is_df && ncol(v) == 2L) "TS(2-col)" else if (length(cur) == 0L) "SCALAR" else if (length(cur) == 1L) "1D" else "ND"
    tibble::tibble(
      path = p,
      cur_dims = if (length(cur)) paste(cur, collapse="x") else "SCALAR",
      val_len = if (is_df) nrow(v) else length(v),
      df_cols = if (is_df) ncol(v) else NA_integer_,
      decision = decision,
      note = NA_character_
    )
  })
  dplyr::bind_rows(rows)
}
