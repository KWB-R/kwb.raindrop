#' @importFrom tibble tibble
#' @importFrom stats setNames
#' @importFrom hdf5r H5File
NULL

# ====================== Helpers ======================

# Robust writer: try multiple possible argument names across hdf5r variants
.dwrite <- function(dset, x, args = NULL) {
  tries <- list(
    list(args = args, value = x),
    list(args = args, obj   = x),
    list(args = args, data  = x),
    list(args = args, buf   = x),
    list(args = args, object= x),
    list(x)               # positional as very last resort
  )
  for (a in tries) {
    ok <- try(do.call(dset$write, a), silent = TRUE)
    if (!inherits(ok, "try-error")) return(invisible(TRUE))
  }
  stop("Failed to write dataset: none of the write() signatures worked.", call. = FALSE)
}

# human readable try-error
.err_text <- function(x) if (inherits(x, "try-error")) as.character(x) else paste0(x)

# parse dims from ls() "2 x 10" or "2x10"
.parse_dims <- function(s) {
  if (is.na(s) || !nzchar(s)) return(integer(0))
  as.integer(strsplit(gsub("\\s", "", s), "x", fixed = TRUE)[[1]])
}

# Infer HDF5 dims from value
.infer_dims_from_value <- function(v, ts_cols = c("time","value")) {
  if (is.data.frame(v) && ncol(v) == 2L) {
    # we treat TS as Nx2 in R, but will write as 2xN in HDF5
    n <- nrow(v)
    return(c(2L, as.integer(n)))
  }
  if (!is.null(dim(v))) return(as.integer(dim(v)))
  as.integer(length(v))
}

# Best-effort coercion to existing dataset type
.dataset_target_kind <- function(dset) {
  got <- try(dset$read(), silent = TRUE)
  if (inherits(got, "try-error")) return("unknown")
  if (is.character(got)) "character" else if (is.integer(got)) "integer"
  else if (is.double(got) || is.numeric(got)) "double"
  else if (is.logical(got)) "logical" else "unknown"
}

.normalize_for_dataset <- function(dset, x) {
  k <- .dataset_target_kind(dset)
  if (k == "character") return(enc2utf8(as.character(if (is.factor(x)) as.character(x) else x)))
  if (k == "integer")   return(as.integer(x))
  if (k == "double")    return(as.numeric(x))
  if (k == "logical")   return(as.logical(x))
  x
}

.shape_for_dataset <- function(x, target_dims, dset) {
  x <- .normalize_for_dataset(dset, x)
  if (length(target_dims) == 0L) {
    if (is.list(x) && length(x) == 1L) x <- x[[1L]]
    if (length(x) != 1L) stop(sprintf("SCALAR expects length 1, got %d.", length(x)), call. = FALSE)
    return(x)
  }
  if (length(target_dims) == 1L) {
    if (is.null(dim(x))) {
      if (length(x) != target_dims[1]) stop(sprintf("Length mismatch: %d vs %d", length(x), target_dims[1]), call. = FALSE)
      return(as.vector(x))
    }
    if (prod(dim(x)) != target_dims[1]) stop(sprintf("Length mismatch: %d vs %d", prod(dim(x)), target_dims[1]), call. = FALSE)
    return(as.vector(x))
  }
  # ND
  if (is.null(dim(x))) {
    if (length(x) != prod(target_dims)) stop(sprintf("Size mismatch: %d vs %d", length(x), prod(target_dims)), call. = FALSE)
    return(array(x, dim = target_dims))
  }
  if (!identical(as.integer(dim(x)), as.integer(target_dims))) {
    if (prod(dim(x)) != prod(target_dims)) {
      stop(sprintf("Dim mismatch: dim(x)=%s vs target=%s",
                   paste(dim(x), collapse = "x"), paste(target_dims, collapse = "x")), call. = FALSE)
    }
    return(array(as.vector(x), dim = target_dims))
  }
  x
}

# Ranked write helper (handles hdf5r variants that require explicit 'args')
#' @keywords internal
.dwrite_ranked <- function(dset, x, rank = NULL, dims = NULL) {
  # Backward compatible: old callers pass only `dims=...`
  if (is.null(rank)) {
    if (!is.null(dims)) {
      rank <- length(dims)
    } else {
      # fall back to dataset space rank
      ext <- dset$get_space()$get_simple_extent_dims()
      rank <- if (!is.null(ext$rank)) as.integer(ext$rank) else 0L
    }
  }
  rank <- as.integer(rank)
  
  # This hdf5r build requires an `args` argument.
  # For full write:
  # rank 0 (scalar): args = list()
  # rank k: args = rep(list(NULL), k)
  args_full <- if (rank <= 0L) list() else rep(list(NULL), rank)
  
  tries <- list(
    list(args = args_full, value = x),
    list(args = args_full, obj   = x),
    list(args = args_full, data  = x),
    list(args = args_full, buf   = x),
    list(args = args_full, object= x),
    list(args_full, x)  # positional (args, value)
  )
  
  errs <- character(0)
  for (a in tries) {
    res <- try(do.call(dset$write, a), silent = TRUE)
    if (!inherits(res, "try-error")) return(invisible(TRUE))
    errs <- c(errs, as.character(res))
  }
  
  # some builds accept scalar with args=1
  if (rank <= 0L) {
    for (a in list(
      list(args = 1L, value = x),
      list(args = 1L, obj   = x),
      list(args = 1L, data  = x),
      list(1L, x)
    )) {
      res <- try(do.call(dset$write, a), silent = TRUE)
      if (!inherits(res, "try-error")) return(invisible(TRUE))
      errs <- c(errs, as.character(res))
    }
  }
  
  stop(
    paste0(
      "write() failed. First: ", errs[1],
      if (length(errs) > 1) paste0("\nLast: ", utils::tail(errs, 1)) else ""
    ),
    call. = FALSE
  )
}



# ====================== 1) List ======================

#' List all datasets (recursive)
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @return A tibble with columns: \code{path}, \code{obj_type}, \code{dims}, \code{maxdims}.
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
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @param paths character vector; if NULL read all.
#' @param simplify_scalars logical: simplify scalar datasets to length-1 atoms.
#' @param timeseries_as_tibble logical: convert 2xN / Nx2 arrays to tibble(time,value).
#' @param ts_names character(2): names for time/value columns.
#' @return Named list.
#' @importFrom stats setNames
#' @importFrom tibble tibble
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
    } else stop("Not 2xN or Nx2.", call. = FALSE)
  }
  
  for (p in paths) {
    dset <- h5[[p]]
    if (!inherits(dset, "H5D")) stop(sprintf("Not a dataset: %s", p), call. = FALSE)
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

# ====================== 3) Write ======================

#' Write (updated) values back into existing HDF5 datasets (robust for your hdf5r build)
#'
#' - Scalars: write with required `args` (args=list() or args=1L fallback).
#' - 2-col TS (data.frame/tibble): expects Nx2 in R; writes as 2xN in HDF5 (RAINDROP style),
#'   using explicit hyperslab args=list(1:2, 1:N) to avoid empty selections.
#' - If TS length changes and dataset maxdims blocks resize, the dataset is deleted via
#'   parent$link_delete(name) and recreated with dims=2xN (fixed), then written.
#'   (No maxdims argument used; compatible with your create_dataset signature.)
#'
#' @param h5 Open hdf5r::H5File.
#' @param values Named list; names are dataset paths (leading // allowed).
#' @param resize Logical. If TRUE tries set_extent() where possible.
#' @param strict Logical. If TRUE stop on first error else warn and continue.
#' @param ts_cols Character(2). Column names for TS (default time/value).
#' @param scalar_strategy "error"|"first"|"collapse".
#' @param collapse_sep Separator for collapse.
#' @param ts_dtype Either an H5T object or one of "double","float","integer","logical".
#' @param verbose Logical.
#' @return Invisibly, written paths.
#' @export
h5_write_values <- function(h5, values,
                            resize = TRUE,
                            strict = TRUE,
                            ts_cols = c("time","value"),
                            scalar_strategy = c("error","first","collapse"),
                            collapse_sep = ";",
                            ts_dtype = "double",
                            verbose = FALSE) {
  
  scalar_strategy <- match.arg(scalar_strategy)
  stopifnot(inherits(h5, "H5File"))
  stopifnot(is.list(values), !is.null(names(values)), all(nzchar(names(values))))
  stopifnot(is.character(ts_cols), length(ts_cols) == 2L)
  
  bail <- function(msg) {
    if (strict) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    NULL
  }
  
  # normalize // -> /
  names(values) <- sub("^//+", "/", names(values))
  
  # ---- helpers ----
  
  .err_text <- function(x) if (inherits(x, "try-error")) as.character(x) else paste0(x)
  
  .is_unlimited <- function(x) is.na(x) || is.infinite(x) || x < 0
  
  .get_dims_maxdims <- function(dset) {
    ext <- dset$get_space()$get_simple_extent_dims()
    dims <- suppressWarnings(as.integer(ext$dims))
    if (length(dims) == 0L) dims <- suppressWarnings(as.integer(ext$size))
    maxdims <- ext$maxdims
    if (length(maxdims) == 0L) maxdims <- ext$max_size
    list(dims = dims, maxdims = maxdims, rank = ext$rank)
  }
  
  .as_h5_dtype <- function(dtype) {
    if (inherits(dtype, "H5T")) return(dtype)
    if (is.null(dtype)) return(hdf5r::h5types$H5T_NATIVE_DOUBLE)
    if (!is.character(dtype) || length(dtype) != 1L) {
      stop("ts_dtype must be an H5T object or a length-1 character like 'double'.", call. = FALSE)
    }
    d <- tolower(dtype)
    if (d %in% c("double","float64","numeric")) return(hdf5r::h5types$H5T_NATIVE_DOUBLE)
    if (d %in% c("float","float32"))           return(hdf5r::h5types$H5T_NATIVE_FLOAT)
    if (d %in% c("integer","int","int32"))     return(hdf5r::h5types$H5T_NATIVE_INT)
    if (d %in% c("logical","bool","boolean"))  return(hdf5r::h5types$H5T_NATIVE_HBOOL)
    stop(sprintf("Unknown ts_dtype '%s'. Use 'double','float','integer','logical' or H5T.", dtype), call. = FALSE)
  }
  
  # Robust write for your build: requires args, and value name varies
  .write_with_args <- function(dset, args, x) {
    tries <- list(
      list(args = args, value  = x),
      list(args = args, obj    = x),
      list(args = args, data   = x),
      list(args = args, buf    = x),
      list(args = args, object = x),
      list(args, x) # positional (args, value)
    )
    errs <- character(0)
    for (a in tries) {
      res <- try(do.call(dset$write, a), silent = TRUE)
      if (!inherits(res, "try-error")) return(invisible(TRUE))
      errs <- c(errs, .err_text(res))
    }
    stop(paste0("write() failed. First: ", errs[1], if (length(errs) > 1) paste0("\nLast: ", utils::tail(errs,1)) else ""), call. = FALSE)
  }
  
  .delete_dataset <- function(h5, path) {
    path <- sub("^//+", "/", path)
    parent_path <- dirname(path)
    if (identical(parent_path, ".")) parent_path <- "/"
    name <- basename(path)
    parent <- h5[[parent_path]]  # H5Group
    ok <- try(parent$link_delete(name), silent = TRUE)
    if (inherits(ok, "try-error")) {
      stop(sprintf("Cannot delete '%s' via %s$link_delete('%s'): %s",
                   path, parent_path, name, .err_text(ok)),
           call. = FALSE)
    }
    invisible(TRUE)
  }
  
  # Recreate TS as fixed 2xN (no maxdims) – compatible with your create_dataset signature
  .recreate_ts_2xN <- function(h5, path, n, dtype) {
    if (h5$exists(path)) .delete_dataset(h5, path)
    h5$create_dataset(path, dtype = dtype, dims = c(2L, as.integer(n)))
    invisible(TRUE)
  }
  
  .same_matrix <- function(a, b) {
    if (!is.matrix(a) && !is.array(a)) return(FALSE)
    if (!is.matrix(b) && !is.array(b)) return(FALSE)
    if (!identical(as.integer(dim(a)), as.integer(dim(b)))) return(FALSE)
    if (is.numeric(a) && is.numeric(b)) return(isTRUE(all.equal(a, b, tolerance = 0)))
    identical(a, b)
  }
  
  # ---- main ----
  written <- character(0)
  ts_h5t <- .as_h5_dtype(ts_dtype)
  
  for (p in names(values)) {
    v <- values[[p]]
    
    dset <- try(h5[[p]], silent = TRUE)
    if (inherits(dset, "try-error") || !inherits(dset, "H5D")) {
      bail(sprintf("No dataset: %s", p)); next
    }
    
    # =========================
    # TS: 2 columns in R (Nx2) -> HDF5 2xN (write transpose with explicit args)
    # =========================
    if (is.data.frame(v)) {
      if (ncol(v) != 2L) { bail(sprintf("%s: TS must have exactly 2 columns.", p)); next }
      
      col1 <- if (all(ts_cols %in% names(v))) v[[ts_cols[1]]] else v[[1]]
      col2 <- if (all(ts_cols %in% names(v))) v[[ts_cols[2]]] else v[[2]]
      n <- length(col1)
      if (length(col2) != n) { bail(sprintf("%s: TS columns differ in length.", p)); next }
      
      mat_r  <- cbind(col1, col2)  # n x 2
      mat_h5 <- t(mat_r)           # 2 x n (matches HDF5 dataset layout)
      target_h5 <- c(2L, as.integer(n))
      
      info <- .get_dims_maxdims(dset)
      dims_h5 <- as.integer(info$dims)
      maxdims_h5 <- info$maxdims
      
      # skip if dims same and content same
      if (length(dims_h5) == 2L && identical(dims_h5, target_h5)) {
        cur <- try(dset$read(), silent = TRUE)
        if (!inherits(cur, "try-error") && .same_matrix(cur, mat_h5)) {
          next
        }
      }
      
      # Decide if we can resize or must recreate (blocked by fixed maxdims or wrong structure)
      structure_ok <- (length(dims_h5) == 2L && dims_h5[1] == 2L)
      
      blocked <- FALSE
      if (structure_ok && length(maxdims_h5) == 2L) {
        md2 <- maxdims_h5[2]
        if (!.is_unlimited(md2) && is.finite(md2) && target_h5[2] > as.integer(md2)) blocked <- TRUE
      }
      
      if (!structure_ok || blocked) {
        if (verbose) message(sprintf("[h5] recreate TS %s (dims=%s max=%s) -> dims=%s",
                                     p,
                                     if (length(dims_h5)) paste(dims_h5, collapse="x") else "NA",
                                     if (length(maxdims_h5)==2L) paste(maxdims_h5, collapse="x") else "NA",
                                     paste(target_h5, collapse="x")))
        .recreate_ts_2xN(h5, p, n = n, dtype = ts_h5t)
        dset <- h5[[p]]
        dims_h5 <- target_h5
      } else {
        if (!identical(dims_h5, target_h5)) {
          if (!resize) { bail(sprintf("%s: dims %s -> %s (set resize=TRUE).", p, paste(dims_h5, collapse="x"), paste(target_h5, collapse="x"))); next }
          dset$set_extent(target_h5)
        }
      }
      
      # Explicit args for full selection to avoid "expected 0"
      args_full <- list(1:2, seq_len(n))
      res <- try(.write_with_args(dset, args_full, mat_h5), silent = TRUE)
      if (inherits(res, "try-error")) { bail(sprintf("write() failed for %s: %s", p, .err_text(res))); next }
      
      written <- c(written, p)
      next
    }
    
    # =========================
    # Scalars / ND
    # =========================
    info <- .get_dims_maxdims(dset)
    cur_dims <- as.integer(info$dims)
    
    # SCALAR dataset
    if (length(cur_dims) == 0L) {
      val2 <- v
      if (is.list(val2) && length(val2) == 1L) val2 <- val2[[1L]]
      
      if (length(val2) != 1L) {
        if (scalar_strategy == "first") {
          val2 <- val2[1L]
        } else if (scalar_strategy == "collapse") {
          if (!is.character(val2)) { bail(sprintf("%s: collapse only for character.", p)); next }
          val2 <- paste(val2, collapse = collapse_sep)
        } else {
          bail(sprintf("%s: SCALAR expects length 1, got %d.", p, length(val2))); next
        }
      }
      
      cur <- try(dset$read(), silent = TRUE)
      if (!inherits(cur, "try-error") && length(cur) == 1L && isTRUE(all.equal(cur, val2, tolerance = 0))) {
        next
      }
      
      # scalar write: args=list() (fallback args=1L handled here)
      ok <- try(.write_with_args(dset, list(), val2), silent = TRUE)
      if (inherits(ok, "try-error")) {
        ok2 <- try(.write_with_args(dset, 1L, val2), silent = TRUE)
        if (inherits(ok2, "try-error")) { bail(sprintf("write() failed for %s: %s", p, .err_text(ok2))); next }
      }
      
      written <- c(written, p)
      next
    }
    
    # ND dataset (non-TS, non-scalar)
    if (is.data.frame(v)) { bail(sprintf("%s: data.frame unsupported (only 2-col TS).", p)); next }
    
    new_dims <- if (is.null(dim(v))) as.integer(length(v)) else as.integer(dim(v))
    
    # Skip if dims and values equal
    if (identical(cur_dims, new_dims)) {
      cur <- try(dset$read(), silent = TRUE)
      if (!inherits(cur, "try-error")) {
        same <- if (is.numeric(cur) && is.numeric(v)) isTRUE(all.equal(cur, v, tolerance = 0)) else identical(cur, v)
        if (isTRUE(same)) next
      }
    }
    
    if (!identical(cur_dims, new_dims)) {
      if (!resize) { bail(sprintf("%s: dims %s -> %s (set resize=TRUE).", p, paste(cur_dims, collapse="x"), paste(new_dims, collapse="x"))); next }
      dset$set_extent(new_dims)
    }
    
    # Full selection args: list(NULL,...,NULL) can be empty-selection in your build,
    # so for rank 1/2 we set explicit indices; for rank>2 fallback to NULL selection.
    rank <- length(new_dims)
    if (rank == 1L) {
      args_full <- list(seq_len(new_dims[1]))
      ok <- try(.write_with_args(dset, args_full, as.vector(v)), silent = TRUE)
    } else if (rank == 2L) {
      args_full <- list(seq_len(new_dims[1]), seq_len(new_dims[2]))
      ok <- try(.write_with_args(dset, args_full, v), silent = TRUE)
    } else {
      args_full <- rep(list(NULL), rank)  # best effort
      ok <- try(.write_with_args(dset, args_full, v), silent = TRUE)
    }
    
    if (inherits(ok, "try-error")) { bail(sprintf("write() failed for %s: %s", p, .err_text(ok))); next }
    
    written <- c(written, p)
  }
  
  invisible(written)
}


#' Validate what would be written where (pre-flight check)
#'
#' Summarizes for each path:
#' - current dataset dims/maxdims (HDF5 order)
#' - intended value shape (R order)
#' - intended write shape (HDF5 order; for TS always 2xN)
#' - decision: SKIP / WRITE / RESIZE / NEED_RECREATE / ERROR
#'
#' Notes:
#' - 2-column data.frames/tibbles are treated as Nx2 in R and mapped to 2xN in HDF5.
#' - Many RAINDROP files store time series as HDF5 dims 2xN (appearing as Nx2 in R).
#'
#' @param h5 An open \code{hdf5r::H5File}.
#' @param values Named list of values to write (names are HDF5 paths).
#' @param ts_cols Character(2). Column names for TS (default time/value).
#' @return tibble
#' @export
h5_validate_write <- function(h5, values, ts_cols = c("time","value")) {
  stopifnot(inherits(h5, "H5File"), is.list(values), !is.null(names(values)))
  
  # normalize // -> /
  paths <- sub("^//+", "/", names(values))
  
  # helper: get dims/maxdims reliably from dataset space
  .get_dims_maxdims <- function(dset) {
    ext <- dset$get_space()$get_simple_extent_dims()
    dims <- suppressWarnings(as.integer(ext$dims))
    if (length(dims) == 0L) dims <- suppressWarnings(as.integer(ext$size))
    maxdims <- ext$maxdims
    if (length(maxdims) == 0L) maxdims <- ext$max_size
    list(dims = dims, maxdims = maxdims)
  }
  
  .is_unlimited <- function(x) is.na(x) || is.infinite(x) || x < 0
  
  rows <- lapply(seq_along(paths), function(i) {
    p <- paths[i]
    v <- values[[i]]
    
    dset <- try(h5[[p]], silent = TRUE)
    if (inherits(dset, "try-error") || !inherits(dset, "H5D")) {
      return(tibble::tibble(
        path = p,
        exists = FALSE,
        cur_dims_h5 = NA_character_,
        cur_maxdims_h5 = NA_character_,
        val_kind = if (is.data.frame(v)) "data.frame" else class(v)[1],
        target_dims_r = NA_character_,
        target_dims_h5 = NA_character_,
        decision = "ERROR",
        note = "dataset not found"
      ))
    }
    
    info <- .get_dims_maxdims(dset)
    dims_h5 <- info$dims
    maxdims_h5 <- info$maxdims
    
    cur_dims_chr <- if (length(dims_h5)) paste(dims_h5, collapse="x") else "SCALAR"
    cur_max_chr  <- if (length(maxdims_h5)==length(dims_h5) && length(dims_h5)) {
      paste(ifelse(.is_unlimited(maxdims_h5), "Inf", as.character(maxdims_h5)), collapse="x")
    } else if (length(dims_h5)==0L) "SCALAR" else NA_character_
    
    # defaults
    target_r <- NA_character_
    target_h5 <- NA_character_
    decision <- "WRITE"
    note <- NA_character_
    
    if (is.data.frame(v)) {
      if (ncol(v) != 2L) {
        return(tibble::tibble(
          path = p, exists = TRUE,
          cur_dims_h5 = cur_dims_chr, cur_maxdims_h5 = cur_max_chr,
          val_kind = sprintf("data.frame(%d cols)", ncol(v)),
          target_dims_r = NA_character_, target_dims_h5 = NA_character_,
          decision = "ERROR",
          note = "TS must have exactly 2 columns"
        ))
      }
      col1 <- if (all(ts_cols %in% names(v))) v[[ts_cols[1]]] else v[[1]]
      col2 <- if (all(ts_cols %in% names(v))) v[[ts_cols[2]]] else v[[2]]
      n <- length(col1)
      if (length(col2) != n) {
        return(tibble::tibble(
          path = p, exists = TRUE,
          cur_dims_h5 = cur_dims_chr, cur_maxdims_h5 = cur_max_chr,
          val_kind = "TS(2-col)",
          target_dims_r = NA_character_, target_dims_h5 = NA_character_,
          decision = "ERROR",
          note = "TS columns have different lengths"
        ))
      }
      
      # intended shapes: R Nx2, HDF5 2xN
      target_r  <- paste(c(n, 2L), collapse="x")
      target_h5 <- paste(c(2L, n), collapse="x")
      
      # can we reach 2xN by resize?
      if (length(dims_h5) != 2L) {
        decision <- "NEED_RECREATE"
        note <- "dataset rank != 2"
      } else if (dims_h5[1] != 2L) {
        decision <- "NEED_RECREATE"
        note <- "TS expects HDF5 dims 2xN"
      } else {
        # maxdims check in N direction (second dim)
        md2 <- if (length(maxdims_h5)==2L) maxdims_h5[2] else NA
        if (!.is_unlimited(md2) && is.finite(md2) && n > md2) {
          decision <- "NEED_RECREATE"
          note <- sprintf("target N=%d exceeds maxdims N=%s", n, md2)
        } else if (dims_h5[2] != n) {
          decision <- "RESIZE+WRITE"
          note <- "resize N then write"
        } else {
          decision <- "WRITE_or_SKIP"
          note <- "dims match; can skip if values equal"
        }
      }
      
    } else if (length(dims_h5) == 0L) {
      # scalar dataset
      len <- length(v)
      target_r <- "1"
      target_h5 <- "SCALAR"
      if (len != 1L) {
        decision <- "ERROR"
        note <- sprintf("scalar dataset but value length=%d", len)
      } else {
        decision <- "WRITE_or_SKIP"
        note <- "scalar; can skip if equal"
      }
      
    } else {
      # ND: compare dims if possible
      new_dims <- if (is.null(dim(v))) as.integer(length(v)) else as.integer(dim(v))
      target_r <- paste(new_dims, collapse="x")
      target_h5 <- target_r
      
      md <- maxdims_h5
      if (length(md)==length(new_dims)) {
        too_big <- any(!.is_unlimited(md) & is.finite(md) & new_dims > md)
        if (too_big) {
          decision <- "ERROR"
          note <- sprintf("target dims %s exceed maxdims %s", target_h5, cur_max_chr)
        } else if (!identical(as.integer(dims_h5), as.integer(new_dims))) {
          decision <- "RESIZE+WRITE"
          note <- "resize then write"
        } else {
          decision <- "WRITE_or_SKIP"
          note <- "dims match; can skip if equal"
        }
      } else {
        decision <- "WRITE_or_SKIP"
        note <- "maxdims unknown; can attempt write"
      }
    }
    
    tibble::tibble(
      path = p,
      exists = TRUE,
      cur_dims_h5 = cur_dims_chr,
      cur_maxdims_h5 = cur_max_chr,
      val_kind = if (is.data.frame(v) && ncol(v)==2L) "TS(2-col)" else class(v)[1],
      target_dims_r = target_r,
      target_dims_h5 = target_h5,
      decision = decision,
      note = note
    )
  })
  
  dplyr::bind_rows(rows)
}

