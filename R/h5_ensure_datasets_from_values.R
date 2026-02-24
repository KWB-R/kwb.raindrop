#' Ensure that all datasets referenced by a values list exist
#'
#' Creates missing groups and datasets for all names(values). Datasets are created
#' with dtype/dims inferred from the corresponding value. No values are written here.
#'
#' @param h5 Open hdf5r::H5File (mode "a" or "r+").
#' @param values Named list; names are dataset paths (leading // allowed).
#' @param ts_cols Character(2). Column names for TS (default time/value).
#' @param ts_layout One of "2xN" or "Nx2". For RAINDROP typically "2xN" in HDF5.
#' @param ts_dtype Either an H5T object or a string ("double","integer","logical","float").
#' @param strict Stop if creation fails.
#' @return Invisibly, the character vector of created dataset paths.
#' @export
h5_ensure_datasets_from_values <- function(h5, values,
                                           ts_cols = c("time","value"),
                                           ts_layout = c("2xN","Nx2"),
                                           ts_dtype = "double",
                                           strict = TRUE) {
  stopifnot(inherits(h5, "H5File"))
  stopifnot(is.list(values), !is.null(names(values)))
  
  ts_layout <- match.arg(ts_layout)
  
  bail <- function(msg) {
    if (strict) stop(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    NULL
  }
  
  # normalize // -> /
  paths <- sub("^//+", "/", names(values))
  
  .as_h5_dtype <- function(dtype, value) {
    if (inherits(dtype, "H5T")) return(dtype)
    if (is.null(dtype)) dtype <- "double"
    if (is.character(dtype) && length(dtype) == 1L) {
      d <- tolower(dtype)
      if (d %in% c("double","numeric","float64")) return(hdf5r::h5types$H5T_NATIVE_DOUBLE)
      if (d %in% c("float","float32"))           return(hdf5r::h5types$H5T_NATIVE_FLOAT)
      if (d %in% c("integer","int","int32"))     return(hdf5r::h5types$H5T_NATIVE_INT)
      if (d %in% c("logical","bool","boolean"))  return(hdf5r::h5types$H5T_NATIVE_HBOOL)
      # Strings neu anzulegen ist je nach Build tricky (varlen/utf8). Daher nur wenn nötig:
      if (d %in% c("character","string"))        return(hdf5r::h5types$H5T_C_S1)
    }
    # infer
    if (is.logical(value)) return(hdf5r::h5types$H5T_NATIVE_HBOOL)
    if (is.integer(value)) return(hdf5r::h5types$H5T_NATIVE_INT)
    if (is.numeric(value)) return(hdf5r::h5types$H5T_NATIVE_DOUBLE)
    hdf5r::h5types$H5T_NATIVE_DOUBLE
  }
  
  .infer_dims <- function(v) {
    # TS: 2-col df/tibble -> create as 2xN (RAINDROP) or Nx2
    if (is.data.frame(v) && ncol(v) == 2L) {
      n <- nrow(v)
      if (ts_layout == "2xN") return(c(2L, as.integer(n)))
      return(c(as.integer(n), 2L))
    }
    # scalar
    if (is.null(dim(v)) && length(v) == 1L && !is.list(v)) return(integer(0))
    # vector / array
    if (!is.null(dim(v))) return(as.integer(dim(v)))
    as.integer(length(v))
  }
  
  .ensure_groups <- function(h5, grp_path) {
    grp_path <- sub("^//+", "/", grp_path)
    if (grp_path %in% c("/", ".", "")) return(invisible(TRUE))
    parts <- strsplit(sub("^/+", "", grp_path), "/", fixed = TRUE)[[1]]
    cur <- h5
    for (nm in parts) {
      nm <- as.character(nm)
      if (!cur$exists(nm)) cur$create_group(nm)
      cur <- cur[[nm]]
    }
    invisible(TRUE)
  }
  
  created <- character(0)
  
  for (i in seq_along(paths)) {
    p <- as.character(paths[i])
    if (h5$exists(p)) next
    
    v <- values[[i]]
    grp_path <- dirname(p); if (identical(grp_path, ".")) grp_path <- "/"
    ds_name  <- basename(p)
    
    .ensure_groups(h5, grp_path)
    
    dtype <- .as_h5_dtype(ts_dtype, v)
    dims  <- .infer_dims(v)
    
    # create dataset (fixed dims; scalar via robj)
    grp <- h5[[as.character(grp_path)]]
    
    
    if (length(dims) == 0L) {
      # SCALAR: must pass robj
      ok <- try(grp$create_dataset(
        as.character(ds_name),
        robj  = v,
        dtype = dtype
      ), silent = TRUE)
    } else {
      # ND: dims provided
      ok <- try(grp$create_dataset(
        as.character(ds_name),
        dtype = dtype,
        dims  = dims
      ), silent = TRUE)
    }
    
    if (inherits(ok, "try-error")) {
      bail(sprintf("Failed to create dataset '%s': %s", p, as.character(ok)))
      next
    }
    
    created <- c(created, p)
  }
  
  invisible(created)
}
