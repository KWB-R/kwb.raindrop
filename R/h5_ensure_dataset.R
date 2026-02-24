#' Ensure that an HDF5 dataset exists (create if missing)
#'
#' Creates missing groups along the path and then creates the dataset.
#' Designed for RAINDROP input files where missing scalar parameters can
#' crash the model definition reader.
#'
#' @param h5 Open \code{hdf5r::H5File} (mode "a" or "r+").
#' @param path Absolute dataset path (e.g. "/Berechnungsparameter/Zeitschritt_Verschaltungen").
#' @param value Initial value to write after creation (scalar, vector, matrix, or 2-col TS).
#' @param dtype Optional. Either an \code{H5T} object or a string ("double","integer","logical","character").
#'   If NULL, inferred from \code{value}.
#' @param dims Optional integer vector. If NULL, inferred from \code{value}.
#' @return Invisibly TRUE
#' @export
h5_ensure_dataset <- function(h5, path, value, dtype = NULL, dims = NULL) {
  stopifnot(inherits(h5, "H5File"))
  path <- sub("^//+", "/", as.character(path))
  stopifnot(startsWith(path, "/"))
  
  # ---- dtype mapper for your hdf5r build (expects H5T objects)
  as_h5t <- function(dtype, value) {
    if (inherits(dtype, "H5T")) return(dtype)
    if (!is.null(dtype)) {
      d <- tolower(as.character(dtype))
      if (d %in% c("double","numeric","float64")) return(hdf5r::h5types$H5T_NATIVE_DOUBLE)
      if (d %in% c("float","float32"))           return(hdf5r::h5types$H5T_NATIVE_FLOAT)
      if (d %in% c("integer","int","int32"))     return(hdf5r::h5types$H5T_NATIVE_INT)
      if (d %in% c("logical","bool","boolean"))  return(hdf5r::h5types$H5T_NATIVE_HBOOL)
      if (d %in% c("character","string"))        return(hdf5r::h5types$H5T_C_S1) # scalar strings often OK as existing; creating varlen is trickier
    }
    # infer
    if (is.logical(value)) return(hdf5r::h5types$H5T_NATIVE_HBOOL)
    if (is.integer(value)) return(hdf5r::h5types$H5T_NATIVE_INT)
    if (is.numeric(value)) return(hdf5r::h5types$H5T_NATIVE_DOUBLE)
    if (is.character(value)) return(hdf5r::h5types$H5T_C_S1)
    hdf5r::h5types$H5T_NATIVE_DOUBLE
  }
  
  infer_dims <- function(v) {
    if (is.data.frame(v) && ncol(v) == 2L) {
      # R Nx2 -> HDF5 2xN (RAINDROP style)
      n <- nrow(v)
      return(c(2L, as.integer(n)))
    }
    if (!is.null(dim(v))) return(as.integer(dim(v)))
    if (length(v) == 1L) return(integer(0))         # scalar
    as.integer(length(v))                           # 1D
  }
  
  if (h5$exists(path)) return(invisible(TRUE))
  
  # ---- ensure groups exist
  grp_path <- dirname(path)
  if (identical(grp_path, ".")) grp_path <- "/"
  parts <- strsplit(sub("^/+", "", grp_path), "/", fixed = TRUE)[[1]]
  cur <- h5
  if (length(parts) && nzchar(parts[1])) {
    for (nm in parts) {
      if (!cur$exists(nm)) cur$create_group(nm)
      cur <- cur[[nm]]
    }
  }
  
  # ---- create dataset
  if (is.null(dims))  dims  <- infer_dims(value)
  h5t <- as_h5t(dtype, value)
  
  # NOTE: your hdf5r create_dataset() does NOT support maxdims; we create fixed size.
  cur$create_dataset(basename(path), dtype = h5t, dims = if (length(dims)) dims else NULL)
  
  invisible(TRUE)
}
