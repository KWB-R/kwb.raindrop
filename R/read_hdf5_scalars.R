#' Read scalar datasets from an HDF5 group
#'
#' @description
#' Reads all scalar datasets contained in an HDF5 group and returns them
#' as a tibble. By default, only numeric / integer scalars are returned.
#' If non-numeric scalars (e.g. strings) are present, they can either be
#' dropped with a warning or kept in a list-column.
#'
#' @param group
#'   An [`hdf5r::H5Group`] object. Direct children of this group are expected
#'   to be scalar datasets (i.e. `dataset.dims == 0`).
#' @param numeric_only
#'   Logical (default: `TRUE`). If `TRUE`, only numeric / integer scalars are
#'   returned (others are dropped with a warning). If `FALSE`, all scalars are
#'   returned in a list-column `value` together with a `type` column.
#'
#' @return
#' If `numeric_only = TRUE`:
#'   A tibble with columns
#'   \describe{
#'     \item{variable}{`character`. Dataset name within the group.}
#'     \item{value}{`numeric`. Scalar value read from the dataset.}
#'   }
#'
#' If `numeric_only = FALSE`:
#'   A tibble with columns
#'   \describe{
#'     \item{variable}{`character`. Dataset name within the group.}
#'     \item{value}{`list`. Scalar values (numeric, integer, character, ...).}
#'     \item{type}{`character`. First class of each value (e.g. `"numeric"`, `"character"`).}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Wasserbilanz: nur numerische Skalare
#'   wb_tbl <- read_hdf5_scalars(res_hdf5_element[["Wasserbilanz"]])
#'
#'   # Metainfo: gemischte Typen (Integer + String)
#'   meta_tbl <- read_hdf5_scalars(res_hdf5_element[["Metainfo"]],
#'                                 numeric_only = FALSE)
#' }
#'
#' @importFrom tibble tibble
#' @export
read_hdf5_scalars <- function(group, numeric_only = TRUE) {
  nm   <- group$ls()$name
  vals <- lapply(nm, function(x) group[[x]]$read())
  
  if (numeric_only) {
    is_num <- vapply(vals, is.numeric, logical(1))
    
    if (!all(is_num)) {
      warning(
        "Dropping non-numeric scalars: ",
        paste(nm[!is_num], collapse = ", ")
      )
    }
    
    nm   <- nm[is_num]
    vals <- vals[is_num]
    
    return(tibble::tibble(
      variable = nm,
      value    = as.numeric(unlist(vals))
    ))
  }
  
  # mixed types: keep everything as list-column
  tibble::tibble(
    variable = nm,
    value    = vals,
    type     = vapply(vals, function(x) class(x)[1], character(1))
  )
}
