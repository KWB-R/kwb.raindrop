#' Read HDF5 time series datasets from a group (supports deeperLayers)
#'
#' Reads all datasets in an HDF5 group and returns a long tibble with
#' columns: variable, time, value.
#'
#' Supported dataset layouts:
#' - k x N   (rows):   [1, ] = time, [2..k, ] = values (series)
#' - N x k   (cols):   [, 1] = time, [, 2..k] = values (series)
#'
#' Special handling for names containing "deeperLayers"/"deeper_layers":
#' - The value series represent layers below layer 1.
#' - Output variable names get suffixed with the layer-id:
#'     <name>_2, <name>_3, ...
#' - If the dataset contains only time (no value series), it returns 0 rows.
#'
#' @param ts_groupvariable `hdf5r::H5Group`
#' @param deeper_layers_pattern regex to detect deeper-layers datasets
#' @return tibble::tibble(variable, time, value)
#' @export
read_hdf5_timeseries <- function(
    ts_groupvariable,
    deeper_layers_pattern = "deeperLayers|deeper_layers"
) {
  
  stopifnot(inherits(ts_groupvariable, "H5Group"))
  
  ls_tbl <- ts_groupvariable$ls()
  ls_tbl <- tibble::as_tibble(ls_tbl)
  
  ds_names <- ls_tbl %>%
    dplyr::filter(.data$obj_type == "H5I_DATASET") %>%
    dplyr::pull(.data$name)
  
  if (length(ds_names) == 0L) {
    return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
  }
  
  # read datasets
  ds_list <- setNames(
    lapply(ds_names, function(nm) ts_groupvariable[[nm]]$read()),
    ds_names
  )
  
  # Helper: turn a single dataset matrix into a long tibble
  read_one <- function(m, nm) {
    is_deeper <- grepl(deeper_layers_pattern, nm)
    
    # Allow vector (sometimes returned for 1 x N or N x 1)
    if (is.vector(m) && is.numeric(m)) {
      # time-only for deeperLayers => no values
      if (is_deeper) {
        return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
      }
      # otherwise ambiguous -> ignore
      return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
    }
    
    if (!is.matrix(m) || !is.numeric(m)) {
      return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
    }
    
    nr <- nrow(m)
    nc <- ncol(m)
    
    # time-only matrix cases for deeperLayers
    if (is_deeper) {
      if ((nr == 1L && nc >= 2L) || (nc == 1L && nr >= 2L)) {
        # Only a time vector stored, no deeper layers defined
        return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
      }
    }
    
    # Need at least time + one value series
    if ((nr < 2L && nc < 2L)) {
      return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
    }
    
    # Decide orientation
    # Prefer the orientation where time is the first row when nr <= nc
    # otherwise treat time as the first column.
    if (nr <= nc) {
      # k x N, time in first row
      time <- as.numeric(m[1, ])
      values_mat <- m[-1, , drop = FALSE]  # (k-1) x N
      n_series <- nrow(values_mat)
      
      if (n_series < 1L) {
        return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
      }
      
      out <- purrr::map_dfr(seq_len(n_series), function(j) {
        var_name <- if (is_deeper) {
          paste0(nm, "_", j + 1L)  # layers start at 2
        } else if (n_series == 1L) {
          nm
        } else {
          paste0(nm, "_", j)
        }
        
        tibble::tibble(
          variable = var_name,
          time     = time,
          value    = as.numeric(values_mat[j, ])
        )
      })
      
      return(out)
      
    } else {
      # N x k, time in first column
      time <- as.numeric(m[, 1])
      values_mat <- m[, -1, drop = FALSE]  # N x (k-1)
      n_series <- ncol(values_mat)
      
      if (n_series < 1L) {
        return(tibble::tibble(variable = character(), time = numeric(), value = numeric()))
      }
      
      out <- purrr::map_dfr(seq_len(n_series), function(j) {
        var_name <- if (is_deeper) {
          paste0(nm, "_", j + 1L)
        } else if (n_series == 1L) {
          nm
        } else {
          paste0(nm, "_", j)
        }
        
        tibble::tibble(
          variable = var_name,
          time     = time,
          value    = as.numeric(values_mat[, j])
        )
      })
      
      return(out)
    }
  }
  
  # Keep only numeric matrices (and handle edge cases inside read_one)
  out <- purrr::map_dfr(names(ds_list), function(nm) {
    read_one(ds_list[[nm]], nm)
  })
  
  out
}
