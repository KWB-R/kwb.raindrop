#' Read HDF5 Results Time Series from HDF5 Group into a Long Tibble
#'
#' @description
#' Extracts all **datasets** (no subgroups) from a given HDF5 group and
#' converts each 2×N matrix into a tidy long table. It assumes the first row
#' holds the time/index vector and the second row the values.
#'
#' @param ts_groupvariable `hdf5r::H5Group`
#'   An open HDF5 group whose children are time-series datasets stored as 2×N
#'   numeric matrices (`[1, ] = time/index`, `[2, ] = value`). Typically a group
#'   like `"/Zustandsvariablen"` or similar in your model output file.
#'
#' @return A `tibble` with columns:
#' \itemize{
#'   \item `variable` (`character`): dataset name within the group.
#'   \item `time` (`numeric`): time or index taken from the first row.
#'   \item `value` (`numeric`): values taken from the second row.
#' }
#'
#' @details
#' The function lists all child objects of `ts_groupvariable`, filters for
#' datasets (`H5I_DATASET`), reads each dataset into memory, and stacks them
#' into one long tibble. Datasets are expected to be **2×N**; if your storage
#' differs (e.g., `N×2` or 1D), adapt the reading logic accordingly.
#'
#' @section Assumptions:
#' - Each dataset under `ts_groupvariable` is a numeric matrix of shape 2×N.
#' - Row 1 is the time/index vector; row 2 contains the values.
#'
#' @examples
#' \dontrun{
#' library(hdf5r)
#' f <- H5File$new("Optimierung_MuldenRigole.h5", mode = "r")
#' grp <- f[["Zustandsvariablen"]]
#' ts_long <- read_hdf5_timeseries(grp)
#' head(ts_long)
#' f$close_all()
#' }
#'
#' @seealso
#'   \code{\link[hdf5r]{H5File}}, \code{\link[hdf5r]{H5Group}}
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter pull
#' @importFrom purrr map_dfr
#' @export

read_hdf5_timeseries <- function(ts_groupvariable) {
  
  # 1) Nur Datasets (keine Subgruppen) auflisten
  ds_ts_groupvariable <- ts_groupvariable$ls() %>%
    tibble::as_tibble() %>%
    dplyr::filter(obj_type == "H5I_DATASET") %>%
    dplyr::pull(name)
  
  ds_ts_groupvariable
  
  
  # als benannte Liste mit Matrizen (2 x N)
  states_list <- setNames(lapply(ds_ts_groupvariable, function(nm) {
    ts_groupvariable[[nm]]$read()}), nm = ds_ts_groupvariable)
  
  # oder direkt als langes tibble (time/value angenommen: 1. Zeile = Zeit, 2. Zeile = Wert)
  states_long <- purrr::map_dfr(ds_ts_groupvariable, function(nm) {
    m <- ts_groupvariable[[nm]]$read()
    tibble::tibble(
      variable = nm,
      time  = as.numeric(m[1, ]),
      value = as.numeric(m[2, ])
    )
  })
  
  states_long
}
