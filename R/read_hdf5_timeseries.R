#' Read HDF5 Results Time Series from HDF5 Group into a Long Tibble
#'
#' @description
#' Extracts all datasets (no subgroups) from a given HDF5 group and converts
#' each **2×N numeric matrix** into a tidy long table. It assumes the first row
#' holds the time/index vector and the second row the values.
#' Datasets that are not 2×N numeric matrices (e.g. scalar metadata like
#' `von_Layer`, `von_Massnahmenelement`, ...) are silently ignored.
#'
#' @param ts_groupvariable `hdf5r::H5Group`
#'   An open HDF5 group whose *time-series* children are stored as 2×N numeric
#'   matrices (`[1, ] = time/index`, `[2, ] = value`), possibly mixed with
#'   scalar metadata datasets.
#'
#' @return A `tibble` with columns:
#' \itemize{
#'   \item `variable` (`character`): dataset name within the group.
#'   \item `time`     (`numeric`): time or index taken from the first row.
#'   \item `value`    (`numeric`): values taken from the second row.
#' }
#'
#' @importFrom tibble tibble as_tibble
#' @importFrom dplyr filter pull
#' @importFrom purrr map_dfr
#' @export
read_hdf5_timeseries <- function(ts_groupvariable) {
  
  # 1) Alle Datasets (keine Subgruppen) auflisten
  ds_tbl <- ts_groupvariable$ls() %>%
    tibble::as_tibble()
  
  ds_names <- ds_tbl %>%
    dplyr::filter(.data$obj_type == "H5I_DATASET") %>%
    dplyr::pull(.data$name)
  
  if (length(ds_names) == 0L) {
    return(tibble::tibble(
      variable = character(),
      time    = numeric(),
      value   = numeric()
    ))
  }
  
  # 2) Alles einlesen
  ds_list <- setNames(
    lapply(ds_names, function(nm) ts_groupvariable[[nm]]$read()),
    nm = ds_names
  )
  
  # 3) Nur echte 2×N-Zeitreihen behalten (numeric Matrix mit 2 Zeilen)
  valid_names <- names(ds_list)[vapply(
    ds_list,
    FUN.VALUE = logical(1),
    FUN = function(m) {
      is.matrix(m) &&
        is.numeric(m) &&
        nrow(m) == 2 &&
        ncol(m) >= 1
    }
  )]
  
  if (length(valid_names) == 0L) {
    # Keine passenden 2×N-Datasets → leeres Tibble
    return(tibble::tibble(
      variable = character(),
      time    = numeric(),
      value   = numeric()
    ))
  }
  
  # 4) In langes Tibble konvertieren
  states_long <- purrr::map_dfr(valid_names, function(nm) {
    m <- ds_list[[nm]]
    tibble::tibble(
      variable = nm,
      time  = as.numeric(m[1, ]),
      value = as.numeric(m[2, ])
    )
  })
  
  states_long
}
