#' Read surface-water connections from an HDF5 results file
#'
#' @description
#' Reads an HDF5 file that contains surface-water *connections* (formerly
#' "Verschaltungen"), e.g. a file like `s78853_Verschaltungen.h5`. The file is
#' expected to have:
#'
#' * a root-level scalar dataset `"Anzahl Verschaltungen"`, and
#' * one group per connection (`"0"`, `"1"`, ...) that contains
#'   - scalar metadata (e.g. `von_Layer`, `von_Massnahmenelement`,
#'     `nach_Layer`, `nach_Massnahmenelement`) and
#'   - time-series datasets stored as 2×N matrices
#'     (e.g. `Qtatsaechlich`, `delta_h_pond_*`, `delta_theta_*`).
#'
#' Scalar metadata are returned
#'   - als breite Tabelle (`meta`),
#'   - als lange Tabelle (`scalars`) und
#'   - pro Verbindung in einer Unterliste (`connections$group_0`, ...).
#'
#' Time series werden in Long-Format zurückgegeben und mit den Metadaten
#' verknüpft.
#'
#' @param file
#'   An [`hdf5r::H5File`] object pointing to a `*_Verschaltungen.h5` file,
#'   already opened in read mode.
#'
#' @return
#' A named list with components:
#'
#' \describe{
#'   \item{n_connections}{`integer(1)`. Value of the root dataset
#'     `"Anzahl Verschaltungen"` (or `NA_integer_` if not present).}
#'
#'   \item{meta}{A tibble with one row per connection:
#'     `connection_id`, `from_layer`, `from_element`, `to_layer`, `to_element`.}
#'
#'   \item{timeseries}{A tibble with long-format time series for all
#'     connections:
#'     `connection_id`, `variable`, `time`, `value`,
#'     `from_layer`, `from_element`, `to_layer`, `to_element`.}
#'
#'   \item{scalars}{A tibble with all scalar datasets per connection in long
#'     format:
#'     `connection_id`, `variable`, `value` (list), `type`.}
#'
#'   \item{connections}{A named list of per-connection sublists:
#'     `connections$group_0`, `connections$group_1`, ...; each containing
#'     `connection_id`, `meta`, `timeseries`, `scalars` für genau diese
#'     Verbindung.}
#' }
#'
#' @examples
#' \dontrun{
#'   library(hdf5r)
#'
#'   f <- H5File$new(paths$path_verschaltungen_hdf5, mode = "r")
#'   conn <- read_hdf5_connections(f)
#'
#'   conn$n_connections
#'   conn$meta
#'   conn$timeseries
#'
#'   # Unterliste für die erste Verbindung:
#'   conn$connections$group_0$meta
#'   conn$connections$group_0$timeseries
#' }
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom dplyr filter pull mutate left_join
#' @importFrom purrr map_dfr
#' @export
read_hdf5_connections <- function(file) {
  
  # 1) Root listing
  root_ls <- file$ls() %>% tibble::as_tibble()
  
  # 2) Number of connections (if available)
  n_connections <- if ("Anzahl Verschaltungen" %in% root_ls$name) {
    file[["Anzahl Verschaltungen"]]$read()
  } else {
    NA_integer_
  }
  
  # 3) All groups at root level (e.g. "0", "1", "2", ...)
  group_names <- root_ls %>%
    dplyr::filter(.data$obj_type == "H5I_GROUP") %>%
    dplyr::pull(.data$name)
  
  # 4) All scalar datasets per connection (generic, long format)
  #    -> nutzt deine read_hdf5_scalars(..., numeric_only = FALSE)
  scalars <- purrr::map_dfr(group_names, function(gname) {
    grp <- file[[gname]]
    
    read_hdf5_scalars(grp, numeric_only = FALSE) %>%
      dplyr::mutate(
        connection_id = as.integer(gname),
        .before = 1
      )
  })
  
  # 5) Meta-Daten (breit, schön benannt) aus den bekannten Skalaren
  meta <- tibble::tibble(
    connection_id = as.integer(group_names),
    from_layer    = vapply(
      group_names,
      function(g) file[[g]][["von_Layer"]]$read(),
      integer(1)
    ),
    from_element  = vapply(
      group_names,
      function(g) file[[g]][["von_Massnahmenelement"]]$read(),
      character(1)
    ),
    to_layer      = vapply(
      group_names,
      function(g) file[[g]][["nach_Layer"]]$read(),
      integer(1)
    ),
    to_element    = vapply(
      group_names,
      function(g) file[[g]][["nach_Massnahmenelement"]]$read(),
      character(1)
    )
  )
  
  # 6) Time series per connection (2×N datasets; scalars werden in read_hdf5_timeseries() ignoriert)
  timeseries_raw <- purrr::map_dfr(group_names, function(gname) {
    grp <- file[[gname]]
    
    read_hdf5_timeseries(grp) %>%
      dplyr::mutate(
        connection_id = as.integer(gname)
      )
  })
  
  # 7) Attach metadata to time series
  timeseries <- dplyr::left_join(
    timeseries_raw,
    meta,
    by = "connection_id"
  )
  
  # 8) Unterlisten je Verbindung aufbauen: connections$group_0, group_1, ...
  connections <- lapply(seq_along(group_names), function(i) {
    id <- as.integer(group_names[i])
    
    list(
      connection_id = id,
      meta          = dplyr::filter(meta, .data$connection_id == id),
      timeseries    = dplyr::filter(timeseries, .data$connection_id == id),
      scalars       = dplyr::filter(scalars, .data$connection_id == id)
    )
  })
  names(connections) <- paste0("group_", group_names)
  
  list(
    n_connections = n_connections,
    meta          = meta,
    timeseries    = timeseries,
    scalars       = scalars,
    connections   = connections
  )
}
