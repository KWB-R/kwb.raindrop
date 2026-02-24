#' Read RAINDROP error logs into a nested tibble
#'
#' Reads the RAINDROP error protocol HDF5 (typically \code{Fehlerprotokoll.h5})
#' for a set of simulation runs and returns a tibble with one row per run and a
#' nested tibble column containing all errors found in the file.
#'
#' The error HDF5 usually contains:
#' \itemize{
#'   \item \code{/AnzahlFehler} (scalar integer)
#'   \item groups \code{/0}, \code{/1}, ... each containing datasets such as
#'         \code{Fehlerbeschreibung}, \code{Layer1}, \code{Layer2},
#'         \code{Massnahmenelement1}, \code{Massnahmenelement2}
#' }
#'
#' Note: In \pkg{hdf5r}, HDF5 group names must be addressed as character strings.
#' So group \code{0} must be accessed as \code{h5[["0"]]} (not \code{h5[[0]]}).
#'
#' @param simulation_names Character vector of simulation run names (e.g. \code{"s00001"}).
#' @param path_list A path list object passed to \code{kwb.utils::resolve()}.
#'   Must resolve to an element \code{path_errors_hdf5}.
#' @param debug Logical. If \code{TRUE}, pass through to \code{kwb.utils::catAndRun()}.
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{id} Integer simulation id (parsed from \code{simulation_names}).
#'   \item \code{scenario_name} The simulation name (e.g. \code{"s00001"}).
#'   \item \code{path} File path to the error HDF5.
#'   \item \code{file_exists} Logical.
#'   \item \code{number_of_errors} Scalar integer or \code{NA}.
#'   \item \code{errors} Nested tibble (list-column) with per-error rows:
#'     \code{error_index}, \code{Fehlerbeschreibung}, \code{Layer1}, \code{Layer2},
#'     \code{Massnahmenelement1}, \code{Massnahmenelement2}
#' }
#'
#' @examples
#' \dontrun{
#' errors_df <- read_raindrop_errors(simulation_names, path_list)
#'
#' # flatten all errors:
#' all_errors <- tidyr::unnest(errors_df, errors)
#' }
#'
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
read_raindrop_errors <- function(simulation_names, path_list, debug = FALSE) {
  
  # ---- helpers ----
  .safe_read_scalar <- function(h5, path) {
    x <- try(h5[[path]]$read(), silent = TRUE)
    if (inherits(x, "try-error")) return(NA)
    # hdf5r sometimes returns list for scalar strings
    if (is.list(x) && length(x) == 1L) x <- x[[1L]]
    x
  }
  
  .safe_read_in_group <- function(grp, name) {
    x <- try(grp[[name]]$read(), silent = TRUE)
    if (inherits(x, "try-error")) return(NA)
    if (is.list(x) && length(x) == 1L) x <- x[[1L]]
    x
  }
  
  .read_one_file <- function(path_errors_hdf5) {
    h5 <- hdf5r::H5File$new(path_errors_hdf5, mode = "r")
    on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)
    
    n_err <- .safe_read_scalar(h5, "AnzahlFehler")
    
    # groups are "0","1",... (strings)
    nms <- names(h5)
    grp_names <- setdiff(nms, "AnzahlFehler")
    
    # keep only purely numeric group names and sort numerically
    grp_names <- grp_names[grepl("^[0-9]+$", grp_names)]
    grp_names <- grp_names[order(as.integer(grp_names))]
    
    errs <- lapply(grp_names, function(g) {
      grp <- h5[[g]]
      tibble::tibble(
        error_index = as.integer(g),
        Fehlerbeschreibung = .safe_read_in_group(grp, "Fehlerbeschreibung"),
        Layer1 = suppressWarnings(as.integer(.safe_read_in_group(grp, "Layer1"))),
        Layer2 = suppressWarnings(as.integer(.safe_read_in_group(grp, "Layer2"))),
        Massnahmenelement1 = .safe_read_in_group(grp, "Massnahmenelement1"),
        Massnahmenelement2 = .safe_read_in_group(grp, "Massnahmenelement2")
      )
    })
    
    tibble::tibble(
      number_of_errors = suppressWarnings(as.integer(n_err)),
      errors = list(dplyr::bind_rows(errs))
    )
  }
  
  # ---- main ----
  errors_df <- lapply(simulation_names, function(s_name) {
    
    s_id <- s_name %>% stringr::str_remove("^s") %>% as.integer()
    paths <- kwb.utils::resolve(path_list, dir_target = s_name)
    
    file_exists <- fs::file_exists(paths$path_errors_hdf5)
    
    if (!file_exists) {
      return(tibble::tibble(
        id = s_id,
        scenario_name = s_name,
        path = paths$path_errors_hdf5,
        file_exists = FALSE,
        number_of_errors = NA_integer_,
        errors = list(tibble::tibble(
          error_index = integer(0),
          Fehlerbeschreibung = character(0),
          Layer1 = integer(0),
          Layer2 = integer(0),
          Massnahmenelement1 = character(0),
          Massnahmenelement2 = character(0)
        ))
      ))
    }
    
    kwb.utils::catAndRun(
      messageText = sprintf("Reading error file '%s'", paths$path_errors_hdf5),
      expr = {
        one <- .read_one_file(paths$path_errors_hdf5)
        tibble::tibble(
          id = s_id,
          scenario_name = s_name,
          path = paths$path_errors_hdf5,
          file_exists = TRUE,
          number_of_errors = one$number_of_errors[[1]],
          errors = one$errors
        )
      },
      dbg = debug
    )
  }) %>%
    dplyr::bind_rows()
  
  errors_df
}
