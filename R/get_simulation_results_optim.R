#' Read RainDrop optimisation simulation results from HDF5
#'
#' Reads per-run result files (HDF5) for both the *measure element* and the
#' *connected area* results and returns a named list (one entry per simulation).
#'
#' For each simulation name (e.g. `"s00001"`), the function resolves the run
#' directory via \code{kwb.utils::resolve(path_list, dir_target = s_name)} and
#' then loads standard result groups from two HDF5 files:
#' \itemize{
#'   \item \strong{element}: \code{Metainfo}, \code{Raten}, \code{Wasserbilanz}, \code{Zustandsvariablen}
#'   \item \strong{connected_area}: \code{Metainfo}, \code{Raten}, \code{Wasserbilanz}, \code{Zustandsvariablen}
#' }
#'
#' If the \strong{element} HDF5 is missing the entry is \code{NULL}; if only the
#' \strong{connected-area} HDF5 is missing, the entry is a list with the
#' \code{element} side populated and \code{connected_area = NULL}.
#' HDF5 handles are closed on exit via \code{on.exit(...$close_all())}.
#'
#' @param paths A list of path definitions. Used for messaging and expected to
#'   contain \code{file_results_hdf5_element} and \code{file_results_hdf5_flaeche}
#'   (file names). Note: within the loop, \code{paths} is overwritten by the
#'   resolved paths for the current simulation run.
#' @param path_list A list passed to \code{kwb.utils::resolve()} to generate
#'   run-specific paths (must yield \code{path_results_hdf5_element},
#'   \code{path_results_hdf5_flaeche}, and \code{dir_target_output}).
#' @param simulation_names Character vector of simulation run identifiers
#'   (e.g. \code{c("s00001", "s00002")}).
#' @param debug print debug messages (default: TRUE)
#' @param lean Logical. If \code{TRUE}, read only the fields consumed by
#'   \code{\link{add_overflow_events_and_waterbalance}} -- \code{element$rates},
#'   \code{element$water_balance} and \code{connected_area$water_balance} -- and
#'   leave \code{meta}/\code{states} (both sides) and \code{connected_area$rates}
#'   as \code{NULL}. This keeps per-run memory and I/O minimal when each run is
#'   thinned to its optimisation row immediately instead of collecting every
#'   run's full results first. Defaults to \code{FALSE} (read everything).
#' @return A named list with one entry per \code{simulation_names}. Each entry is
#'   either \code{NULL} (element HDF5 missing) or a nested list:
#' \describe{
#'   \item{element}{\describe{
#'     \item{meta}{Data.frame/list of scalar metadata (from \code{Metainfo}).}
#'     \item{rates}{Time series table (from \code{Raten}).}
#'     \item{water_balance}{Scalars table (from \code{Wasserbilanz}).}
#'     \item{states}{Time series table (from \code{Zustandsvariablen}).}
#'   }}
#'   \item{connected_area}{Same structure as \code{element}, read from the
#'     connected-area HDF5, or \code{NULL} if that file is missing for the run.}
#' }
#'
#' @details
#' The function uses \code{hdf5r::H5File$new(..., mode = "r")} to open the files
#' and registers an \code{on.exit(...$close_all())} so handles are released even
#' when the iteration body errors out.
#'
#' @seealso
#' \code{\link[kwb.utils]{resolve}},
#' \code{\link[kwb.raindrop]{read_hdf5_scalars}},
#' \code{\link[kwb.raindrop]{read_hdf5_timeseries}}
#'
#' @export
#' @importFrom stats setNames
#' @importFrom hdf5r H5File
get_simulation_results_optim <- function(paths,
                                         path_list,
                                         simulation_names,
                                         debug = TRUE,
                                         lean = FALSE) {

  if (isTRUE(debug)) {
    message(sprintf("Reading results files ('%s') for %d model runs",
                    paste0(c(paths$file_results_hdf5_element, paths$file_results_hdf5_flaeche), collapse = "|"),
                    length(simulation_names)))
  }
  stats::setNames(lapply(simulation_names, function(s_name) {

    paths <- kwb.utils::resolve(path_list, dir_target = s_name)

    has_element <- file.exists(paths$path_results_hdf5_element)
    has_flaeche <- file.exists(paths$path_results_hdf5_flaeche)

    # The element (Mulde_Rigole) H5 is the indispensable artefact; without it
    # nothing downstream is useful. A missing connected_area (Dach) file is
    # tolerated and yields connected_area = NULL (for example when
    # //Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv
    # is 0 the engine skips writing Dach.h5).
    if (!has_element) {
      if (isTRUE(debug)) {
        message(sprintf(
          "Missing element H5 for %s ('%s') -> returning NULL",
          s_name, paths$path_results_hdf5_element
        ))
      }
      return(NULL)
    }

    # Open + read in an inner function so its on.exit() handle-closing binds to
    # *its own* frame and always fires (even on error) before we decide what to
    # return. The surrounding tryCatch makes a result file that exists but is
    # unreadable -- e.g. the engine crashed mid-write for that scenario --
    # behave like a missing file (NULL + warning) instead of aborting the whole
    # (possibly parallel) batch. Downstream add_overflow_events_and_waterbalance()
    # then emits an NA row for the scenario.
    read_result <- function() {
      res_hdf5_element <- hdf5r::H5File$new(paths$path_results_hdf5_element, mode = "r")
      on.exit(try(res_hdf5_element$close_all(), silent = TRUE), add = TRUE)

      res_hdf5_flaeche <- if (has_flaeche) {
        h <- hdf5r::H5File$new(paths$path_results_hdf5_flaeche, mode = "r")
        on.exit(try(h$close_all(), silent = TRUE), add = TRUE)
        h
      } else {
        if (isTRUE(debug)) {
          message(sprintf(
            "No connected_area H5 for %s ('%s') -> connected_area = NULL",
            s_name, paths$path_results_hdf5_flaeche
          ))
        }
        NULL
      }

      kwb.utils::catAndRun(
        messageText = sprintf("(%d/%d)) Reading results files for model run %s",
                              which(simulation_names == s_name),
                              length(simulation_names),
                              paths$dir_target_output),
        expr = {
          element <- list(
            meta          = if (lean) NULL else kwb.raindrop::read_hdf5_scalars(res_hdf5_element[["Metainfo"]],
                                                            numeric_only = FALSE),
            rates         = kwb.raindrop::read_hdf5_timeseries(res_hdf5_element[["Raten"]]),
            water_balance = kwb.raindrop::read_hdf5_scalars(res_hdf5_element[["Wasserbilanz"]]),
            states        = if (lean) NULL else kwb.raindrop::read_hdf5_timeseries(res_hdf5_element[["Zustandsvariablen"]])
          )

          connected_area <- if (!is.null(res_hdf5_flaeche)) {
            list(
              meta          = if (lean) NULL else kwb.raindrop::read_hdf5_scalars(res_hdf5_flaeche[["Metainfo"]],
                                                              numeric_only = FALSE),
              rates         = if (lean) NULL else kwb.raindrop::read_hdf5_timeseries(res_hdf5_flaeche[["Raten"]]),
              water_balance = kwb.raindrop::read_hdf5_scalars(res_hdf5_flaeche[["Wasserbilanz"]]),
              states        = if (lean) NULL else kwb.raindrop::read_hdf5_timeseries(res_hdf5_flaeche[["Zustandsvariablen"]])
            )
          } else {
            NULL
          }

          list(element = element, connected_area = connected_area)
        },
        dbg = debug
      )
    }

    tryCatch(read_result(), error = function(e) {
      warning(sprintf(
        "Scenario '%s': result HDF5 unreadable ('%s'): %s -- treating as missing (NULL).",
        s_name, paths$path_results_hdf5_element, conditionMessage(e)
      ), call. = FALSE)
      NULL
    })
  }), nm = simulation_names)
}

