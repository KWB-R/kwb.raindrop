#' Read Raindrop optimisation simulation results from HDF5
#' (parallel via future.apply + progress)
#'
#' Parallel variant of \code{get_simulation_results_optim()} using
#' \code{future.apply::future_lapply()} including optional progress reporting
#' via \pkg{progressr}.
#'
#' @inheritParams get_simulation_results_optim
#' @param workers Optional number of parallel workers. If not NULL,
#'   a temporary \code{future::multisession} plan is set.
#' @param show_progress Logical (default TRUE).
#' @param future_seed Passed to \code{future.apply::future_lapply()}.
#' @param progress_handler A progressr handler function (default:
#'   \code{progressr::handler_txtprogressbar}). If NULL, no handler is set.
#'
#' @return Named list (see \code{\link{get_simulation_results_optim}}).
#'
#' @export
#' @importFrom stats setNames
#' @importFrom hdf5r H5File
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor handlers handler_txtprogressbar
#' @importFrom kwb.utils resolve
get_simulation_results_optim_parallel <- function(paths,
                                                  path_list,
                                                  simulation_names,
                                                  debug = TRUE,
                                                  workers = NULL,
                                                  show_progress = TRUE,
                                                  future_seed = TRUE,
                                                  progress_handler = progressr::handler_txtprogressbar) {
  
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Package 'future.apply' is required.")
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required.")
  }
  if (show_progress && !requireNamespace("progressr", quietly = TRUE)) {
    stop("Package 'progressr' is required for progress reporting.")
  }
  
  message(sprintf(
    "Reading results files in parallel ('%s') for %d model runs%s",
    paste0(c(paths$file_results_hdf5_element,
             paths$file_results_hdf5_flaeche), collapse = "|"),
    length(simulation_names),
    if (!is.null(workers)) sprintf(" (workers=%d)", workers) else ""
  ))
  
  # Optional temporary future plan
  if (!is.null(workers)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
  }
  
  n <- length(simulation_names)
  
  # local handler setup (NOT global!)
  old_handlers <- NULL
  if (isTRUE(show_progress) && !is.null(progress_handler)) {
    old_handlers <- progressr::handlers()
    on.exit(progressr::handlers(old_handlers), add = TRUE)
    progressr::handlers(progress_handler)
  }
  
  # worker
  run_one <- function(s_name, p = NULL) {
    
    if (!is.null(p)) p(sprintf("Reading %s", s_name))
    
    run_paths <- kwb.utils::resolve(path_list, dir_target = s_name)
    
    if (!all(file.exists(c(run_paths$path_results_hdf5_element,
                           run_paths$path_results_hdf5_flaeche)))) {
      if (isTRUE(debug)) {
        message(sprintf("Missing files for %s -> returning NULL", s_name))
      }
      return(NULL)
    }
    
    res_hdf5_element <- hdf5r::H5File$new(run_paths$path_results_hdf5_element, mode = "r")
    res_hdf5_flaeche <- hdf5r::H5File$new(run_paths$path_results_hdf5_flaeche, mode = "r")
    
    on.exit({
      try(res_hdf5_element$close_all(), silent = TRUE)
      try(res_hdf5_flaeche$close_all(), silent = TRUE)
    }, add = TRUE)
    
    list(
      element = list(
        meta = read_hdf5_scalars(res_hdf5_element[["Metainfo"]], numeric_only = FALSE),
        rates = read_hdf5_timeseries(res_hdf5_element[["Raten"]]),
        water_balance = read_hdf5_scalars(res_hdf5_element[["Wasserbilanz"]]),
        states = read_hdf5_timeseries(res_hdf5_element[["Zustandsvariablen"]])
      ),
      connected_area = list(
        meta = read_hdf5_scalars(res_hdf5_flaeche[["Metainfo"]], numeric_only = FALSE),
        rates = read_hdf5_timeseries(res_hdf5_flaeche[["Raten"]]),
        water_balance = read_hdf5_scalars(res_hdf5_flaeche[["Wasserbilanz"]]),
        states = read_hdf5_timeseries(res_hdf5_flaeche[["Zustandsvariablen"]])
      )
    )
  }
  
  # --- try with_progress; fallback if nested handlers exist -----------------
  res_list <- NULL
  
  if (isTRUE(show_progress)) {
    res_list <- tryCatch(
      progressr::with_progress({
        p <- progressr::progressor(steps = n)
        
        future.apply::future_lapply(
          X = simulation_names,
          FUN = function(s_name) run_one(s_name, p = p),
          future.seed = future_seed
        )
      }),
      error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("handlers on the stack", msg, fixed = TRUE)) {
          # fallback: run without with_progress (no nesting issues)
          future.apply::future_lapply(
            X = simulation_names,
            FUN = function(s_name) run_one(s_name, p = NULL),
            future.seed = future_seed
          )
        } else {
          stop(e)
        }
      }
    )
  } else {
    res_list <- future.apply::future_lapply(
      X = simulation_names,
      FUN = function(s_name) run_one(s_name, p = NULL),
      future.seed = future_seed
    )
  }
  
  stats::setNames(res_list, simulation_names)
}