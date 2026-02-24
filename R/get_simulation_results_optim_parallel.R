#' Read Raindrop optimisation simulation results from HDF5
#' (parallel via future.apply + progress)
#'
#' Parallel variant of \code{get_simulation_results_optim()} using
#' \code{future.apply::future_lapply()} including progress reporting
#' via \code{progressr}.
#'
#' @inheritParams get_simulation_results_optim
#' @param workers Optional number of parallel workers. If not NULL,
#'   a temporary \code{future::multisession} plan is set.
#' @param show_progress Logical (default TRUE).
#' @param future_seed Passed to \code{future.apply::future_lapply()}.
#'
#' @return Named list (see \code{\link{get_simulation_results_optim}}).
#'
#' @export
#' @importFrom stats setNames
#' @importFrom hdf5r H5File
#' @importFrom future plan multisession
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor handlers
#' @importFrom kwb.utils resolve
get_simulation_results_optim_parallel <- function(paths,
                                                  path_list,
                                                  simulation_names,
                                                  debug = TRUE,
                                                  workers = NULL,
                                                  show_progress = TRUE,
                                                  future_seed = TRUE) {
  
  if (!requireNamespace("future.apply", quietly = TRUE)) {
    stop("Package 'future.apply' is required.")
  }
  if (!requireNamespace("future", quietly = TRUE)) {
    stop("Package 'future' is required.")
  }
  
  message(sprintf(
    "Reading results files in parallel ('%s') for %d model runs%s",
    paste0(c(paths$file_results_hdf5_element,
             paths$file_results_hdf5_flaeche), collapse = "|"),
    length(simulation_names),
    if (!is.null(workers)) sprintf(" (workers=%d)", workers) else ""
  ))
  
  # Optional temporary future plan
  old_plan <- NULL
  if (!is.null(workers)) {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
  }
  
  n <- length(simulation_names)
  
  # ---- progressr integration ----
  if (show_progress) {
    if (!requireNamespace("progressr", quietly = TRUE)) {
      stop("Package 'progressr' required for progress reporting.")
    }
    progressr::handlers(global = TRUE)
  }
  
  res_list <- if (show_progress) {
    
    progressr::with_progress({
      
      p <- progressr::progressor(steps = n)
      
      future.apply::future_lapply(
        X = seq_along(simulation_names),
        FUN = function(i) {
          
          s_name <- simulation_names[[i]]
          
          p(sprintf("Reading %s (%d/%d)", s_name, i, n))
          
          run_paths <- kwb.utils::resolve(path_list, dir_target = s_name)
          
          if (!all(file.exists(c(run_paths$path_results_hdf5_element,
                                 run_paths$path_results_hdf5_flaeche)))) {
            if (isTRUE(debug)) {
              message(sprintf("Missing files for %s -> returning NULL", s_name))
            }
            return(NULL)
          }
          
          if (isTRUE(debug)) {
            message(sprintf("Reading HDF5 for %s (%s)",
                            s_name, run_paths$dir_target_output))
          }
          
          res_hdf5_element <- hdf5r::H5File$new(
            run_paths$path_results_hdf5_element, mode = "r"
          )
          res_hdf5_flaeche <- hdf5r::H5File$new(
            run_paths$path_results_hdf5_flaeche, mode = "r"
          )
          
          on.exit({
            try(res_hdf5_element$close_all(), silent = TRUE)
            try(res_hdf5_flaeche$close_all(), silent = TRUE)
          }, add = TRUE)
          
          list(
            element = list(
              meta = read_hdf5_scalars(
                res_hdf5_element[["Metainfo"]], numeric_only = FALSE
              ),
              rates = read_hdf5_timeseries(
                res_hdf5_element[["Raten"]]
              ),
              water_balance = read_hdf5_scalars(
                res_hdf5_element[["Wasserbilanz"]]
              ),
              states = read_hdf5_timeseries(
                res_hdf5_element[["Zustandsvariablen"]]
              )
            ),
            connected_area = list(
              meta = read_hdf5_scalars(
                res_hdf5_flaeche[["Metainfo"]], numeric_only = FALSE
              ),
              rates = read_hdf5_timeseries(
                res_hdf5_flaeche[["Raten"]]
              ),
              water_balance = read_hdf5_scalars(
                res_hdf5_flaeche[["Wasserbilanz"]]
              ),
              states = read_hdf5_timeseries(
                res_hdf5_flaeche[["Zustandsvariablen"]]
              )
            )
          )
        },
        future.seed = future_seed
      )
    })
    
  } else {
    
    future.apply::future_lapply(
      X = seq_along(simulation_names),
      FUN = function(i) {
        s_name <- simulation_names[[i]]
        
        run_paths <- kwb.utils::resolve(path_list, dir_target = s_name)
        
        if (!all(file.exists(c(run_paths$path_results_hdf5_element,
                               run_paths$path_results_hdf5_flaeche)))) {
          return(NULL)
        }
        
        res_hdf5_element <- hdf5r::H5File$new(
          run_paths$path_results_hdf5_element, mode = "r"
        )
        res_hdf5_flaeche <- hdf5r::H5File$new(
          run_paths$path_results_hdf5_flaeche, mode = "r"
        )
        
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
      },
      future.seed = future_seed
    )
  }
  
  stats::setNames(res_list, simulation_names)
}