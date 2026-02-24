#' Run scenarios (parallel or sequential) with a user-supplied worker function
#'
#' Executes scenarios by applying \code{run_one_scenario} to each element of
#' \code{indices}. Supports parallel execution via \pkg{future.apply} and
#' sequential execution for debugging. Optionally shows progress via
#' \pkg{progressr}.
#'
#' @param indices Vector. Scenario identifiers to iterate over (often integer
#'   row indices).
#' @param run_one_scenario Function. Worker function with signature
#'   \code{function(x, timestep_hours, debug, ...)}. Must accept the arguments
#'   \code{timestep_hours} and \code{debug}.
#' @param timestep_hours Numeric. Time step (hours) forwarded to
#'   \code{run_one_scenario}.
#' @param debug Logical. Debug flag forwarded to \code{run_one_scenario}.
#' @param ... Additional arguments forwarded to \code{run_one_scenario}.
#' @param parallel Logical. If \code{TRUE}, use \code{future.apply::future_lapply}.
#'   If \code{FALSE}, use base \code{lapply}.
#' @param workers Integer. Number of workers when \code{parallel = TRUE}.
#'   Defaults to \code{parallel::detectCores()}.
#' @param show_progress Logical. If \code{TRUE}, show progress.
#' @param progress_handler Character. Progress handler key. One of
#'   \code{"txtprogressbar"}, \code{"rstudio"}, \code{"cli"}.
#'
#' @return A list with one element per \code{indices} entry containing the
#'   return values of \code{run_one_scenario}.
#'
#' @importFrom future plan multisession sequential
#' @importFrom future.apply future_lapply
#' @importFrom parallel detectCores
#' @importFrom progressr with_progress progressor handler_txtprogressbar handler_rstudio handler_cli
#' @export
run_scenarios <- function(indices,
                          run_one_scenario,
                          timestep_hours,
                          debug = FALSE,
                          ...,
                          parallel = TRUE,
                          workers = parallel::detectCores(),
                          show_progress = TRUE,
                          progress_handler = "txtprogressbar") {
  
  stopifnot(is.function(run_one_scenario))
  stopifnot(is.numeric(timestep_hours), length(timestep_hours) == 1L, is.finite(timestep_hours))
  stopifnot(is.logical(debug), length(debug) == 1L)
  
  if (parallel) {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::sequential)
  }
  
  apply_fun <- if (parallel) future.apply::future_lapply else lapply
  n <- length(indices)
  
  # map string -> handler function (LOCAL, no global handlers!)
  handler_fun <- switch(
    progress_handler,
    "txtprogressbar" = progressr::handler_txtprogressbar,
    "rstudio"        = progressr::handler_rstudio,
    "cli"            = progressr::handler_cli,
    stop(sprintf("Unknown progress_handler: '%s' (use 'txtprogressbar', 'rstudio', or 'cli')",
                 progress_handler))
  )
  
  if (show_progress && parallel) {
    
    progressr::with_progress(
      expr = {
        p <- progressr::progressor(steps = n)
        
        apply_fun(seq_along(indices), function(k) {
          x <- indices[[k]]
          p(sprintf("Scenario %d/%d", k, n))
          run_one_scenario(x, timestep_hours = timestep_hours, debug = debug, ...)
        })
      },
      handlers = list(handler_fun())  # <-- IMPORTANT
    )
    
  } else {
    
    apply_fun(seq_along(indices), function(k) {
      x <- indices[[k]]
      if (show_progress) message(sprintf("Running %d/%d", k, n))
      run_one_scenario(x, timestep_hours = timestep_hours, debug = debug, ...)
    })
  }
}