# Read Raindrop optimisation simulation results from HDF5 (parallel via future.apply + progress)

Parallel variant of
[`get_simulation_results_optim()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim.md)
using
[`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)
including optional progress reporting via progressr.

## Usage

``` r
get_simulation_results_optim_parallel(
  paths,
  path_list,
  simulation_names,
  debug = TRUE,
  workers = NULL,
  show_progress = TRUE,
  future_seed = TRUE,
  progress_handler = progressr::handler_txtprogressbar
)
```

## Arguments

- paths:

  A list of path definitions. Used for messaging and expected to contain
  `file_results_hdf5_element` and `file_results_hdf5_flaeche` (file
  names). Note: within the loop, `paths` is overwritten by the resolved
  paths for the current simulation run.

- path_list:

  A list passed to
  [`kwb.utils::resolve()`](https://rdrr.io/pkg/kwb.utils/man/resolve.html)
  to generate run-specific paths (must yield
  `path_results_hdf5_element`, `path_results_hdf5_flaeche`, and
  `dir_target_output`).

- simulation_names:

  Character vector of simulation run identifiers (e.g.
  `c("s00001", "s00002")`).

- debug:

  print debug messages (default: TRUE)

- workers:

  Optional number of parallel workers. If not NULL, a temporary
  [`future::multisession`](https://future.futureverse.org/reference/multisession.html)
  plan is set.

- show_progress:

  Logical (default TRUE).

- future_seed:

  Passed to
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html).

- progress_handler:

  A progressr handler function (default:
  [`progressr::handler_txtprogressbar`](https://progressr.futureverse.org/reference/handler_txtprogressbar.html)).
  If NULL, no handler is set.

## Value

Named list (see
[`get_simulation_results_optim`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim.md)).
