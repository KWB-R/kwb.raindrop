# Run scenarios (parallel or sequential) with a user-supplied worker function

Executes scenarios by applying `run_one_scenario` to each element of
`indices`. Supports parallel execution via future.apply and sequential
execution for debugging. Optionally shows progress via progressr.

## Usage

``` r
run_scenarios(
  indices,
  run_one_scenario,
  timestep_hours,
  debug = FALSE,
  ...,
  parallel = TRUE,
  workers = parallel::detectCores(),
  show_progress = TRUE,
  progress_handler = "txtprogressbar"
)
```

## Arguments

- indices:

  Vector. Scenario identifiers to iterate over (often integer row
  indices).

- run_one_scenario:

  Function. Worker function with signature
  `function(x, timestep_hours, debug, ...)`. Must accept the arguments
  `timestep_hours` and `debug`.

- timestep_hours:

  Numeric. Time step (hours) forwarded to `run_one_scenario`.

- debug:

  Logical. Debug flag forwarded to `run_one_scenario`.

- ...:

  Additional arguments forwarded to `run_one_scenario`.

- parallel:

  Logical. If `TRUE`, use
  [`future.apply::future_lapply`](https://future.apply.futureverse.org/reference/future_lapply.html).
  If `FALSE`, use base `lapply`.

- workers:

  Integer. Number of workers when `parallel = TRUE`. Defaults to
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).

- show_progress:

  Logical. If `TRUE`, show progress.

- progress_handler:

  Character. Progress handler key. One of `"txtprogressbar"`,
  `"rstudio"`, `"cli"`.

## Value

A list with one element per `indices` entry containing the return values
of `run_one_scenario`.
