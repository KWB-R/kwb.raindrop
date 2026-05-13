# Read Raindrop optimisation simulation results from HDF5

Reads per-run result files (HDF5) for both the *measure element* and the
*connected area* results and returns a named list (one entry per
simulation).

## Usage

``` r
get_simulation_results_optim(paths, path_list, simulation_names, debug = TRUE)
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

## Value

A named list with one entry per `simulation_names`. Each entry is either
`NULL` (element HDF5 missing) or a nested list:

- element:

  meta

  :   Data.frame/list of scalar metadata (from `Metainfo`).

  rates

  :   Time series table (from `Raten`).

  water_balance

  :   Scalars table (from `Wasserbilanz`).

  states

  :   Time series table (from `Zustandsvariablen`).

- connected_area:

  Same structure as `element`, read from the connected-area HDF5, or
  `NULL` if that file is missing for the run.

## Details

For each simulation name (e.g. `"s00001"`), the function resolves the
run directory via `kwb.utils::resolve(path_list, dir_target = s_name)`
and then loads standard result groups from two HDF5 files:

- **element**: `Metainfo`, `Raten`, `Wasserbilanz`, `Zustandsvariablen`

- **connected_area**: `Metainfo`, `Raten`, `Wasserbilanz`,
  `Zustandsvariablen`

If the **element** HDF5 is missing the entry is `NULL`; if only the
**connected-area** HDF5 is missing, the entry is a list with the
`element` side populated and `connected_area = NULL`. HDF5 handles are
closed on exit via `on.exit(...$close_all())`.

The function uses `hdf5r::H5File$new(..., mode = "r")` to open the files
and registers an `on.exit(...$close_all())` so handles are released even
when the iteration body errors out.

## See also

[`resolve`](https://rdrr.io/pkg/kwb.utils/man/resolve.html),
[`read_hdf5_scalars`](https://kwb-r.github.io/kwb.raindrop/reference/read_hdf5_scalars.md),
[`read_hdf5_timeseries`](https://kwb-r.github.io/kwb.raindrop/reference/read_hdf5_timeseries.md)
