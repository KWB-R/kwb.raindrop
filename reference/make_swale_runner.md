# Create a site-specific single-scenario runner for the optimiser

Factors the `run_one()` function that was duplicated across the three
workflow vignettes (Eisenstadt 2005, Wien, Bad Aussee) into one
package-level closure factory. The returned function runs the RAINDROP
engine for one parameter set and returns the thinned one-row
optimisation result (overflow events + water balance), augmented with
the input parameters and the overflow volume in m3.

## Usage

``` r
make_swale_runner(
  path_list,
  timestep_hours = 0.1,
  timeseries_rain = NULL,
  timeseries_et = NULL,
  storage_types = default_storage_types(),
  event_separation_hours = 4,
  scenario_prefix = "o",
  cleanup = TRUE,
  debug = FALSE
)
```

## Arguments

- path_list:

  Path definition list as used by the workflow vignettes (resolvable
  with
  [`kwb.utils::resolve()`](https://rdrr.io/pkg/kwb.utils/man/resolve.html),
  must contain `path_base`, `path_exe`, `dir_input`, `dir_output`,
  `dir_target_output`, `path_target_input`, `path_results_hdf5_element`,
  `path_results_hdf5_flaeche`, `file_target`).

- timestep_hours:

  Engine time step in hours (default 0.1).

- timeseries_rain:

  Optional data.frame `time`/`value` (mm/h) written to `//Kurven/Regen`
  (the dataset must exist in `base.h5`); when given, the
  `//Kurven/Growth_1` and `//Kurven/Shading_1` end times are extended to
  the rain series end (skipped for templates without these curves) and
  `rain_factor` is ignored. Without `timeseries_rain`, a per-run
  `rain_factor != 1` requires `//Kurven/Regen` to exist as a time series
  in `base.h5` – a clear error is thrown otherwise.

- timeseries_et:

  Optional data.frame `time`/`value` (mm/h) written to `//Kurven/ET0`.

- storage_types:

  Soil presets of the storage layer per storage type, see
  [`default_storage_types()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_types.md).

- event_separation_hours:

  Event separation for overflow counting (default 4, as in the vignettes
  and the monotonicity analysis).

- scenario_prefix:

  Prefix for generated scenario names (default `"o"` -\> `o00001`,
  `o00002`, ... – distinct from the grid runs `s00001` ...).

- cleanup:

  Delete each scenario's copied input file and output directory right
  after the thinned one-row result has been read (default `TRUE`). The
  optimisers only need that row; without the cleanup an optimisation run
  (hundreds of engine runs per task, each with its own copy of `base.h5`
  plus all output HDF5s) fills the temp drive and the engine aborts with
  HDF5 `errno = 28` ("No space left on device"). Set `FALSE` to keep all
  scenario files for debugging. Files of a *failed* run are always kept.

- debug:

  Passed on to the engine/reader helpers.

## Value

`function(params)` where `params` is a named list (or one-row
data.frame) with `mulde_area`, `mulde_height` (mm), `storage_type`,
`storage_height` (mm), `connected_area` (m2), `filter_height` (mm),
`filter_hydraulicconductivity` (mm/h), `bottom_hydraulicconductivity`
(mm/h) and optionally `rain_factor` (default 1) and `lai` (default 3.9).
It returns a one-row tibble with the parameters, the scenario name and
the optimisation metrics (`n_overflows`, `sum_overflows` in mm,
`overflow_volume_m3`, water-balance shares).

## Details

Site differences are covered by the arguments: Eisenstadt scales the
rain curve shipped in `base.h5` by `rain_factor` (leave
`timeseries_rain` = `NULL`), Wien and Bad Aussee replace the rain and
ET0 curves entirely (`timeseries_rain` / `timeseries_et`, values in mm/h
as written by the vignettes).

On the first call the runner prepares a **site master file** once:
`base.h5` plus everything identical for every run (calculation settings,
ET/rain time series). Each run then copies the master and writes only
its ~15 small parameter datasets. Compared to the previous full
read/rewrite of *all* datasets per run this removes the dominant per-run
overhead of the optimisation searches (hundreds of runs; for Wien / Bad
Aussee it skips rewriting the 15-year rain series on every single engine
run).

## See also

[`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md),
[`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md)
