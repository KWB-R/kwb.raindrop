# Read RAINDROP error logs into a nested tibble

Reads the RAINDROP error protocol HDF5 (typically `Fehlerprotokoll.h5`)
for a set of simulation runs and returns a tibble with one row per run
and a nested tibble column containing all errors found in the file.

## Usage

``` r
read_raindrop_errors(simulation_names, path_list, debug = FALSE)
```

## Arguments

- simulation_names:

  Character vector of simulation run names (e.g. `"s00001"`).

- path_list:

  A path list object passed to
  [`kwb.utils::resolve()`](https://rdrr.io/pkg/kwb.utils/man/resolve.html).
  Must resolve to an element `path_errors_hdf5`.

- debug:

  Logical. If `TRUE`, pass through to
  [`kwb.utils::catAndRun()`](https://rdrr.io/pkg/kwb.utils/man/catAndRun.html).

## Value

A tibble with columns:

- `id` Integer simulation id (parsed from `simulation_names`).

- `scenario_name` The simulation name (e.g. `"s00001"`).

- `path` File path to the error HDF5.

- `file_exists` Logical.

- `number_of_errors` Scalar integer or `NA`.

- `errors` Nested tibble (list-column) with per-error rows:
  `error_index`, `Fehlerbeschreibung`, `Layer1`, `Layer2`,
  `Massnahmenelement1`, `Massnahmenelement2`

## Details

The error HDF5 usually contains:

- `/AnzahlFehler` (scalar integer)

- groups `/0`, `/1`, ... each containing datasets such as
  `Fehlerbeschreibung`, `Layer1`, `Layer2`, `Massnahmenelement1`,
  `Massnahmenelement2`

Note: In hdf5r, HDF5 group names must be addressed as character strings.
So group `0` must be accessed as `h5[["0"]]` (not `h5[[0]]`).

## Examples

``` r
if (FALSE) { # \dontrun{
errors_df <- read_raindrop_errors(simulation_names, path_list)

# flatten all errors:
all_errors <- tidyr::unnest(errors_df, errors)
} # }
```
