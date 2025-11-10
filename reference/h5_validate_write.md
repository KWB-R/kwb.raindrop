# Validate what would be written where (pre-flight check)

Checks a named list of values against an open HDF5 file and summarizes,
per dataset path, the current dimensions (with a fallback via
`h5$ls()`), the length/column count of the supplied value, and a
suggested handling (`SCALAR`/`1D`/`ND` or `TS(2-col)` for 2-column time
series).

## Usage

``` r
h5_validate_write(h5, values)
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md)
  (e.g., opened with `mode = "r+"`).

- values:

  A named `list`. Names are absolute dataset paths (starting with
  `"/"`); values are the R objects to be written (scalar, vector,
  matrix/array, or a 2-column `data.frame`/`tibble` for time series).

## Value

A `tibble` with columns:

- `path` – dataset path (after normalizing `"//"` → `"/"`).

- `cur_dims` – detected dims as `"2x4"` or `"SCALAR"`.

- `val_len` – length of the supplied value (for data frames: `nrow`).

- `df_cols` – number of columns (for data frames/tibbles; otherwise
  `NA`).

- `decision` – heuristic label: `"SCALAR"`, `"1D"`, `"ND"`, or
  `"TS(2-col)"`.

- `note` – additional note (e.g., `"not found"`).

## Details

If `get_simple_extent_dims()` incorrectly reports a dataset as *SCALAR*,
the function falls back to dimensions derived from
`list_h5_datasets(h5)` (i.e., `h5$ls(recursive = TRUE)`). Names in
`values` are normalized so that multiple leading slashes collapse to a
single leading slash.

## See also

[`list_h5_datasets`](https://kwb-r.github.io/kwb.raindrop/reference/list_h5_datasets.md),
`hdf5r`, and a matching writer like
[`h5_write_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_write_values.md).

## Examples

``` r
if (FALSE) { # \dontrun{
library(hdf5r)
h5 <- H5File$new("input.h5", mode = "r+")

vals <- list(
  "/Parameters/OutputPath" = "C:/temp/out.h5",
  "/Rain/Hyetograph" = tibble::tibble(time = c(0, 10, 20, 30),
                                      value = c(0, 5, 12, 0)),
  "/Measures/.../LayerThickness" = c(150L, 150L)
)

h5_validate_write(h5, vals)
} # }
```
