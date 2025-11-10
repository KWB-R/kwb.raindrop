# Write (updated) values back into existing HDF5 datasets (robust)

Writes scalars, vectors, matrices/arrays, and 2-column data
frames/tibbles (treated as time series) into existing HDF5 datasets. If
the dataset reports SCALAR incorrectly, the function can infer target
dimensions from the supplied value and resize accordingly.

## Usage

``` r
h5_write_values(
  h5,
  values,
  resize = TRUE,
  strict = TRUE,
  prefer_rows = NA,
  ts_cols = c("time", "value"),
  scalar_strategy = c("error", "first", "collapse"),
  collapse_sep = ";",
  verbose = FALSE
)
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md)
  (e.g., `mode = "r+"`).

- values:

  Named `list`: names are absolute dataset paths, values are R objects
  to write.

- resize:

  Logical. If `TRUE`, resize datasets via `set_extent()` when shapes
  differ.

- strict:

  Logical. If `TRUE`, stop on first error; otherwise warn and skip.

- prefer_rows:

  Logical(1) or `NA`. For 2-column time series: `NA` keeps dataset
  orientation (2xN if first dim == 2), `TRUE` forces 2xN, `FALSE` forces
  Nx2.

- ts_cols:

  Character(2). Column names to pull from time-series data frames
  (default `c("time","value")`).

- scalar_strategy:

  One of `"error"`, `"first"`, `"collapse"`. Controls how non-length-1
  values are handled for true SCALAR datasets.

- collapse_sep:

  Character. Separator used when `scalar_strategy = "collapse"`.

- verbose:

  Logical. If `TRUE`, prints per-path dimension info.

## Value

Invisibly returns the character vector of written dataset paths.

## Examples

``` r
if (FALSE) { # \dontrun{
vals <- h5_read_values(h5)
vals[["/Parameters/OutputPath"]] <- "C:/temp/out.h5"
h5_write_values(h5, vals, resize = TRUE, scalar_strategy = "first", verbose = TRUE)
} # }
```
