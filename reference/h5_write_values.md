# Write (updated) values back into existing HDF5 datasets (robust for your hdf5r build)

- Scalars: write with required `args` (args=list() or args=1L fallback).

- 2-col TS (data.frame/tibble): expects Nx2 in R; writes as 2xN in HDF5
  (RAINDROP style), using explicit hyperslab args=list(1:2, 1:N) to
  avoid empty selections.

- If TS length changes and dataset maxdims blocks resize, the dataset is
  deleted via parent\$link_delete(name) and recreated with dims=2xN
  (fixed), then written. (No maxdims argument used; compatible with your
  create_dataset signature.)

## Usage

``` r
h5_write_values(
  h5,
  values,
  resize = TRUE,
  strict = TRUE,
  ts_cols = c("time", "value"),
  scalar_strategy = c("error", "first", "collapse"),
  collapse_sep = ";",
  ts_dtype = "double",
  verbose = FALSE
)
```

## Arguments

- h5:

  Open hdf5r::H5File.

- values:

  Named list; names are dataset paths (leading // allowed).

- resize:

  Logical. If TRUE tries set_extent() where possible.

- strict:

  Logical. If TRUE stop on first error else warn and continue.

- ts_cols:

  Character(2). Column names for TS (default time/value).

- scalar_strategy:

  "error"\|"first"\|"collapse".

- collapse_sep:

  Separator for collapse.

- ts_dtype:

  Either an H5T object or one of "double","float","integer","logical".

- verbose:

  Logical.

## Value

Invisibly, written paths.
