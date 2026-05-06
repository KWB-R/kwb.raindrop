# Ensure that all datasets referenced by a values list exist

Creates missing groups and datasets for all names(values). Datasets are
created with dtype/dims inferred from the corresponding value. No values
are written here.

## Usage

``` r
h5_ensure_datasets_from_values(
  h5,
  values,
  ts_cols = c("time", "value"),
  ts_layout = c("2xN", "Nx2"),
  ts_dtype = "double",
  strict = TRUE
)
```

## Arguments

- h5:

  Open hdf5r::H5File (mode "a" or "r+").

- values:

  Named list; names are dataset paths (leading // allowed).

- ts_cols:

  Character(2). Column names for TS (default time/value).

- ts_layout:

  One of "2xN" or "Nx2". For RAINDROP typically "2xN" in HDF5.

- ts_dtype:

  Either an H5T object or a string
  ("double","integer","logical","float").

- strict:

  Stop if creation fails.

## Value

Invisibly, the character vector of created dataset paths.
