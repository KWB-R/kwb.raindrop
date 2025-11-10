# Read values of all (or selected) datasets

Read values of all (or selected) datasets

## Usage

``` r
h5_read_values(
  h5,
  paths = NULL,
  simplify_scalars = TRUE,
  timeseries_as_tibble = TRUE,
  ts_names = c("time", "value")
)
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md)

- paths:

  character vector; if NULL read all

- simplify_scalars:

  logical: simplify H5S_SCALAR to length-1 atom

- timeseries_as_tibble:

  logical: convert 2xN / Nx2 to tibble(time,value)

- ts_names:

  character(2): names for time/value columns

## Value

named list
