# Validate what would be written where (pre-flight check)

Summarizes for each path:

- current dataset dims/maxdims (HDF5 order)

- intended value shape (R order)

- intended write shape (HDF5 order; for TS always 2xN)

- decision: SKIP / WRITE / RESIZE / NEED_RECREATE / ERROR

## Usage

``` r
h5_validate_write(h5, values, ts_cols = c("time", "value"))
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md).

- values:

  Named list of values to write (names are HDF5 paths).

- ts_cols:

  Character(2). Column names for TS (default time/value).

## Value

tibble

## Details

Notes:

- 2-column data.frames/tibbles are treated as Nx2 in R and mapped to 2xN
  in HDF5.

- Many RAINDROP files store time series as HDF5 dims 2xN (appearing as
  Nx2 in R).
