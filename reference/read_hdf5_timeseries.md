# Read HDF5 Results Time Series from HDF5 Group into a Long Tibble

Extracts all datasets (no subgroups) from a given HDF5 group and
converts each **2×N numeric matrix** into a tidy long table. It assumes
the first row holds the time/index vector and the second row the values.
Datasets that are not 2×N numeric matrices (e.g. scalar metadata like
`von_Layer`, `von_Massnahmenelement`, ...) are silently ignored.

## Usage

``` r
read_hdf5_timeseries(ts_groupvariable)
```

## Arguments

- ts_groupvariable:

  [`hdf5r::H5Group`](http://hhoeflin.github.io/hdf5r/reference/H5Group-class.md)
  An open HDF5 group whose *time-series* children are stored as 2×N
  numeric matrices (`[1, ] = time/index`, `[2, ] = value`), possibly
  mixed with scalar metadata datasets.

## Value

A `tibble` with columns:

- `variable` (`character`): dataset name within the group.

- `time` (`numeric`): time or index taken from the first row.

- `value` (`numeric`): values taken from the second row.
