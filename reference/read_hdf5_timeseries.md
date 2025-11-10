# Read HDF5 Results Time Series from HDF5 Group into a Long Tibble

Extracts all **datasets** (no subgroups) from a given HDF5 group and
converts each 2×N matrix into a tidy long table. It assumes the first
row holds the time/index vector and the second row the values.

## Usage

``` r
read_hdf5_timeseries(ts_groupvariable)
```

## Arguments

- ts_groupvariable:

  [`hdf5r::H5Group`](http://hhoeflin.github.io/hdf5r/reference/H5Group-class.md)
  An open HDF5 group whose children are time-series datasets stored as
  2×N numeric matrices (`[1, ] = time/index`, `[2, ] = value`).
  Typically a group like `"/Zustandsvariablen"` or similar in your model
  output file.

## Value

A `tibble` with columns:

- `variable` (`character`): dataset name within the group.

- `time` (`numeric`): time or index taken from the first row.

- `value` (`numeric`): values taken from the second row.

## Details

The function lists all child objects of `ts_groupvariable`, filters for
datasets (`H5I_DATASET`), reads each dataset into memory, and stacks
them into one long tibble. Datasets are expected to be **2×N**; if your
storage differs (e.g., `N×2` or 1D), adapt the reading logic
accordingly.

## Assumptions

- Each dataset under `ts_groupvariable` is a numeric matrix of shape
  2×N.

- Row 1 is the time/index vector; row 2 contains the values.

## See also

[`H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md),
[`H5Group`](http://hhoeflin.github.io/hdf5r/reference/H5Group-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(hdf5r)
f <- H5File$new("Optimierung_MuldenRigole.h5", mode = "r")
grp <- f[["Zustandsvariablen"]]
ts_long <- read_hdf5_timeseries(grp)
head(ts_long)
f$close_all()
} # }
```
