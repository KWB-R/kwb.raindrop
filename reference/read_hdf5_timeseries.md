# Read HDF5 time series datasets from a group (supports deeperLayers)

Reads all datasets in an HDF5 group and returns a long tibble with
columns: variable, time, value.

## Usage

``` r
read_hdf5_timeseries(
  ts_groupvariable,
  deeper_layers_pattern = "deeperLayers|deeper_layers"
)
```

## Arguments

- ts_groupvariable:

  [`hdf5r::H5Group`](http://hhoeflin.github.io/hdf5r/reference/H5Group-class.md)

- deeper_layers_pattern:

  regex to detect deeper-layers datasets

## Value

tibble::tibble(variable, time, value)

## Details

Supported dataset layouts:

- k x N (rows): 1, = time, 2..k, = values (series)

- N x k (cols): , 1 = time, , 2..k = values (series)

Special handling for names containing "deeperLayers"/"deeper_layers":

- The value series represent layers below layer 1.

- Output variable names get suffixed with the layer-id: \_2, \_3, ...

- If the dataset contains only time (no value series), it returns 0
  rows.
