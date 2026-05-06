# Ensure that an HDF5 dataset exists (create if missing)

Creates missing groups along the path and then creates the dataset.
Designed for RAINDROP input files where missing scalar parameters can
crash the model definition reader.

## Usage

``` r
h5_ensure_dataset(h5, path, value, dtype = NULL, dims = NULL)
```

## Arguments

- h5:

  Open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md)
  (mode "a" or "r+").

- path:

  Absolute dataset path (e.g.
  "/Berechnungsparameter/Zeitschritt_Verschaltungen").

- value:

  Initial value to write after creation (scalar, vector, matrix, or
  2-col TS).

- dtype:

  Optional. Either an `H5T` object or a string
  ("double","integer","logical","character"). If NULL, inferred from
  `value`.

- dims:

  Optional integer vector. If NULL, inferred from `value`.

## Value

Invisibly TRUE
