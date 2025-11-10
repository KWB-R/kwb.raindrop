# List all datasets (recursive)

List all datasets (recursive)

## Usage

``` r
list_h5_datasets(h5)
```

## Arguments

- h5:

  An open
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md).

## Value

A tibble with columns: `path`, `obj_type`, `dims`, `maxdims`.

## Examples

``` r
if (FALSE) { # \dontrun{
h5 <- hdf5r::H5File$new("file.h5", mode = "r")
list_h5_datasets(h5)
} # }
```
