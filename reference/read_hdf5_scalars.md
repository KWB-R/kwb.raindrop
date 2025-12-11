# Read scalar datasets from an HDF5 group

Reads all scalar datasets contained in an HDF5 group and returns them as
a tibble. By default, only numeric / integer scalars are returned. If
non-numeric scalars (e.g. strings) are present, they can either be
dropped with a warning or kept in a list-column.

## Usage

``` r
read_hdf5_scalars(group, numeric_only = TRUE)
```

## Arguments

- group:

  An
  [`hdf5r::H5Group`](http://hhoeflin.github.io/hdf5r/reference/H5Group-class.md)
  object. Direct children of this group are expected to be scalar
  datasets (i.e. `dataset.dims == 0`).

- numeric_only:

  Logical (default: `TRUE`). If `TRUE`, only numeric / integer scalars
  are returned (others are dropped with a warning). If `FALSE`, all
  scalars are returned in a list-column `value` together with a `type`
  column.

## Value

If `numeric_only = TRUE`: A tibble with columns

- variable:

  `character`. Dataset name within the group.

- value:

  `numeric`. Scalar value read from the dataset.

If `numeric_only = FALSE`: A tibble with columns

- variable:

  `character`. Dataset name within the group.

- value:

  `list`. Scalar values (numeric, integer, character, ...).

- type:

  `character`. First class of each value (e.g. `"numeric"`,
  `"character"`).

## Examples

``` r
if (FALSE) { # \dontrun{
  # Wasserbilanz: nur numerische Skalare
  wb_tbl <- read_hdf5_scalars(res_hdf5_element[["Wasserbilanz"]])

  # Metainfo: gemischte Typen (Integer + String)
  meta_tbl <- read_hdf5_scalars(res_hdf5_element[["Metainfo"]],
                                numeric_only = FALSE)
} # }
```
