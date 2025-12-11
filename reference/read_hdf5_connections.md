# Read surface-water connections from an HDF5 results file

Reads an HDF5 file that contains surface-water *connections* (formerly
"Verschaltungen"), e.g. a file like `s78853_Verschaltungen.h5`. The file
is expected to have:

- a root-level scalar dataset `"Anzahl Verschaltungen"`, and

- one group per connection (`"0"`, `"1"`, ...) that contains

  - scalar metadata (e.g. `von_Layer`, `von_Massnahmenelement`,
    `nach_Layer`, `nach_Massnahmenelement`) and

  - time-series datasets stored as 2×N matrices (e.g. `Qtatsaechlich`,
    `delta_h_pond_*`, `delta_theta_*`).

Scalar metadata are returned

- als breite Tabelle (`meta`),

- als lange Tabelle (`scalars`) und

- pro Verbindung in einer Unterliste (`connections$group_0`, ...).

Time series werden in Long-Format zurückgegeben und mit den Metadaten
verknüpft.

## Usage

``` r
read_hdf5_connections(file)
```

## Arguments

- file:

  An
  [`hdf5r::H5File`](http://hhoeflin.github.io/hdf5r/reference/H5File-class.md)
  object pointing to a `*_Verschaltungen.h5` file, already opened in
  read mode.

## Value

A named list with components:

- n_connections:

  `integer(1)`. Value of the root dataset `"Anzahl Verschaltungen"` (or
  `NA_integer_` if not present).

- meta:

  A tibble with one row per connection: `connection_id`, `from_layer`,
  `from_element`, `to_layer`, `to_element`.

- timeseries:

  A tibble with long-format time series for all connections:
  `connection_id`, `variable`, `time`, `value`, `from_layer`,
  `from_element`, `to_layer`, `to_element`.

- scalars:

  A tibble with all scalar datasets per connection in long format:
  `connection_id`, `variable`, `value` (list), `type`.

- connections:

  A named list of per-connection sublists: `connections$group_0`,
  `connections$group_1`, ...; each containing `connection_id`, `meta`,
  `timeseries`, `scalars` für genau diese Verbindung.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(hdf5r)

  f <- H5File$new(paths$path_verschaltungen_hdf5, mode = "r")
  conn <- read_hdf5_connections(f)

  conn$n_connections
  conn$meta
  conn$timeseries

  # Unterliste für die erste Verbindung:
  conn$connections$group_0$meta
  conn$connections$group_0$timeseries
} # }
```
