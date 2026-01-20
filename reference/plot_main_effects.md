# Plot main effects of multiple parameters on an outcome (violin/box/jitter)

Creates a facetted overview plot showing the distribution of an outcome
(e.g., `n_overflows`) across the tested levels of multiple (varied)
parameters. Parameters are sorted by a simple effect-size proxy: the
range of median outcome values across parameter levels.

## Usage

``` r
plot_main_effects(df, y = "n_overflows", params, max_levels = 25)
```

## Arguments

- df:

  A data.frame (or tibble) containing the outcome column `y` and the
  parameter columns listed in `params`.

- y:

  Character scalar. Name of the outcome column to plot on the y-axis.
  Defaults to `"n_overflows"`.

- params:

  Character vector of parameter column names in `df` to consider.

- max_levels:

  Integer. Parameters with more than `max_levels` distinct values are
  dropped to keep the plot readable. Defaults to 25.

## Value

A ggplot object (facetted violin + boxplot + jitter).

## Details

The function is intended for optimisation / sensitivity grids with many
parameters, where a single 2D scatter plot is not informative.

## Examples

``` r
if (FALSE) { # \dontrun{
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

df <- read_csv("simulation_results_optimisation.csv", show_col_types = FALSE)
params <- c("connected_area", "mulde_area", "mulde_height",
            "filter_hydraulicconductivity", "filter_height",
            "storage_height", "bottom_hydraulicconductivity", "rain_factor")

p <- plot_main_effects(df, y = "n_overflows", params = params, max_levels = 20)
p
} # }
```
