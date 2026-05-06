# Plot main effects of multiple parameters on an outcome (violin/box/jitter)

Creates a facetted overview plot showing the distribution of an outcome
(for example `n_overflows`) across the tested levels of multiple varied
parameters. Parameters are sorted by a simple effect-size proxy: the
range of median outcome values across parameter levels.

## Usage

``` r
plot_main_effects(
  df,
  y = "n_overflows",
  params,
  max_levels = 25,
  ylim_lower = 0,
  lang = c("de", "en")
)
```

## Arguments

- df:

  A data.frame or tibble containing the outcome column `y` and the
  parameter columns listed in `params`.

- y:

  Character scalar. Name of the outcome column to plot on the y-axis.
  Defaults to `"n_overflows"`.

- params:

  Character vector of parameter column names in `df` to consider.

- max_levels:

  Integer. Parameters with more than `max_levels` distinct values are
  dropped to keep the plot readable. Defaults to 25.

- ylim_lower:

  Numeric scalar or `NULL`. Optional lower display limit for the y-axis.
  Uses
  [`ggplot2::coord_cartesian()`](https://ggplot2.tidyverse.org/reference/coord_cartesian.html),
  so data are not removed before computing violin and boxplots. Defaults
  to `0`.

- lang:

  Character. Plot language: `"de"` or `"en"`.

## Value

A ggplot object with facetted violin, boxplot, and jitter layers.

## Details

The function is intended for optimisation or sensitivity grids with many
parameters, where a single 2D scatter plot is not informative.

The plot language can be switched via `lang = "de"` or `lang = "en"`.
This affects the title, y-axis label, and selected parameter labels.
