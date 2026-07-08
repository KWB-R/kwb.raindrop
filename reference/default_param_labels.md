# German / English labels for optimisation parameter-grid columns

Maps the raw `param_grid` column names produced by the case-study
workflows to human-readable, unit-carrying labels. Used to translate the
"varying parameters" block in the interactive tooltips of
[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
and
[`plot_cost_overflow_boxplot()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_overflow_boxplot.md),
so a hovered point shows e.g. `Muldenflaeche [m2]=125` instead of the
raw `mulde_area=125`.

## Usage

``` r
default_param_labels(lang = c("de", "en"))
```

## Arguments

- lang:

  Character. `"de"` or `"en"`.

## Value

A named `character` vector: names are `param_grid` column names, values
are the display labels.

## Details

Unknown columns fall back to their raw name, so a grid gaining a new
column still renders (just untranslated). Override individual entries or
pass your own named vector via the `param_labels` argument of the plot
functions.

## Examples

``` r
default_param_labels("de")[["mulde_area"]]
#> [1] "Muldenfläche [m²]"
default_param_labels("en")[["storage_height"]]
#> [1] "Storage thickness [mm]"
```
