# Caption line naming the unit-cost rates behind the cost plots

Formats the unit-cost rates (EUR per m2 / m3, see
[`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md))
as a single-line caption for the cost plots, so every figure names the
rates its EUR values were computed with. Used as the default `caption`
of
[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md),
[`plot_cost_vs_evaporation()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_evaporation.md)
and
[`plot_cost_overflow_boxplot()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_overflow_boxplot.md)
(rendered by ggplot at the bottom of the PDFs) and passed to
[`plotly_add_caption()`](https://kwb-r.github.io/kwb.raindrop/reference/plotly_add_caption.md)
for the interactive HTMLs
([`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
drops ggplot captions).

## Usage

``` r
cost_rates_caption(lang = c("de", "en"), cost_rates = default_cost_rates())
```

## Arguments

- lang:

  Character. `"de"` or `"en"`.

- cost_rates:

  `list` of unit costs as returned by
  [`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md).

## Value

`character(1)`, a single line.

## Details

If the costs were computed with non-default rates, pass the same
`cost_rates` list here so the caption matches the numbers.

## Examples

``` r
cost_rates_caption("de")
#> [1] "Kostensätze (inkl. Einbau): Aushub 70 €/m³ · Profilierung + Begrünung 10 €/m² · Bodenfilter 200 €/m³ · Sickerbox 350 €/m³ · Schotterrigol 50 €/m³"
```
