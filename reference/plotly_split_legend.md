# Split the combined (colour, shape) ggplotly legend into two clean legends

[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
flattens a ggplot with both a colour and a shape aesthetic into one
trace per (colour, shape) combination and names the legend entries as
tuples such as `"(0,Sickerbox / Infiltration box)"` – with two storage
types and the 0..x / "\>x" overflow palette that yields an unreadable
legend. This helper post-processes the plotly object:

## Usage

``` r
plotly_split_legend(
  pl,
  lang = c("de", "en"),
  colour_title = NULL,
  shape_title = NULL,
  add_shape_legend = TRUE
)
```

## Arguments

- pl:

  A plotly object as returned by
  `plotly::ggplotly(p, tooltip = "text")`.

- lang:

  Character. `"de"` or `"en"`; sets the default legend group titles.

- colour_title:

  Character or `NULL`. Title of the colour legend group. Defaults to the
  language-specific "Number of overflow events".

- shape_title:

  Character or `NULL`. Title of the storage-type legend group. Defaults
  to the language-specific "Storage type".

- add_shape_legend:

  Logical. If `TRUE` (default), append the two legend-only storage-type
  entries.

## Value

The modified plotly object.

## Details

- the real traces lose their legend entries; instead every overflow
  class gets one legend-only key drawn as a **neutral circle in the
  class colour** (a coloured square or triangle would wrongly suggest
  one specific storage type). The key shares its legend group with the
  real traces of that class, so clicking it toggles **both** storage
  types of the class together;

- two legend-only keys (**neutral grey** filled square = infiltration
  box, filled triangle = gravel trench) are appended under their own
  **storage-type group title**, so the shape encoding is explained
  separately from the colours – set `add_shape_legend = FALSE` to skip
  them (e.g. for storage-type-faceted plots whose strips already label
  the panels). The keys are **clickable**: since a plotly trace can only
  carry one legend group (taken by the overflow class), a small
  JavaScript handler (via
  [`htmlwidgets::onRender()`](https://rdrr.io/pkg/htmlwidgets/man/onRender.html))
  toggles all traces drawn with that marker symbol, so each storage type
  can be shown or hidden individually; the key greys out to reflect the
  state;

- the combined `"colour,shape"` legend-title annotation that ggplotly
  draws over the plot title is removed; group titles take its place and
  the legend moves to a vertical layout on the right, where the groups
  stack cleanly.

Traces whose name is not a `"(colour,shape)"` tuple (frontier lines,
best markers, single-aesthetic plots) are left untouched, so the helper
is safe to apply to any of the package's interactive plots.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- plot_cost_vs_evaporation(sim_results, param_grid, x = 5, lang = "de")
pl <- plotly::ggplotly(p, tooltip = "text")
pl <- plotly_split_legend(pl, lang = "de")
} # }
```
