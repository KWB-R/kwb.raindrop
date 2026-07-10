# Add a caption annotation to a ggplotly object

[`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
drops ggplot captions (and subtitles). This helper re-adds the caption
as a small grey annotation below the plot area (bottom left, under the
x-axis title) and widens the bottom margin accordingly. `\n` line breaks
are converted to `<br />`.

## Usage

``` r
plotly_add_caption(pl, caption, font_size = 10)
```

## Arguments

- pl:

  A plotly object as returned by
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html).

- caption:

  Character. The caption text; `NULL` or `""` returns `pl` unchanged.

- font_size:

  Numeric. Caption font size in px. Default 10.

## Value

The modified plotly object.

## Details

Used by the vignettes together with
[`cost_rates_caption()`](https://kwb-r.github.io/kwb.raindrop/reference/cost_rates_caption.md)
so the interactive cost plots name the unit-cost rates they were
computed with.

## Examples

``` r
if (FALSE) { # \dontrun{
pl <- plotly::ggplotly(p, tooltip = "text")
pl <- plotly_add_caption(pl, cost_rates_caption("de"))
} # }
```
