# Plot the influence of single-parameter variations on a response

Uses
[`find_single_param_variations()`](https://kwb-r.github.io/kwb.raindrop/reference/find_single_param_variations.md)
(by default excluding `^h_` columns from parameter detection) and plots
absolute or percentage deviations of the chosen `response`. The title
shows the reference value in mm.

## Usage

``` r
plot_hpond_vs_ref(
  data,
  response,
  ref_scenario = "s00001",
  diff = c("abs", "pct"),
  param_cols = NULL,
  exclude_cols = NULL,
  exclude_cols_regex = "^h_",
  tol = 1e-09,
  quiet = FALSE
)
```

## Arguments

- data:

  Data frame with at least `scenario_name`, parameter columns, and
  `response`.

- response:

  Character; the response column to plot (e.g. `"h_pond_max"` or
  `"h_pond_mean"`).

- ref_scenario:

  Reference scenario ID (default `"s00001"`).

- diff:

  `"abs"` for absolute or `"pct"` for percentage deviation.

- param_cols:

  Optional explicit parameter columns (recommended if you prefilter).

- exclude_cols:

  Extra columns to exclude from parameter detection.

- exclude_cols_regex:

  Regex to auto-exclude non-parameter columns (default `"^h_"`).

- tol:

  Numeric tolerance passed through to
  [`find_single_param_variations()`](https://kwb-r.github.io/kwb.raindrop/reference/find_single_param_variations.md).

- quiet:

  Logical; diagnostics if `FALSE`.

## Value

A `ggplot` object.
