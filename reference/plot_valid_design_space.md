# Valid solutions in design space (x × y) with overflow-threshold discrete color scale

Visualises model results in a 2D design space (by default `mulde_area`
vs. `mulde_height`) and highlights valid solutions (by default
`n_overflows <= valid_max`). The plot is prepared for direct use with
`plotly::ggplotly(..., tooltip = "text")` by mapping an HTML tooltip via
`aes(text = ...)` that lists all other varied parameters from
`param_grid`, excluding `x` and `y`.

## Usage

``` r
plot_valid_design_space(
  param_grid,
  sim_results,
  id_col = "scenario_name",
  overflow_col = "n_overflows",
  valid_max = 1,
  x = "mulde_area",
  y = "mulde_height",
  max_levels = 50,
  alpha_invalid = 0.12,
  size = 2.4,
  digits = 4,
  drop_overflow_gt_valid_max = FALSE,
  jitter = FALSE,
  jitter_width = NULL,
  jitter_height = NULL,
  jitter_factor = 0.005,
  alpha_mode = c("none", "duplicates"),
  alpha_min = 0.2,
  alpha_max = 1,
  keep_param_grid_limits = TRUE,
  lang = c("de", "en"),
  title = NULL,
  subtitle = NULL,
  legend_position = "top"
)
```

## Arguments

- param_grid:

  A data.frame/tibble containing the scenario id (`id_col`) and
  parameters.

- sim_results:

  A data.frame/tibble containing the scenario id (`id_col`) and
  `overflow_col`.

- id_col:

  Character. Join key (scenario id), default `"scenario_name"`.

- overflow_col:

  Character. Overflow outcome column, default `"n_overflows"`.

- valid_max:

  Numeric. Validity threshold: `overflow_col <= valid_max`. Also used as
  the orange breakpoint in the color scale.

- x:

  Character. Name of the x-axis column, default `"mulde_area"`.

- y:

  Character. Name of the y-axis column, default `"mulde_height"`.

- max_levels:

  Integer. Parameter columns with more than `max_levels` distinct values
  are ignored in the tooltip list. Default 50.

- alpha_invalid:

  Numeric. Extra alpha multiplier for invalid solutions (background
  layer).

- size:

  Numeric. Base point size.

- digits:

  Integer. Significant digits for numeric tooltip values.

- drop_overflow_gt_valid_max:

  Logical. If `TRUE`, scenarios with `overflow_col > valid_max` are not
  plotted at all. Default `FALSE`.

- jitter:

  Logical. If `TRUE`, apply jitter, useful when many points share
  identical x/y.

- jitter_width:

  Numeric or `NULL`. Jitter width in data units. If `NULL`, defaults
  from x-range.

- jitter_height:

  Numeric or `NULL`. Jitter height in data units. If `NULL`, defaults
  from y-range.

- jitter_factor:

  Numeric. Factor used to derive default jitter width/height from x/y
  ranges. Default 0.005.

- alpha_mode:

  Character. Either `"none"` for fixed alpha or `"duplicates"` to vary
  alpha by the number of scenarios sharing the same x/y. Default
  `"none"`.

- alpha_min:

  Numeric. Minimum alpha used when `alpha_mode = "duplicates"`. Default
  0.20.

- alpha_max:

  Numeric. Maximum alpha used when `alpha_mode = "duplicates"`. Default
  1.00.

- keep_param_grid_limits:

  Logical. If `TRUE` and `drop_overflow_gt_valid_max = TRUE`, the x/y
  axis limits are fixed to the full range or full set of levels found in
  `param_grid`, so the design-space axes do not shrink after filtering.
  Default `TRUE`.

- lang:

  Character. Plot language: `"de"` or `"en"`.

- title:

  Character or `NULL`. Plot title. If `NULL`, a language-specific
  default title is used.

- subtitle:

  Character or `NULL`. Plot subtitle. If `NULL`, a language-specific
  default subtitle is used.

- legend_position:

  Character. Legend position, e.g. `"top"`, `"bottom"`, `"left"`, or
  `"right"`. Default `"top"`.

## Value

A ggplot object. Tooltip text is mapped via `aes(text = ...)`.

## Details

Varied parameters are detected automatically as columns in `param_grid`
with more than one distinct value. Optionally, scenarios with
`overflow_col > valid_max` can be removed entirely
(`drop_overflow_gt_valid_max = TRUE`). To help identify multiple
scenarios that share identical `x`/`y` coordinates but differ in other
parameters, optional jitter and/or variable transparency can be applied.

Discrete color mapping (threshold-style):

- **dark green** at `n_overflows = 0`

- a discrete palette from dark green to yellow-green for integer levels
  `1..(valid_max-1)`

- **orange** at `n_overflows = valid_max`

- **red** for `n_overflows > valid_max`, shown as level `">valid_max"`

The plot language can be switched via `lang = "de"` or `lang = "en"`.
This affects title, subtitle, legend title, tooltip labels, and selected
axis labels.
