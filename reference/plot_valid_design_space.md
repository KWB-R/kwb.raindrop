# Valid solutions in design space (mulde_area × storage_height) with tooltip labels

Visualises model results in a 2D design space (by default `mulde_area`
vs. `storage_height`) and highlights valid solutions (by default
`n_overflows <= 1`). The function also creates an HTML tooltip string
via `aes(text = ...)` listing all other varied parameters from
`param_grid` (excluding `x` and `y`), making the plot directly usable
with `plotly::ggplotly(..., tooltip="text")`.

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
  keep_param_grid_limits = TRUE
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

  Numeric. Validity threshold: `overflow_col <= valid_max`.

- x:

  Character. Name of the x-axis column (default `"mulde_area"`).

- y:

  Character. Name of the y-axis column (default `"mulde_height"`).

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

  Logical. If `TRUE`, apply jitter (useful when many points share
  identical x/y).

- jitter_width:

  Numeric or `NULL`. Jitter width in data units; if `NULL` defaults from
  x-range.

- jitter_height:

  Numeric or `NULL`. Jitter height in data units; if `NULL` defaults
  from y-range.

- jitter_factor:

  Numeric. Factor to derive default jitter width/height from x/y ranges.
  Default 0.005.

- alpha_mode:

  Character. Either `"none"` (fixed alpha) or `"duplicates"` (vary alpha
  by number of scenarios sharing the same x/y). Default `"none"`.

- alpha_min:

  Numeric. Minimum alpha used when `alpha_mode="duplicates"`. Default
  0.20.

- alpha_max:

  Numeric. Maximum alpha used when `alpha_mode="duplicates"`. Default
  1.00.

- keep_param_grid_limits:

  Logical. If `TRUE` and `drop_overflow_gt_valid_max = TRUE`, the x/y
  axis limits are fixed to the full range (or full set of levels) found
  in `param_grid`, so the design-space axes do not shrink after
  filtering. Default `TRUE`.

## Value

A ggplot object. Tooltip text is mapped via `aes(text = ...)`.

## Details

Varied parameters are detected automatically as columns in `param_grid`
with more than one distinct value. Optionally, scenarios with
`overflow_col > valid_max` can be removed entirely
(`drop_overflow_gt_valid_max = TRUE`). To help identify multiple
scenarios that share identical `x`/`y` coordinates but differ in other
parameters, optional jitter and/or variable transparency can be applied.
