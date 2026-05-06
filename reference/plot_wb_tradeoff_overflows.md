# Trade-off plot: Infiltration vs. Evapotranspiration (discrete colors by overflow threshold)

Creates a scatter plot from optimisation results with element
infiltration on the x-axis and element evapotranspiration on the y-axis.

## Usage

``` r
plot_wb_tradeoff_overflows(
  simulation_results_optimisation,
  param_grid,
  x = 1,
  filter_n_gtx = FALSE,
  use_jitter = TRUE,
  jitter_width = 0.15,
  jitter_height = 0.15,
  jitter_seed = 1L,
  digits = 2L,
  digits_params = 4L,
  lang = c("de", "en"),
  title = NULL,
  lab_x = NULL,
  lab_y = NULL,
  legend_position = "top"
)
```

## Arguments

- simulation_results_optimisation:

  Data frame with simulation results. Required columns are
  `scenario_name`, `n_overflows`, `sum_overflows`,
  `element.WB_InfiltrationNetto_`, `element.WB_Evapotranspiration_`, and
  `element.WB_Oberflaechenablauf_Ueberlauf_`.

- param_grid:

  Data frame with parameter grid. Must contain `scenario_name`.

- x:

  Numeric, typically integer. Threshold for overflow coloring. Values
  greater than or equal to `x` are mapped to the red category `">=x"`.

- filter_n_gtx:

  Logical. If `TRUE`, scenarios with `n_overflows >= x` are removed
  before plotting.

- use_jitter:

  Logical. If `TRUE`, slight jitter is applied to reduce overplotting.

- jitter_width, jitter_height:

  Numeric. Jitter strength in x- and y- direction, only used if
  `use_jitter = TRUE`.

- jitter_seed:

  Integer. Seed for reproducible jitter.

- digits:

  Integer. Number of digits used for rounding water balance values in
  the tooltip.

- digits_params:

  Integer. Number of digits used for rounding numeric parameter values
  in the tooltip.

- lang:

  Character. Plot language: `"de"` or `"en"`.

- title:

  Character or `NULL`. Plot title. If `NULL`, a language-specific
  default title is used.

- lab_x:

  Character or `NULL`. X-axis label. If `NULL`, a language-specific
  default label is used.

- lab_y:

  Character or `NULL`. Y-axis label. If `NULL`, a language-specific
  default label is used.

- legend_position:

  Character. Legend position, e.g. `"top"`, `"bottom"`, `"left"`, or
  `"right"`. Default `"top"`.

## Value

A `ggplot` object.

## Details

Coloring is *discrete* based on `n_overflows` and a threshold `x`:

- **dark green** for `n_overflows = 0`

- a discrete palette from dark green to yellow-green for integer levels
  `1..(x-1)`

- **red** for `n_overflows >= x`, shown as category `">=x"`

The plot language can be switched via `lang = "de"` or `lang = "en"`.
This affects title, axis labels, legend title, and tooltip labels unless
custom labels are supplied explicitly.

Tooltip text additionally includes all parameters from `param_grid` that
vary across scenarios, excluding `scenario_name`.
