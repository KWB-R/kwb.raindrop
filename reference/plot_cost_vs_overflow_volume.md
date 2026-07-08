# Cost vs. overflow-volume scatter with n_overflows-coloured points

Companion to
[`plot_wb_tradeoff_overflows`](https://kwb-r.github.io/kwb.raindrop/reference/plot_wb_tradeoff_overflows.md)
for cost-aware optimisation. Plots the per-scenario **total construction
cost** (EUR) on the x-axis against the **overflow volume** (m3) on the
y-axis, with the points coloured discretely by the **number** of
overflow events (same 0..x / \>x palette used by
`plot_wb_tradeoff_overflows`, legend at the top).

## Usage

``` r
plot_cost_vs_overflow_volume(
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
  param_labels = NULL,
  title = NULL,
  lab_x = NULL,
  lab_y = NULL,
  legend_position = "top"
)
```

## Arguments

- simulation_results_optimisation:

  Data frame with the columns `scenario_name`, `n_overflows`,
  `sum_overflows`, `mulde_area`, `element.WB_Evapotranspiration_`,
  `element.WB_InfiltrationNetto_`,
  `element.WB_Oberflaechenablauf_Ueberlauf_`, `cost_excavation`,
  `cost_profiling`, `cost_filter`, `cost_storage`, `cost_total`,
  `storage_type`. Typically the joined output of
  [`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md)
  and
  [`compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md).

- param_grid:

  Data frame with parameter grid. Must contain `scenario_name`.

- x:

  Numeric threshold for the overflow-count colour bucket. Values greater
  than `x` are pushed into the red `">x"` category.

- filter_n_gtx:

  Logical. If `TRUE`, scenarios with `n_overflows > x` are dropped
  before plotting.

- use_jitter, jitter_width, jitter_height, jitter_seed:

  As in
  [`plot_wb_tradeoff_overflows()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_wb_tradeoff_overflows.md).

- digits:

  Integer. Rounding for numeric values in the tooltip.

- digits_params:

  Integer. Rounding for parameter values in the tooltip.

- lang:

  Character. Plot language: `"de"` or `"en"`.

- param_labels:

  Named character vector translating `param_grid` columns to tooltip
  labels, or `NULL` to use
  [`default_param_labels()`](https://kwb-r.github.io/kwb.raindrop/reference/default_param_labels.md)
  for `lang`.

- title, lab_x, lab_y:

  Optional character overrides for the default language-specific title /
  axis labels.

- legend_position:

  Character. Legend position, default `"top"`.

## Value

A `ggplot` object. Convert to interactive via
`plotly::ggplotly(p, tooltip = "text")`.

## Details

Overflow volume is computed from `sum_overflows` (in mm on the swale
surface, as returned by
[`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md))
multiplied by `mulde_area` (m2) and converted to m3:
`overflow_volume_m3 = sum_overflows * mulde_area / 1000`.

The tooltip carries the element water balance
(`element.WB_Evapotranspiration_`, `element.WB_InfiltrationNetto_`,
`element.WB_Oberflaechenablauf_Ueberlauf_`, all as % of the total water
input) and the cost breakdown (`cost_excavation`, `cost_profiling`,
`cost_filter`, `cost_storage`, `cost_total`) plus the varying parameters
from `param_grid` (excluding `scenario_name`), so the user can hover
over a scatter point and see exactly why it landed where it did.

The plot language can be switched via `lang = "de"` or `lang = "en"`.
Titles / axis labels / legend / tooltip labels follow the choice unless
explicit overrides are supplied.

## See also

[`plot_cost_overflow_boxplot()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_overflow_boxplot.md)
for the same data / tooltip shown as a cost-by-overflow-count boxplot.
