# Cost vs. evapotranspiration scatter with storage-type shapes

Second companion to
[`plot_cost_vs_overflow_volume`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
for cost-aware optimisation. Plots the per-scenario **total construction
cost** (EUR) on the x-axis against the element **evapotranspiration
share** (% of the total water input, from
`element.WB_Evapotranspiration_`) on the y-axis. Points are coloured
discretely by the **number** of overflow events (same 0..x / \>x palette
used by the sibling plots, legend at the top) and **shaped by the
storage type**: filled square = infiltration box (Sickerbox), filled
triangle = gravel trench (Schotterrigol).

## Usage

``` r
plot_cost_vs_evaporation(
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
  caption = NULL,
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

- caption:

  Character or `NULL`. Caption below the plot naming the unit-cost rates
  the EUR values were computed with. `NULL` (default) uses
  [`cost_rates_caption()`](https://kwb-r.github.io/kwb.raindrop/reference/cost_rates_caption.md)
  with the
  [`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md);
  pass your own string if the costs were computed with different rates,
  or `""` to drop the caption. Note that
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  drops ggplot captions – re-add it to the interactive version via
  [`plotly_add_caption()`](https://kwb-r.github.io/kwb.raindrop/reference/plotly_add_caption.md).

- legend_position:

  Character. Legend position, default `"top"`.

## Value

A `ggplot` object. Convert to interactive via
`plotly::ggplotly(p, tooltip = "text")`.

## Details

The tooltip is identical to
[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md):
scenario, overflow count / sum (mm) / volume (m3), the element water
balance (`element.WB_Evapotranspiration_`,
`element.WB_InfiltrationNetto_`,
`element.WB_Oberflaechenablauf_Ueberlauf_`, all as % of the total water
input), the storage type, the usable storage volume of the storage layer
(m3), the cost breakdown (`cost_excavation`, `cost_profiling`,
`cost_filter`, `cost_storage`, `cost_total`), the derived **cost per
percentage point of evapotranspiration** (EUR/%) plus the varying
parameters from `param_grid` (excluding `scenario_name`).

The plot language can be switched via `lang = "de"` or `lang = "en"`.
Titles / axis labels / legend / tooltip labels follow the choice unless
explicit overrides are supplied.

## See also

[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
for cost vs. overflow volume and
[`plot_cost_overflow_boxplot()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_overflow_boxplot.md)
for the boxplot views (including `y_var = "cost_per_evap_pct"`, the cost
per percentage point of evapotranspiration).
