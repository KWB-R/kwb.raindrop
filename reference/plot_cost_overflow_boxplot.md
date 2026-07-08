# Cost boxplot per overflow-event count, points sized by overflow volume

Companion to
[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md).
For every number of overflow events (x-axis) it draws a boxplot of the
total construction cost (y-axis, EUR) across all scenarios with that
count, overlaid with the individual scenarios as jittered points whose
**size scales with `size_by`** – the overflow volume (`m3`,
`sum_overflows` (`mm`) \* `mulde_area` (`m2`) / 1000; the default) or
the element evapotranspiration share (%). One best scenario per box is
highlighted; `best_by` selects its objective – cheapest, smallest
overflow volume, or highest evapotranspiration (cost as tie-breaker) –
so the three variants trace three different frontier lines. `label_best`
annotates the marker.

## Usage

``` r
plot_cost_overflow_boxplot(
  simulation_results_optimisation,
  param_grid,
  x = 5,
  filter_n_gtx = FALSE,
  use_jitter = TRUE,
  jitter_width = 0.2,
  jitter_seed = 1L,
  max_point_size = 6,
  box_alpha = 0.35,
  point_alpha = 0.6,
  digits = 2L,
  digits_params = 4L,
  lang = c("de", "en"),
  param_labels = NULL,
  size_by = c("overflow_volume", "evapotranspiration"),
  best_by = c("min_cost", "min_overflow", "max_evapotranspiration"),
  label_best = FALSE,
  title = NULL,
  lab_x = NULL,
  lab_y = NULL,
  lab_size = NULL,
  mark_best = TRUE,
  connect_best = TRUE,
  legend_position = "right"
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

  Numeric threshold. Counts `0..x` each get their own box; counts `> x`
  collapse into a single `">x"` box.

- filter_n_gtx:

  Logical. If `TRUE`, scenarios with `n_overflows > x` are dropped
  (removing the `">x"` box) before plotting.

- use_jitter:

  Logical. If `TRUE`, points are horizontally jittered.

- jitter_width:

  Numeric. Horizontal jitter half-width.

- jitter_seed:

  Integer. Seed for reproducible jitter.

- max_point_size:

  Numeric. Point size for the largest (valid-region) `size_by` value;
  the smallest maps to a fixed minimum so no point vanishes.

- box_alpha, point_alpha:

  Numeric in `[0, 1]`. Box-fill / point opacity.

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

- size_by:

  Character. Which variable drives the point area (and its legend):
  `"overflow_volume"` (default, m3) or `"evapotranspiration"` (the
  element evapotranspiration share in %, from
  `element.WB_Evapotranspiration_` – larger points then mean *more*
  evapotranspiration, which is desirable).

- best_by:

  Character. Objective for the highlighted best scenario per box, with
  cost as the tie-breaker: `"min_cost"` (default; cheapest, ties broken
  by `scenario_name`), `"min_overflow"` (smallest overflow volume) or
  `"max_evapotranspiration"` (highest evapotranspiration). In the `">x"`
  box the fewest-overflow scenario is picked first, `best_by` then
  breaking ties.

- label_best:

  Logical. If `TRUE`, the best scenario per box is annotated next to it:
  overflow volume plus overflow share (`"NN m3 / NN %"`) for
  `min_overflow`, the evapotranspiration share (`"NN %"`) for
  `max_evapotranspiration`, or the total cost for `min_cost`. Default
  `FALSE`.

- title, lab_x, lab_y:

  Optional character overrides for the default language-specific title /
  axis labels.

- lab_size:

  Optional character override for the size-legend title.

- mark_best:

  Logical. If `TRUE` (default), the best scenario per box (see
  `best_by`) is highlighted with a black-outlined diamond filled in that
  box's group colour, so its plotly tooltip inherits the group colour.

- connect_best:

  Logical. If `TRUE` (default), the highlighted best scenarios of
  **all** boxes (overflow counts `0..x` plus the `">x"` catch-all) are
  connected by a line – the best-per-overflow-level frontier.

- legend_position:

  Character. Legend position, default `"top"`.

## Value

A `ggplot` object. Convert to interactive via
`plotly::ggplotly(p, tooltip = "text")`.

## Details

Overflow counts greater than `x` are collapsed into a single `">x"`
catch-all box (furthest right, coloured red), keeping the axis readable
for the long-tailed 15-year runs (Wien / Bad Aussee reach several
hundred overflow events). Its highlighted scenario is the one with the
**fewest** overflow events above `x` (closest to the valid region). Set
`x` high to resolve more counts individually, or to `max(n_overflows)`
to give every count its own box.

The point tooltip is **identical** to
[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md):
scenario, overflow count / sum (`mm`) / volume (`m3`), the element water
balance (evapotranspiration / infiltration / overflow, %), the cost
breakdown (EUR) and the varying `param_grid` parameters translated via
`param_labels`. Points and boxes are coloured with the same green (low
counts) to red (`">x"`) palette as the sibling plots; because the colour
merely echoes the x-axis it carries no separate legend – only the
point-size legend is shown.

The point-size scale is calibrated to the valid region (`0..x`): the
extreme overflow volumes of the `">x"` catch-all are capped and a
minimum size keeps even zero-volume points (the `0`-overflow box)
visible, so the many-overflow outliers no longer shrink every
valid-region point to an invisible dot.

## See also

[`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
