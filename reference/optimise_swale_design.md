# Find the cost-optimal swale design per overflow target

Coordinate-descent optimiser built from a single primitive
([`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md),
bisection over one parameter): shrink the expensive lever first
(`mulde_area`), then the cheap one (`mulde_height`); the storage layer
starts at its smallest level and is only escalated when the area is
stuck at its upper bound. The filter conductivity is expected to be
fixed at the maximum via `fixed` (it is cost-free and dominant, see the
`monotonicity_analysis` vignette). Every engine run is cached, so the
sweep over all `x_targets` and both storage types shares evaluations.

## Usage

``` r
optimise_swale_design(
  run_fn,
  x_targets = 0:5,
  area_bounds = c(25, 200),
  area_tol = 2,
  height_bounds = c(100, 300),
  height_tol = 10,
  storage_spec = default_storage_spec(),
  fixed = list(connected_area = 1000, filter_height = 300, filter_hydraulicconductivity =
    360, bottom_hydraulicconductivity = 12),
  prior_results = NULL,
  split_jitter = 0,
  max_total_depth = NULL,
  cost_rates = default_cost_rates(),
  verbose = TRUE
)
```

## Arguments

- run_fn:

  `function(params)` running one scenario and returning at least
  `n_overflows` plus `sum_overflows` (mm) or `overflow_volume_m3`;
  typically created with
  [`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md).
  `params` is a named list of `mulde_area`, `mulde_height`,
  `storage_type`, `storage_height` plus everything in `fixed`.

- x_targets:

  Integer vector of overflow targets (feasible :\<=\>
  `n_overflows <= x`), default `0:5`.

- area_bounds, area_tol:

  Search range (m2) and resolution for `mulde_area`. The found area sits
  up to one `area_tol` above the exact feasibility boundary (worst-case
  cost overshoot roughly `area_tol` x specific cost per m2, i.e. a few
  percent at the default 2 m2); thanks to the bisection, every *halving*
  of `area_tol` costs only one additional engine run per area search –
  the cheapest precision lever of this optimiser. It also shrinks the
  tolerance-artefact part of the final `mulde_height` trim (the values
  just below the height maximum at low `x_targets`).

- height_bounds, height_tol:

  Search range (mm) and resolution for `mulde_height`.

- storage_spec:

  Storage search space per type, see
  [`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md):
  discrete `levels` (infiltration box) or continuous `bounds` + `tol`
  (gravel trench).

- fixed:

  Named list of parameters passed unchanged to `run_fn` (connected area,
  filter geometry, kf at maximum, ...). Must contain `filter_height` for
  the cost model.

- prior_results:

  Optional data.frame with prior (grid) results in the workflow CSV
  schema, used as warm start (narrows the first area bracket to one grid
  step).

- split_jitter:

  Passed to
  [`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md):
  0 (default) = deterministic halving; \> 0 randomises every bisection
  split point (Monte-Carlo of the search path – repeated runs with
  different seeds must agree within the search tolerances).

- max_total_depth:

  Optional analytic depth constraint in mm:
  `mulde_height + filter_height + storage_height <= max_total_depth`
  (e.g. from DWA-A 138 groundwater clearance or cover requirements).
  Enforced without any simulation runs.

- cost_rates:

  Unit costs, see
  [`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md).

- verbose:

  Print one progress line per solved cell.

## Value

Tibble with one row per (storage type, x): the optimal design
(`mulde_area`, `mulde_height`, `storage_height`), its metrics
(`n_overflows`, `overflow_volume_m3`, `et_pct`), cost columns from
[`compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md),
a `status` (`"ok"` or `"infeasible_within_bounds"`),
`monotonicity_warning` (volume referee) and `n_runs_new` (fresh engine
runs spent on this cell). All evaluated designs are attached as
attribute `"evaluations"`.

## Details

**The search order is derived from `cost_rates`** via a specific-cost
proxy (EUR per mm of storage capacity, capacity model
`V ~ area * (mulde_height + porosity * storage_height)`; the layer
porosity comes from `storage_spec`, see
[`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md)):
maximising `mulde_height` first is optimal for *any* rates under this
cost model (it costs only excavation, while area pays every component),
and the starting storage level is chosen as the cheapest level per mm of
capacity – the smallest level under the default rates, a high level when
e.g. the storage material is cheap. Without a `porosity` entry in
`storage_spec` the legacy order (smallest level first) is used. The
proxy is a first-order heuristic: it assumes capacity-additive levers
and cannot rank parameters with nonlinear hydraulic effects (e.g. a
variable filter conductivity) – for those, and as the assumption-free
cross-check, use
[`optimise_swale_design_simultaneous()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design_simultaneous.md),
which carries `cost_rates` directly inside its objective.

## See also

[`optimise_swale_design_simultaneous()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design_simultaneous.md)
(alternative: all parameters at once via penalised Nelder-Mead, as an
independent cross-check of the coordinate descent),
[`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md),
[`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md),
[`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md)
