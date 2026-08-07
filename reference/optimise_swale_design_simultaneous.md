# Find the cost-optimal swale design by simultaneous parameter search

Alternative to the coordinate-descent optimiser
([`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md),
bisection per parameter): all design parameters – `mulde_area`,
`mulde_height` and `storage_height` – are optimised **simultaneously**.
Infeasible designs (`n_overflows > x`) are not excluded but penalised
(any infeasible design is worse than any feasible one; the number of
excess events grades the penalty, steering the search back towards
feasibility), so the search moves freely through the full parameter
space and can trade the parameters against each other in a single step –
it does not rely on the per-parameter monotonicity that the bisection
exploits.

## Usage

``` r
optimise_swale_design_simultaneous(
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
  method = c("nelder_mead", "diff_evolution", "halton_search"),
  n_starts = 4,
  max_evals = 80,
  seed = 1,
  wobble = 1L,
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

  Search range (m2) and resolution for `mulde_area`.

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
  schema, used as warm start (the cheapest feasible grid cell of the
  branch becomes the first start / seeds the population).

- method:

  Search method, see Details: `"nelder_mead"` (default),
  `"diff_evolution"` or `"halton_search"` (the latter two mainly for
  comparison).

- n_starts:

  Number of Nelder-Mead starts per (storage type, x) cell (default 4;
  only used by `method = "nelder_mead"`). Warm starts (prior, previous
  target) count towards this number, then the storage-ladder anchors,
  then the space-filling points.

- max_evals:

  Soft cap on fresh engine runs per cell for the search phase: once
  reached, the search winds down (already cached designs remain free).
  The final multi-valley lattice polish adds its own runs on top
  (typically 20-50 per cell). Default 80 – thanks to the shared cache
  the later `x_targets` of a storage type stay cheaper.

- seed:

  Integer seed of the internal deterministic generator used by
  `method = "diff_evolution"` (ignored by the other methods). R's global
  RNG state is not touched.

- wobble:

  Maximum counting-artefact size tolerated at the upper corner (default
  1, matching the +1 event-counting wobble): only if the maximal design
  overflows by more than `wobble` events is the cell declared infeasible
  without a search.

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

Tibble with one row per (storage type, x), same schema as
[`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
plus a `method` column: the optimal design (`mulde_area`,
`mulde_height`, `storage_height`), its metrics (`n_overflows`,
`overflow_volume_m3`, `et_pct`), cost columns from
[`compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md),
a `status` (`"ok"` or `"infeasible_within_bounds"`),
`monotonicity_warning` (`TRUE` if a strictly larger design produced more
overflows *and* more overflow volume among the cell's evaluations) and
`n_runs_new` (fresh engine runs spent on this cell). All evaluated
designs are attached as attribute `"evaluations"`.

## Details

Three search `method`s share this penalised objective (plus cache,
tolerance snapping and final lattice polish) and differ only in how they
propose candidates:

- `"nelder_mead"` (default): multistart Nelder-Mead simplex via
  [`stats::optim()`](https://rdrr.io/r/stats/optim.html) – the
  recommended method.

- `"diff_evolution"`: a compact differential evolution (DE/rand/1/bin,
  population 12, F = 0.7, CR = 0.9), included for comparison.
  Deterministic: it draws from an internal Park-Miller generator seeded
  with `seed` and leaves R's global RNG (`.Random.seed`) untouched.

- `"halton_search"`: quasi-random space-filling sampling (Halton
  sequence, bases 2/3/5) – a deliberately simple baseline showing what
  the structured searches must beat.

Three ingredients keep the number of engine runs in check:

- **Snapping**: every candidate is snapped to the search tolerances
  (`area_tol`, `height_tol`, storage `tol` / discrete `levels`) before
  evaluation, so the shared cache absorbs repeated visits and the sweep
  over all `x_targets` reuses runs.

- **Multistart**: `n_starts` deterministic starting points (prior warm
  start and the optimum of the previous overflow target first, then a
  *storage ladder* – one anchor start per storage level, smallest level
  first – then fixed space-filling points) guard against the simplex
  stalling on the plateaus that the snapping and the integer overflow
  count create, and make sure every storage level competes: along the
  feasibility boundary the cost valley is flat, so the cheapest (usually
  smallest) storage level is easily missed from a single start.
  Different starts take different search paths – the counterpart of
  `split_jitter` in the bisection optimiser. Every start receives an
  equal slice of the remaining `max_evals` budget (unused runs roll
  over).

- **Lattice polish**: an accelerated pattern descent (steps of 8/4/2/1
  tolerances downwards, cheaper by construction) runs from the cheapest
  feasible design of every storage level visited – capped at the 6
  cheapest levels, which only bites for the continuous gravel trench
  (the discrete box has at most a handful) – because the storage axis
  separates cost valleys that single coordinate steps cannot cross.
  Besides the per-axis down steps each round proposes a *boundary slide*
  (area down with `mulde_height` at its maximum – the two-coordinate
  trade towards the cheap end of the feasibility boundary) and a
  *mulde_height floor probe* (at large `x` the overflow count saturates,
  so the whole lower height range can be feasible even when a +1
  counting wobble blocks every single step). All are just evaluated
  candidates – no monotonicity assumption enters. The result is locally
  optimal on the tolerance lattice, whatever the search method
  delivered.

The discrete infiltration-box levels are mapped onto a continuous latent
axis (each level owns an equal share of `[0, 1]`), the gravel trench is
searched continuously. The filter conductivity is expected to be fixed
at the maximum via `fixed` (cost-free and dominant, see the monotonicity
analysis,
<https://raindrop.kompetenz-wasser.io/optimisation/monotonicity_analysis/>).
`max_total_depth` is enforced by construction (the `mulde_height` axis
is compressed to the remaining depth), so no simulation runs are spent
on depth-invalid designs.

Compared to
[`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
this needs considerably more engine runs per cell (typically 60-120
instead of ~15; search phase plus multi-valley polish) but serves as an
independent cross-check: it can discover cheaper corners of the design
space that coordinate descent would miss if the parameter interaction
were stronger than the monotonicity analysis suggests.

## See also

[`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
(coordinate descent / bisection),
[`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md),
[`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md)

## Examples

``` r
# synthetic monotone model: overflows fall with retention capacity
run <- function(params) {
  cap <- params$mulde_area *
    (params$mulde_height + 0.95 * params$storage_height)
  list(n_overflows = max(0, floor(3.6e5 / cap) - 3),
       sum_overflows = 800 * max(0, 3.6e5 / cap - 3))
}
opt <- optimise_swale_design_simultaneous(
  run, x_targets = 1,
  storage_spec = default_storage_spec()["infiltration_box"],
  verbose = FALSE
)
opt[, c("x", "mulde_area", "mulde_height", "storage_height", "cost_total")]
#> # A tibble: 1 × 5
#>       x mulde_area mulde_height storage_height cost_total
#>   <int>      <dbl>        <dbl>          <dbl>      <dbl>
#> 1     1        125          300            300      29750
```
