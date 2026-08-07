# Changelog

## kwb.raindrop 0.1.0 (2026-08-07)

### New features

- New **swale-design optimiser** — finds the cost-minimal design per
  overflow target (`n_overflows <= x`) with roughly ten engine runs per
  (storage type, target) cell instead of a full factorial sweep, at
  finer resolution (2 m² / 10 mm instead of 25 m² / 100 mm grid steps):

  - [`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md)
    — the single primitive everything is built from: bisection for the
    smallest feasible value of one design parameter, over continuous
    bounds (`lower`/`upper`/`tol`) or discrete stack `levels` (binary
    search). Evaluations are memoised and two safety rules from the
    monotonicity analysis are built in: an **edge guard** for the +1
    event-counting wobble of the 4-h event separation (a descending
    ladder below an “infeasible by exactly one event” upper edge, so the
    Bad Aussee 175-m²-feasible/200-m²-infeasible pattern cannot eat a
    solution) and a **volume referee** that warns — and flags the result
    — iff the overflow count *and* the overflow volume increase together
    (real non-monotonicity; never observed in the 5 112 validation
    comparisons). An optional `split_jitter` randomises the bisection
    split point — a Monte-Carlo of the search path (repeated runs with
    different seeds must agree within `tol`).
  - [`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
    — coordinate descent whose **search order is derived from
    `cost_rates`** via a specific-cost proxy (EUR per mm of storage
    capacity, capacity model V ≈ area × (mulde_height + porosity ×
    storage_height); porosity from
    [`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md)):
    maximising `mulde_height` first is provably optimal for any rates
    under this cost model, and the starting storage level is the
    cheapest level per mm of capacity — the smallest under the default
    rates, a high level when e.g. the storage material is cheap (with
    box material at 5 EUR/m³ this finds the 63 m²/1200 mm corner for
    12.7k EUR in 15 runs, where the fixed legacy order returned 155 m²/
    300 mm for 20.8k EUR; regression-tested against the simultaneous
    optimiser). Specs without a `porosity` entry keep the legacy order;
    the proxy assumes capacity-additive levers, so parameters with
    nonlinear hydraulic effects remain the domain of
    [`optimise_swale_design_simultaneous()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design_simultaneous.md).
    Within a cell the descent then runs: minimal feasible `mulde_area`
    at maximal `mulde_height` on the chosen storage level, storage
    escalated only when the area is stuck at its upper bound,
    `mulde_height` shrunk last. One shared evaluation cache spans all
    `x_targets` and both storage types (a run classifies itself for
    every target at once), warm-start brackets are derived from prior
    brute-force results (CSV schema of the workflows), `max_total_depth`
    adds an analytic depth constraint (DWA-A 138 groundwater clearance /
    cover requirements) that costs no simulation runs, and “infeasible
    within bounds” is a regular result status, not an error. Costs are
    attached via
    [`compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md);
    all evaluated designs ship as attribute `"evaluations"`.
  - [`optimise_swale_design_simultaneous()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design_simultaneous.md)
    — alternative optimiser that searches **all design parameters at
    once** (`mulde_area`, `mulde_height`, `storage_height`) instead of
    per-parameter bisection: infeasible designs are not excluded but
    penalised (any infeasible design is worse than any feasible one;
    excess overflow events grade the penalty and steer the search back
    towards the feasibility boundary, where the optimum lives), so the
    search can trade the parameters against each other in a single step
    and does not rely on the per-parameter monotonicity the bisection
    exploits. Three search `method`s share this penalised objective, the
    tolerance snapping (the shared cache absorbs repeats across all
    `x_targets`) and a final **multi-valley lattice polish**
    (accelerated 8/4/2/1-tolerance pattern descent from the cheapest
    feasible design of every storage level visited, capped at the 6
    cheapest levels for the continuous gravel trench — the storage axis
    separates cost valleys that single coordinate steps cannot cross;
    each round also proposes a *boundary slide* — area down with
    `mulde_height` at its maximum, the two-coordinate trade towards the
    cheap end of the feasibility boundary — and a `mulde_height` *floor
    probe* that jumps over +1 counting-wobble bands, both plain
    evaluated candidates without any monotonicity assumption):
    `"nelder_mead"` (default; deterministic multistart via
    [`stats::optim()`](https://rdrr.io/r/stats/optim.html) — prior warm
    start, previous-target optimum, one anchor start per storage level,
    space-filling points; every start gets an equal slice of the
    `max_evals` run budget), `"diff_evolution"` (compact DE/rand/1/bin
    for comparison; deterministic via an internal Park-Miller generator
    seeded with `seed` — R’s global RNG stays untouched) and
    `"halton_search"` (quasi-random space-filling baseline). Same
    interface and result schema as
    [`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
    (incl. `max_total_depth`, warm start and the `"evaluations"`
    attribute) plus a `method` column; a pairwise dominance check per
    cell (a strictly larger design with more overflows *and* more
    overflow volume) replaces the bisection’s volume referee. Needs
    considerably more engine runs per cell (typically 60–120 instead of
    ~15) but serves as an independent cross-check that coordinate
    descent did not miss a cheaper corner of the design space.
  - [`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md)
    — package-level refactoring of the `run_one()` function previously
    duplicated across the three case-study vignettes: one closure
    factory covering both variants (Eisenstadt: `base.h5` rain curve
    scaled by `rain_factor`; Wien / Bad Aussee: own rain + ET0 series in
    mm/h incl. the Growth/Shading end-time fix). Returns the thinned
    one-row optimisation result augmented with `overflow_volume_m3` (=
    `sum_overflows` \[mm\] × `mulde_area` / 1000). **Behaviour note —
    files are deleted by default:** with `cleanup = TRUE` (the default)
    each scenario’s input copy *and its output HDF5s*
    (`Mulde_Rigole.h5`, `Dach.h5`, `Fehlerprotokoll.h5`, …) are removed
    right after the one-row result has been read — only the returned
    tibble survives a run. Pass `cleanup = FALSE` if you need the raw
    scenario files (failed runs always keep theirs for debugging).
    Rationale: without the cleanup, long searches (hundreds of engine
    runs per task, each with its own `base.h5` copy plus output HDF5s)
    fill the temp drive and the engine dies with HDF5 `errno = 28` (“No
    space left on device”). Prepares a **site master file** once
    (base.h5 + calculation settings + ET/rain series) and writes only
    the ~15 small parameter datasets per run instead of reading and
    rewriting *every* dataset each time — that full HDF5 round trip
    (plus process spawn and virus-scanner latency on new files) was the
    dominant per-run cost of the optimisation searches, several times
    the ~2 s engine time of the Eisenstadt model.
  - [`stack_levels()`](https://kwb-r.github.io/kwb.raindrop/reference/stack_levels.md),
    [`sickerbox_level_presets()`](https://kwb-r.github.io/kwb.raindrop/reference/sickerbox_level_presets.md),
    [`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md),
    [`default_storage_types()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_types.md)
    — storage-layer search spaces: achievable stack heights from module
    heights (incl. mixed combinations such as Rigofill full + half
    block), manufacturer presets (GRAF, Fränkische, ACO, Wavin; verify
    against data sheets before productive runs) with the brute-force
    grid levels 300/600/900/1200 mm as the default, and the
    gravel-trench range coupled at 3 × the box range (usable-porosity
    ratio 0.95 / 0.3).

- New conditional vignette `monotonicity_analysis` — validates the
  optimiser’s core assumption on the three brute-force result sets (5
  112 neighbour comparisons): `n_overflows` is quasi-monotone in every
  design parameter (13 violations, all +1 counting artefacts of the 4-h
  event separation), the overflow volume (in m³) is monotone without
  exception, ET depends on `mulde_area` only, and the filter
  conductivity is a cost-free dominant lever (fix at maximum). Renders
  after the three workflow vignettes into
  `vignettes/monotonicity_analysis/` (deploy unit with the
  plain-language report `index.html` and the exported `mono_*` detail
  tables as CSV + interactive HTML).

- New conditional vignette `workflow_optimisation` — runs the optimiser
  for all three sites (Eisenstadt 2005, Wien, Bad Aussee),
  **parallelised over site × storage type** (6 independent tasks via
  `future`/`future.apply`; wall time = longest single task, ~15–20 min
  instead of ~65 min sequential — bisection within a search is
  inherently sequential and the x-targets of one storage type share the
  evaluation cache, but box and trench never share a single engine run).
  Exposes the search space and the cost rates as explicit, adjustable
  code (defaults used), warm-starts from the grid CSVs, renders the
  combined optimum table, per-site cost-effectiveness curves,
  total-runtime reporting (per task, per section and for the whole
  document) and a **Monte-Carlo section** (`n_mc = 10`) evaluating the
  robustness of the *search itself*: every bisection split point is
  randomly displaced (`split_jitter = 0.3`) while rain, cost rates and
  all other inputs stay fixed; the repetitions with different seeds must
  agree on the storage level, keep the area within 2 × `area_tol` and
  the cost within a few percent — the swale depth may scatter somewhat
  more because it is hydraulically coupled to the found area (x = 1; the
  full pool of 3 sites × 2 storage types × 10 repetitions runs as 60
  parallel tasks, one full re-optimisation each).

- New conditional vignette `workflow_optimisation_simultaneous` — the
  simultaneous counterpart of `workflow_optimisation` (which stays
  bisection-only and now points here): runs the Nelder-Mead sweep for
  all three sites in parallel (site × storage type), compares the optima
  cell by cell against the bisection CSV export when present
  (`delta_pct` table), and benchmarks the three search methods
  (Nelder-Mead / differential evolution / Halton baseline) on the same x
  = 1 cell across all sites and storage types — 12 parallel tasks — to
  show what the structured searches contribute over naive sampling. Both
  compute chunks report live progress across the worker boundary (one
  tick per engine run — a multi-hour sweep no longer looks frozen), the
  site list has a quick-test switch (Eisenstadt only) and `max_evals` is
  exposed as the runtime lever.

- New exported helper
  [`read_site_timeseries()`](https://kwb-r.github.io/kwb.raindrop/reference/read_site_timeseries.md)
  — the rain/ET0 time-series preparation previously duplicated in the
  Wien and Bad Aussee vignettes (hours since start, series-end
  alignment, engine mm/h convention); selects strictly `time` + `value`,
  tolerating extra raw-data columns (Bad Aussee ships a `substation`
  column that Wien does not have).

- **testthat suite added** (edition 3; `tests/testthat/`): unit tests
  for the bisection primitive (threshold accuracy, run counts, wobble
  guard, volume referee, discrete levels) and end-to-end optimiser tests
  against a synthetic monotone hydraulic model, verified against a fine
  brute-force reference (cost within 5 %, monotone cost-effectiveness
  curve, storage escalation, infeasibility handling, warm-start savings,
  `max_total_depth`).

- New exported plot
  [`plot_cost_vs_evaporation()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_evaporation.md)
  — third cost view: scatters `cost_total` (EUR, x) against the element
  evapotranspiration share (`element.WB_Evapotranspiration_`, %, y).
  Points share the overflow-count palette of the sibling plots and are
  **shaped by the storage type** (filled square = infiltration box /
  Sickerbox, filled triangle = gravel trench / Schotterrigol); identical
  tooltip. Rendered as `*_cost-vs-evaporation.html` in the three
  case-study vignettes and linked from `vignettes/index.Rmd` under
  “Kosten vs. Evapotranspiration”.

- [`plot_cost_overflow_boxplot()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_overflow_boxplot.md)
  gains `y_var = "cost_per_evap_pct"` (y-axis = total cost per
  percentage point of evapotranspiration **above the reference minimum**
  — the lowest evapotranspiration among the scenarios satisfying the
  validity criterion (`n_overflows <= x`; fallback: complete run) —,
  EUR/%; the reference (minimum share, criterion and scenario id) is
  named on a second title line, and `label_best = TRUE` annotates the
  evapotranspiration gain `"(+NN % Evapotranspiration)"` after the
  price; titles, y-label and the `min_cost` objective/label follow) and
  `facet_storage_type = TRUE` (two stacked storage-type panels —
  infiltration box on top, gravel trench below — each with its own
  best-per-box markers and frontier line;
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  keeps the split as stacked subplots). When both storage types share
  one panel (no faceting) the overlaid points are shaped by the storage
  type like the scatter siblings; faceted panels keep plain circles for
  readability. The vignettes render the three existing boxplot variants
  with storage-type panels plus the new `*_cost-per-evap-boxplot.html`
  (cheapest EUR/% per class, point size = evapotranspiration), linked
  from `vignettes/index.Rmd` under “Boxplot – Kosten je Prozent
  Evapotranspiration”.

- [`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
  points are now also **shaped by the storage type** (square/triangle,
  own legend under the colour legend).

- The shared cost tooltip gains a derived **“Kosten je %
  Evapotranspiration (über Min. von X %) \[€/%\]”** line right below the
  total cost: the total cost per percentage point of element
  evapotranspiration **above the reference minimum** (the lowest
  evapotranspiration among the scenarios satisfying the validity
  criterion `n_overflows <= x`; fallback: complete run) — the baseline
  comes “for free”, only the gain is paid for. The reference value is
  named in the line; “-” at or below the minimum. Shown consistently in
  both cost scatters and all cost boxplot variants. German labels
  consistently say **“Evapotranspiration”** instead of “Verdunstung”
  throughout.

- New **usable storage volume** of the storage layer:
  `storage_volume_m3 = mulde_area * storage_height/1000 * (thetaS - thetaFC)`
  (usable porosity 0.95 infiltration box / 0.3 gravel trench). The
  vignettes add the column to the parameter grid (grid datatable +
  results CSV) and the tooltips of **all** scenario plots show it as
  “Nutzbares Speichervolumen \[m³\]” — the cost scatters and boxplots
  (right below the storage type), the water-balance trade-off plot and
  the design-space plots (there sourced from `sim_results`, since the
  plotting grid drops the helper columns) — computed on the fly from the
  `storage_theta*` columns for existing result sets without the column.
  In the “Variierende Parameter” block the raw storage_type values are
  now translated too (`Speichertyp=Schotterrigol` instead of
  `=gravel_trench`; shared value labels with the
  [`plot_main_effects()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_main_effects.md)
  storage-type panel).

- The boxplots’ point-size legend keys match the plotted markers: with
  storage-type shapes in use (no faceting) they are drawn with the grey
  square/triangle instead of the default circle; the faceted variants
  use circular points and matching circular keys.

- Storage-type names in legends and facet strips are now the **short,
  language-specific** ones (“Sickerbox” / “Schotterrigol” for
  `lang = "de"`, “Infiltration box” / “Gravel trench” for `"en"`); only
  the bold tooltip line keeps the long bilingual form.

- All cost plots now carry a **caption naming the unit-cost rates** they
  were computed with (new exported
  [`cost_rates_caption()`](https://kwb-r.github.io/kwb.raindrop/reference/cost_rates_caption.md),
  built from
  \[[`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md)\]:
  Aushub 70 €/m³ · Profilierung + Begrünung 10 €/m² · Bodenfilter 200
  €/m³ · Sickerbox 350 €/m³ · Schotterrigol 50 €/m³,
  incl. installation). ggplot renders it at the bottom of the PDFs
  (`caption` argument, `""` to drop); since
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  drops captions, the new exported
  [`plotly_add_caption()`](https://kwb-r.github.io/kwb.raindrop/reference/plotly_add_caption.md)
  re-adds it as a bottom annotation in the interactive HTMLs (wired up
  in all three vignettes).

- New exported helper
  [`plotly_split_legend()`](https://kwb-r.github.io/kwb.raindrop/reference/plotly_split_legend.md)
  — cleans up the interactive legends:
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
  flattens colour + shape into unreadable
  `"(0,Sickerbox / Infiltration box)"` tuple entries. The helper
  rebuilds the legend from legend-only keys with unambiguous glyphs: one
  **neutral circle per overflow class in the class colour** (clicking
  toggles both storage types of the class) plus two **neutral grey**
  square/triangle keys under a “Speichertyp” / “Storage type” group
  title (skippable via `add_shape_legend = FALSE` for faceted plots) — a
  coloured square/triangle key would wrongly suggest one specific
  (colour, type) combination. The storage-type keys are **individually
  clickable** (a JavaScript handler toggles all traces with that marker
  symbol, since the traces’ only legend group is taken by the overflow
  class); the overlapping combined legend title is removed and the
  legend moves to a vertical layout on the right. Applied in all three
  vignettes to the cost-vs-overflow, cost-vs-evaporation, water-balance
  and design-space HTMLs.

- [`plot_wb_tradeoff_overflows()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_wb_tradeoff_overflows.md)
  no longer crashes with “Can’t combine `mulde_area` and `storage_type`
  ” on two-type grids: its inline copy of the varying-parameter tooltip
  block was replaced by the shared `build_varying_param_html()` helper
  (the tooltip parameter names are now translated via
  [`default_param_labels()`](https://kwb-r.github.io/kwb.raindrop/reference/default_param_labels.md),
  as in the cost plots). When the results carry a `storage_type` column,
  its points are shaped by the storage type (square/triangle) and the
  tooltip names the type; single-type result sets plot as before.

- [`plot_valid_design_space()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_valid_design_space.md)
  gains `facet_storage_type` — two stacked storage-type panels with
  **free y-scales**, so disjoint per-type levels (storage_height:
  300–1200 mm boxes vs. 900–3600 mm trenches) fill their own panel;
  duplicate-based alpha is then counted per panel and the points stay
  plain circles (the strips name the type). Without faceting, points are
  shaped by the storage type whenever `storage_type` varies. The
  vignettes facet both design-space blocks.

- [`plot_main_effects()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_main_effects.md)
  now supports character parameters (the pivot previously failed on
  mixed types), keeps numeric level ordering (“500” no longer sorts
  after “1000”), and renders `storage_type` as its own panel with
  display names (Sickerbox/Schotterrigol or Infiltration box/Gravel
  trench). The vignettes add `storage_type` to the main-effects
  parameter set.

- Fixed `build_varying_param_html()` (the shared tooltip helper): it
  errored with “Can’t combine `storage_height` and `storage_type` ” as
  soon as the character column `storage_type` varied across scenarios —
  i.e. for every grid sweeping both storage types
  (`values_transform = as.character` in the pivot). Numbers in the
  varying-parameters tooltip block are now formatted element-wise, so
  one decimal-valued parameter no longer forces trailing “.00” onto
  every other value. The vignettes additionally drop the storage_theta\*
  helper columns (fully determined by `storage_type`) from the plotting
  `param_grid`, keeping tooltips lean.

- New exported plot
  [`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
  — companion to
  [`plot_wb_tradeoff_overflows()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_wb_tradeoff_overflows.md)
  for cost-aware optimisation. Scatters `cost_total` (EUR) against
  overflow volume (m³, computed from `sum_overflows` \[mm\] and
  `mulde_area` \[m²\]), points coloured discretely by `n_overflows` with
  the same `0..x / ">x"` palette and top legend as the water-balance
  plot. Both cost plots report the **share of scenarios meeting the
  validity criterion** (`n_overflows` ≤ `x`) in the plot title
  (e.g. `(39 % mit ≤ 5 Überläufen)`), since ggplotly drops ggplot
  subtitles. The plotly tooltip carries the element water balance
  (evapotranspiration, infiltration, overflow — all in %), the chosen
  storage type on its own bold line (bilingual,
  `Sickerbox / Infiltration box` or `Schotterrigol / Gravel trench`),
  the full cost breakdown (excavation, profiling, filter, storage,
  total) plus the varying `param_grid` entries (translated via
  [`default_param_labels()`](https://kwb-r.github.io/kwb.raindrop/reference/default_param_labels.md)).
  Rendered as HTML (`*_cost-vs-overflow-volume.html`) in the three
  case-study vignettes and linked from `vignettes/index.Rmd` under a new
  “Kosten vs. Überlaufvolumen” section.

- New exported plot
  [`plot_cost_overflow_boxplot()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_overflow_boxplot.md)
  — boxplot of the total construction cost (EUR, y) per number of
  overflow events (x), with the individual scenarios overlaid as
  jittered points whose **size scales with the overflow volume** (m³;
  the size scale is calibrated to the valid region so the many-overflow
  outliers do not shrink the valid-region points away, and a minimum
  size keeps every point visible). Counts up to the threshold `x` (=
  `max_n_overflows`, as in the sibling plots) each get their own box;
  higher counts collapse into a single `">x"` catch-all box (furthest
  right, red), keeping the axis readable for the long-tailed 15-year
  runs (Wien / Bad Aussee reach several hundred overflow events); the
  `">x"` box highlights the scenario with the fewest overflow events
  above `x`. One best scenario per box is highlighted with a
  black-outlined diamond in that box’s group colour (so its tooltip
  inherits the group colour), and the best scenarios of **all** boxes
  are joined by a frontier line (`mark_best` / `connect_best`). The
  point tooltip is identical to
  [`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md).
  `best_by` picks the objective (cost as tie-breaker) — `"min_cost"`
  (cheapest), `"min_overflow"` (smallest overflow volume) or
  `"max_evapotranspiration"` (highest evapotranspiration) — so the three
  variants trace three different frontier lines; `label_best` annotates
  the marker (overflow volume + share `"NN m³ / NN %"`, or
  evapotranspiration `"NN %"`); `size_by` scales the points by overflow
  volume (default) or evapotranspiration. The three case-study vignettes
  render all three variants
  (`*_cost-by-overflows-boxplot-cheapest.html`, `*-min-overflow.html`,
  `*-max-evap.html`), each linked from `vignettes/index.Rmd` under the
  grouped “Kosten” section.

- New exported helper
  [`default_param_labels()`](https://kwb-r.github.io/kwb.raindrop/reference/default_param_labels.md)
  — German / English, unit-carrying labels for the parameter-grid
  columns. The “varying parameters” block of both cost-plot tooltips now
  shows e.g. `Muldenfläche [m²]=125` instead of the raw
  `mulde_area=125`; pass `param_labels =` to the plot functions to
  override.

### Consistency

- Non-ASCII characters in R code are now unicode-escaped: all string
  literals in
  [`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
  and in the vignette code chunks use `\uxxxx` escapes (rendered labels
  unchanged), and the few non-ASCII code comments were rewritten in
  plain ASCII. Markdown prose keeps UTF-8 (escapes are not interpreted
  there).

- Eisenstadt 2005 (`workflow_eisenstadt-2005.Rmd` and
  `workflow_eisenstadt-2005_neu.Rmd`) now pins
  `//Massnahmenelemente/Mulde_Rigole/Parameter_Evapotranspiration/LAI_LeafAreaIndex = 3.9`
  (Hörnschemeyer grass value) so all four case-study vignettes operate
  on the same LAI baseline. Wien uses it as one of the sweep levels
  (`c(3.9, 8.5)`), Bad Aussee identical to Wien.

### Bug fixes

- `vignettes/workflow_eisenstadt-2005.Rmd` now pipes the joined
  optimisation results through
  [`kwb.raindrop::compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md)
  like the Wien and Bad Aussee workflows already did. Without it the
  vignette’s
  [`plot_cost_vs_overflow_volume()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_cost_vs_overflow_volume.md)
  call aborted with “missing column(s): cost_excavation, …” — the
  cost-vs-overflow-volume PDF/HTML was never produced and the exported
  CSV lacked the cost columns.

- [`get_simulation_results_optim()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim.md)
  now treats a result HDF5 that exists but cannot be opened/read
  (e.g. the engine crashed mid-write for a scenario, or a file briefly
  locked just after the run) like a missing file: it
  [`warning()`](https://rdrr.io/r/base/warning.html)s, names the
  scenario, and returns `NULL` instead of throwing. Because the Wien /
  Bad Aussee / Eisenstadt workflows now read results per run inside
  `run_one()`, a single unreadable file used to abort the entire
  `future_lapply` batch (seen as an `H5File.open()` “unable to open
  file” error mid-render); the run now completes with NA rows for the
  affected scenarios.

- `vignettes/example_wien_minimal.Rmd`, `vignettes/workflow_wien.Rmd`
  and `vignettes/workflow_badaussee.Rmd` now convert ET0 from mm/day to
  mm/h (`value / period_et`) before writing `//Kurven/ET0`, mirroring
  the existing rain conversion. The engine reads the ET0 curve as a mm/h
  rate, so the unconverted daily values were integrated 24× too high —
  the cause of the implausibly large modelled ET share. The minimal
  vignette’s timeseries-info summary now labels ET0 as mm/h and recovers
  its total via `value * period_h`.

### New features

- The Wien and Bad Aussee workflows now thin each run to its
  optimisation row **inside** `run_one()` (via
  `get_simulation_results_optim(..., lean = TRUE)`

  - [`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md))
    and
    [`run_scenarios()`](https://kwb-r.github.io/kwb.raindrop/reference/run_scenarios.md)
    returns those one-row tibbles for a final
    [`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).
    This replaces the previous “run everything, then read every run’s
    full results into memory at once” pass
    ([`get_simulation_results_optim_parallel()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim_parallel.md)),
    drastically cutting peak RAM for large parameter grids.

- [`get_simulation_results_optim()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim.md)
  gains a `lean` argument. When `TRUE` it reads only the fields consumed
  downstream (`element$rates`, `element$water_balance`,
  `connected_area$water_balance`) and leaves the unused `meta`/`states`
  and `connected_area$rates` as `NULL`, minimising per-run memory and
  I/O. Its intro message is now gated behind `debug`.

- `inst/scripts/prepare_eisenstadt_swmm_timeseries.R` extracts the rain
  (`/Kurven/Regen`) and ET0 (`/Kurven/ET0`) curves from an engine HDF5
  and writes SWMM-5 external time-series files. It converts **out** of
  the engine’s mm/h convention (rain → mm per interval for `[RAINGAGES]`
  VOLUME, ET0 → mm/day for `[EVAPORATION]`). Pre-generated files for the
  bundled Eisenstadt 2005 template ship under
  `inst/extdata/models/eisenstadt-2005/swmm/` together with a README
  that documents the mm/h-vs-mm/day pitfall: the kernel reads
  `/Kurven/ET0` as **mm/h**, so a daily ET0 (mm/d) written there without
  dividing by 24 is integrated 24× too high.

- [`download_engine()`](https://kwb-r.github.io/kwb.raindrop/reference/download_engine.md)
  fetches the Tandler “Regenwasserbewirtschaftung” Windows executable
  from the companion repository
  [`KWB-R/kwb.raindrop.binaries`](https://github.com/KWB-R/kwb.raindrop.binaries/releases)
  and caches it under
  `tools::R_user_dir("kwb.raindrop", "cache")/<version>/`. Multiple
  engine versions can coexist side-by-side because the version is
  encoded in the release tag (`engine-<YYYY-MM-DD>`), not in the asset
  filename. The download is atomic (temp file + rename) and rejects
  obviously broken responses (\< 1 KB ⇒ likely a 404 HTML page).

- [`compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md)
  and
  [`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md)
  attach a per-scenario construction-cost breakdown (`cost_excavation`,
  `cost_profiling`, `cost_filter`, `cost_storage`, `cost_total`) to a
  parameter grid using the Austrian unit-cost rates supplied by Johannes
  Leimgruber (OeStaP, 2026-03-27). Pass
  `storage_type = "infiltration_box"` (Sickerbox, default, ~95 %
  porosity) or `"gravel_trench"` (Schotterrigol, ~30 % porosity) — or a
  per-row `storage_type` column on the grid — to switch the storage
  layer. The new columns are wired into the `example_wien_minimal`,
  `workflow_wien` and `workflow_badaussee` vignettes so the
  solution-space datatables become filterable and sortable by cost.

- New vignette `example_wien_minimal`: a self-contained smoke test of
  the full input → engine → results loop on Wien. Now extended into an
  ET-diagnostics grid that sweeps three engine switches —
  `keineEvapotranspirationBeiRegen`, `Hoernschemeyer_aktiv` and the
  `ET0ref_GrasReferenzverdunstung` factor (`0`, `1`, `100`) — at
  Daniel’s reference geometry (12 scenarios total). Daniel’s three
  XLSX-review corrections (`Dach/Evapotranspiration_aktiv = 0`,
  `EvapPond = 0`, `LAI = 3.9`) were applied briefly between PRs
  [\#11](https://github.com/KWB-R/kwb.raindrop/issues/11) and the one
  introducing this NEWS entry but made the Tandler engine return Status
  1 for every scenario, so they are reverted for now. They will be
  re-introduced one-by-one as sweep dimensions in a follow-up diagnostic
  vignette so the failing combination can be isolated. After the model
  loop the per-scenario `*.h5` inputs are dumped to a single XLSX
  (`raindrop_wien_minimal_params.xlsx`) with one sheet per scenario plus
  a `base` sheet for the un-modified template, a `timeseries_info` sheet
  summarising the rain / ET0 series fed to every run (identical across
  scenarios), and an `applied_settings` sheet listing the diff of every
  key the package writes on top of `base.h5` per scenario. Prints a
  complete static-parameter overview from `base.h5` for review of every
  default that drives the model. Designed to render on Windows CI; the
  four heavy case-study vignettes only render their parameter grids on
  CI and skip the model runs.

### Helper scripts

- `inst/scripts/prepare_wien_swmm_timeseries.R` converts the shipped
  Wien rainfall (10-minute, mm) and reference ET0 (daily, mm/day) series
  to SWMM-5 external time-series files (`wien_rain.dat`, `wien_et0.dat`)
  for direct import into a SWMM `[TIMESERIES]` / `[RAINGAGES]` /
  `[EVAPORATION]` block. Output directory defaults to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html); pass
  `out_dir` (R) or a positional CLI argument (`Rscript`) to redirect.

### Inputs and data shipping

- Per-scenario input data ships under `inst/extdata/models/<scenario>/`
  for Wien, Bad Aussee and Eisenstadt 2005 (`base.h5` HDF5 model
  templates plus `rain.csv.gz` and `et.csv` time series for the two
  GeoSphere-Austria sites). Vignettes read inputs via
  [`system.file()`](https://rdrr.io/r/base/system.file.html) and write
  model outputs to a [`tempdir()`](https://rdrr.io/r/base/tempfile.html)
  scratch root, so they are hermetic and reproducible.

- Rain timeseries are shipped gzipped (`rain.csv.gz`);
  [`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
  reads the compressed files transparently. Total `inst/extdata/`
  footprint is ~6.8 MB (down from ~53 MB raw).

- Provenance documented in `inst/extdata/SOURCES.md` and an “Input data”
  section in each rendered vignette: precipitation and
  evapotranspiration come from [GeoSphere
  Austria](https://www.geosphere.at/) (Österreichischer Wetterdienst,
  formerly ZAMG); HDF5 templates are produced with the Tandler engine.

### CI / packaging

- GitHub Actions workflows for Claude Code (`@claude` mention bot and
  automatic PR review) added; both pinned to Opus 4.7 with `think hard`
  reasoning on the review prompt.

- `R-CMD-check` matrix restricted to `windows-latest`
  (devel/oldrel/release) — the calculation engine is a Windows `.exe`,
  so non-Windows runners cannot exercise the workflow. Aligns with the
  existing `test-coverage` and `pkgdown` jobs.

- Bumped `actions/checkout` to v5 and pinned `actions/upload-artifact`
  to v4 (Node.js 24 readiness ahead of the 2026-09-16 Node 20 removal).

### Dependency hygiene

- `tidyr`, `rlang`: moved to `Imports` (used in package code).
- `plotly`: moved from `Imports` to `Suggests` (only used in vignettes).
- `htmlwidgets`, `readr`, `writexl`: added to `Suggests` (used in
  vignettes).

### Bug fixes

- [`get_simulation_results_optim()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim.md)
  and
  [`get_simulation_results_optim_parallel()`](https://kwb-r.github.io/kwb.raindrop/reference/get_simulation_results_optim_parallel.md)
  no longer return `NULL` when only the connected-area H5 (Dach.h5) is
  missing; they now return a partial result with `connected_area = NULL`
  while still populating the element side. This unblocks scenarios where
  `//Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv`
  is `0` and the engine consequently skips writing Dach.h5.

- [`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md)
  tolerates per-scenario `NULL` and missing components (`element`,
  `connected_area`, `*$water_balance`, `element$rates`). Affected
  scenarios still produce a row of the output tibble with the available
  metrics computed and the missing columns left as `NA`.

- [`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md)
  now fabricates an `NA`-filled column stub when one side’s water
  balance is missing while the other side has data, by mirroring the
  populated side’s variable names. Previously the missing side’s columns
  were dropped entirely
  ([`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
  only adds columns that at least one scenario contributes), which left
  the results table with no `connectedarea.*_` columns at all when every
  scenario disabled roof ET. The mirror keeps the column structure
  visible in the rendered datatable and the function emits one summary
  [`message()`](https://rdrr.io/r/base/message.html) per fallback path
  (instead of one per scenario) naming all affected scenarios, so the
  user can match the diagnostic to the all-NA rows.

- New exported
  [`default_canonical_wb_variables()`](https://kwb-r.github.io/kwb.raindrop/reference/default_canonical_wb_variables.md)
  returns the canonical set of water-balance variable names the Tandler
  engine writes (`WB_Regen`, `WB_Evapotranspiration`,
  `WB_InfiltrationNetto`, `WB_Oberflaechenablauf_Ueberlauf`,
  `WB_Oberflaechenablauf_Verschaltungen`). All five vignettes now pass
  it as `canonical_variables = default_canonical_wb_variables()` to
  [`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md),
  so the rendered datatables keep the expected `element.WB_*_` and
  `connectedarea.WB_*_` columns even when every scenario in a batch is
  `NULL` (e.g. the engine returns Status 1 for every input and writes no
  result HDF5).

- `example_wien_minimal` vignette: the per-scenario `run_one()` helper
  now wraps the H5 input write + engine call in `tryCatch` and passes
  `strict = FALSE` + `scalar_strategy = "first"` to
  [`h5_write_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_write_values.md).
  A scenario that errors during write or whose engine returns a non-zero
  status no longer aborts the whole `run_scenarios` loop; the failure is
  reported via [`message()`](https://rdrr.io/r/base/message.html) and
  the remaining scenarios still execute, producing a results datatable
  with NA in the result columns for the failed rows (paired with the new
  mirror-stub above).

- `R/plot_hpond_vs_ref.R`: replace literal `▲` glyph in the caption with
  `▲` so the source file is ASCII-only (R-CMD-check WARNING).

- `R/read_hdf5_timeseries.R`: wrap array-indexing notation (`[1, ]`,
  `[2..k, ]`, `[, 1]`, `[, 2..k]`) in backticks so roxygen2’s markdown
  parser does not turn them into broken `\link{...}` entries.

### Disabled

- `vignettes/workflow.Rmd` and `vignettes/workflow_120min_40mm.Rmd`
  reference the obsolete `Optimierungsfall_kurz.h5` layout and have been
  dot-prefixed (`R CMD build` excludes them as invalid file names). They
  will be revisited in a future release.

## kwb.raindrop 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
