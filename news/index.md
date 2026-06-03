# Changelog

## kwb.raindrop (development version)

### Bug fixes

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
  `keineVerdunstungBeiRegen`, `Hoernschemeyer_aktiv` and the
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
