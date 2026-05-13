# kwb.raindrop (development version)

## New features

* `download_engine()` fetches the Tandler "Regenwasserbewirtschaftung"
  Windows executable from the companion repository
  [`KWB-R/kwb.raindrop.binaries`](https://github.com/KWB-R/kwb.raindrop.binaries/releases)
  and caches it under `tools::R_user_dir("kwb.raindrop", "cache")/<version>/`.
  Multiple engine versions can coexist side-by-side because the version is
  encoded in the release tag (`engine-<YYYY-MM-DD>`), not in the asset
  filename. The download is atomic (temp file + rename) and rejects
  obviously broken responses (< 1 KB ⇒ likely a 404 HTML page).

* `compute_costs()` and `default_cost_rates()` attach a per-scenario
  construction-cost breakdown (`cost_excavation`, `cost_profiling`,
  `cost_filter`, `cost_storage`, `cost_total`) to a parameter grid using
  the Austrian unit-cost rates supplied by Johannes Leimgruber (OeStaP,
  2026-03-27). Pass `storage_type = "infiltration_box"` (Sickerbox,
  default, ~95 % porosity) or `"gravel_trench"` (Schotterrigol,
  ~30 % porosity) — or a per-row `storage_type` column on the grid — to
  switch the storage layer. The new columns are wired into the
  `example_wien_minimal`, `workflow_wien` and `workflow_badaussee`
  vignettes so the solution-space datatables become filterable and
  sortable by cost.

* New vignette `example_wien_minimal`: a self-contained smoke test of
  the full input → engine → results loop on Wien. Now extended into
  an ET-diagnostics grid that sweeps three engine switches —
  `keineVerdunstungBeiRegen`, `Hoernschemeyer_aktiv` and the
  `ET0ref_GrasReferenzverdunstung` factor (`0`, `1`, `100`) — at
  Daniel's reference geometry (12 scenarios total). After Daniel's
  XLSX review of the SWIMM-UrbanEva comparison run, the vignette now
  also unconditionally corrects three further `base.h5` defaults on
  every row:
  `Dach/Berechnungsparameter/Evapotranspiration_aktiv = 0`
  (impervious roof, no vegetation; the engine then skips writing
  Dach.h5, see "Minor improvements and bug fixes" below),
  `Mulde_Rigole/Eigenschaften_Oberflaeche/EvapPond = 0`
  (no open-water ET while the grass is submerged), and
  `Mulde_Rigole/Parameter_Evapotranspiration/LAI_LeafAreaIndex = 3.9`
  (Hörnschemeyer grass value, was `8.5`). After the model loop the
  per-scenario `*.h5` inputs are dumped to a single XLSX
  (`raindrop_wien_minimal_params.xlsx`) with one sheet per scenario
  plus a `base` sheet for the un-modified template, a
  `timeseries_info` sheet summarising the rain / ET0 series fed to
  every run (identical across scenarios), and an `applied_settings`
  sheet listing the diff of every key the package writes on top of
  `base.h5` per scenario. Prints a complete static-parameter overview
  from `base.h5` for review of every default that drives the model.
  Designed to render on Windows CI; the four heavy case-study
  vignettes only render their parameter grids on CI and skip the
  model runs.

## Helper scripts

* `inst/scripts/prepare_wien_swmm_timeseries.R` converts the
  shipped Wien rainfall (10-minute, mm) and reference ET0 (daily,
  mm/day) series to SWMM-5 external time-series files
  (`wien_rain.dat`, `wien_et0.dat`) for direct import into a SWMM
  `[TIMESERIES]` / `[RAINGAGES]` / `[EVAPORATION]` block. Output
  directory defaults to `tempdir()`; pass `out_dir` (R) or a positional
  CLI argument (`Rscript`) to redirect.

## Inputs and data shipping

* Per-scenario input data ships under `inst/extdata/models/<scenario>/`
  for Wien, Bad Aussee and Eisenstadt 2005 (`base.h5` HDF5 model
  templates plus `rain.csv.gz` and `et.csv` time series for the two
  GeoSphere-Austria sites). Vignettes read inputs via `system.file()`
  and write model outputs to a `tempdir()` scratch root, so they are
  hermetic and reproducible.

* Rain timeseries are shipped gzipped (`rain.csv.gz`); `readr::read_csv()`
  reads the compressed files transparently. Total `inst/extdata/`
  footprint is ~6.8 MB (down from ~53 MB raw).

* Provenance documented in `inst/extdata/SOURCES.md` and an
  "Input data" section in each rendered vignette: precipitation and
  evapotranspiration come from
  [GeoSphere Austria](https://www.geosphere.at/) (Österreichischer
  Wetterdienst, formerly ZAMG); HDF5 templates are produced with the
  Tandler engine.

## CI / packaging

* GitHub Actions workflows for Claude Code (`@claude` mention bot and
  automatic PR review) added; both pinned to Opus 4.7 with `think hard`
  reasoning on the review prompt.

* `R-CMD-check` matrix restricted to `windows-latest`
  (devel/oldrel/release) — the calculation engine is a Windows `.exe`,
  so non-Windows runners cannot exercise the workflow. Aligns with the
  existing `test-coverage` and `pkgdown` jobs.

* Bumped `actions/checkout` to v5 and pinned `actions/upload-artifact`
  to v4 (Node.js 24 readiness ahead of the 2026-09-16 Node 20
  removal).

## Dependency hygiene

* `tidyr`, `rlang`: moved to `Imports` (used in package code).
* `plotly`: moved from `Imports` to `Suggests` (only used in vignettes).
* `htmlwidgets`, `readr`, `writexl`: added to `Suggests` (used in
  vignettes).

## Bug fixes

* `get_simulation_results_optim()` and
  `get_simulation_results_optim_parallel()` no longer return `NULL`
  when only the connected-area H5 (Dach.h5) is missing; they now
  return a partial result with `connected_area = NULL` while still
  populating the element side. This unblocks scenarios where
  `//Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv`
  is `0` and the engine consequently skips writing Dach.h5.
* `add_overflow_events_and_waterbalance()` tolerates per-scenario
  `NULL` and missing components (`element`, `connected_area`,
  `*$water_balance`, `element$rates`). Affected scenarios still
  produce a row of the output tibble with the available metrics
  computed and the missing columns left as `NA`.

* `add_overflow_events_and_waterbalance()` now fabricates an
  `NA`-filled column stub when one side's water balance is missing
  while the other side has data, by mirroring the populated side's
  variable names. Previously the missing side's columns were
  dropped entirely (`dplyr::bind_rows()` only adds columns that at
  least one scenario contributes), which left the results table
  with no `connectedarea.*_` columns at all when every scenario
  disabled roof ET. The mirror keeps the column structure visible
  in the rendered datatable.
* `R/plot_hpond_vs_ref.R`: replace literal `▲` glyph in the caption
  with `▲` so the source file is ASCII-only (R-CMD-check WARNING).
* `R/read_hdf5_timeseries.R`: wrap array-indexing notation
  (`[1, ]`, `[2..k, ]`, `[, 1]`, `[, 2..k]`) in backticks so roxygen2's
  markdown parser does not turn them into broken `\link{...}` entries.

## Disabled

* `vignettes/workflow.Rmd` and `vignettes/workflow_120min_40mm.Rmd`
  reference the obsolete `Optimierungsfall_kurz.h5` layout and have
  been dot-prefixed (`R CMD build` excludes them as invalid file
  names). They will be revisited in a future release.

# kwb.raindrop 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
