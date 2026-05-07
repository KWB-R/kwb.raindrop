# Compute construction costs for an infiltration-swale parameter grid

Given a parameter grid that drives the simulation, attach a per-scenario
breakdown of construction costs plus a `cost_total` column suitable for
filtering / sorting in the results datatable.

## Usage

``` r
compute_costs(
  param_grid,
  storage_type = c("infiltration_box", "gravel_trench"),
  cost_rates = default_cost_rates()
)
```

## Arguments

- param_grid:

  `data.frame` / `tibble` — must contain the four geometry columns
  listed above. Other columns are passed through unchanged.

- storage_type:

  `character(1)` — `"infiltration_box"` (default; ~95% porosity, the
  Austrian "Sickerbox") or `"gravel_trench"` (~30% porosity, the
  Austrian "Schotterrigol"). Only used when `param_grid` does not have a
  `storage_type` column.

- cost_rates:

  `list` — unit costs as returned by
  [`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md).
  Override individual entries to run cost sensitivity analyses.

## Value

The input `param_grid` with the columns `storage_type`,
`cost_excavation`, `cost_profiling`, `cost_filter`, `cost_storage`,
`cost_total` (all in EUR) appended.

## Details

Required columns in `param_grid`:

- `mulde_area` — element footprint area in m²

- `mulde_height` — surface depression depth in **mm**

- `filter_height` — soil-filter layer thickness in **mm**

- `storage_height` — storage layer thickness in **mm**

Optional column `storage_type` (character: `"infiltration_box"` or
`"gravel_trench"`) selects the per-scenario storage rate. If absent, the
value of the `storage_type` argument is used for every row.

Cost formulas (Leimgruber, 2026-03-27):


      cost_excavation = mulde_area * (mulde_h + filter_h + storage_h)/1000 * 70
      cost_profiling  = mulde_area * 10
      cost_filter     = mulde_area * filter_h/1000 * 200
      cost_storage    = mulde_area * storage_h/1000 * <350 | 50>
      cost_total      = sum of the four above

## Examples

``` r
grid <- tibble::tibble(
  scenario_name = c("s00001", "s00002"),
  mulde_area = c(50, 50),
  mulde_height = c(300, 300),
  filter_height = c(300, 300),
  storage_height = c(600, 600)
)

# Infiltration box (default)
compute_costs(grid)
#> # A tibble: 2 × 11
#>   scenario_name mulde_area mulde_height filter_height storage_height
#>   <chr>              <dbl>        <dbl>         <dbl>          <dbl>
#> 1 s00001                50          300           300            600
#> 2 s00002                50          300           300            600
#> # ℹ 6 more variables: storage_type <chr>, cost_excavation <dbl>,
#> #   cost_profiling <dbl>, cost_filter <dbl>, cost_storage <dbl>,
#> #   cost_total <dbl>

# Gravel trench
compute_costs(grid, storage_type = "gravel_trench")
#> # A tibble: 2 × 11
#>   scenario_name mulde_area mulde_height filter_height storage_height
#>   <chr>              <dbl>        <dbl>         <dbl>          <dbl>
#> 1 s00001                50          300           300            600
#> 2 s00002                50          300           300            600
#> # ℹ 6 more variables: storage_type <chr>, cost_excavation <dbl>,
#> #   cost_profiling <dbl>, cost_filter <dbl>, cost_storage <dbl>,
#> #   cost_total <dbl>

# Per-scenario storage type
compute_costs(dplyr::mutate(grid,
                            storage_type = c("infiltration_box",
                                             "gravel_trench")))
#> # A tibble: 2 × 11
#>   scenario_name mulde_area mulde_height filter_height storage_height
#>   <chr>              <dbl>        <dbl>         <dbl>          <dbl>
#> 1 s00001                50          300           300            600
#> 2 s00002                50          300           300            600
#> # ℹ 6 more variables: storage_type <chr>, cost_excavation <dbl>,
#> #   cost_profiling <dbl>, cost_filter <dbl>, cost_storage <dbl>,
#> #   cost_total <dbl>
```
