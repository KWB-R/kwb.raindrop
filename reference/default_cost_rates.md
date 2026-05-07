# Default unit-cost rates for infiltration-swale installations

Returns the default Austrian unit-cost rates used by
[`compute_costs()`](https://kwb-r.github.io/kwb.raindrop/reference/compute_costs.md).
Rates were provided by Johannes Leimgruber (OeStaP) on 2026-03-27 for
the RAINDROP cost-optimisation work.

## Usage

``` r
default_cost_rates()
```

## Value

A named `list` of rates in EUR per m² or m³: `excavation_eur_per_m3`,
`profiling_eur_per_m2`, `filter_eur_per_m3`,
`infiltration_box_eur_per_m3`, `gravel_trench_eur_per_m3`.

## Source rates (Leimgruber, 2026-03-27, "Kostenansätze Optimierung")

- Aushub (excavation, incl. loading + transport): 70 EUR/m³

- Profilierung und Begrünung (profiling + greening): 10 EUR/m²

- Bodenfilter (soil filter, incl. installation): 200 EUR/m³

- Sickerbox (infiltration box, incl. installation): 350 EUR/m³

- Schotterrigol (gravel trench, incl. installation): 50 EUR/m³
