# Default storage specification for the swale-design optimiser

Storage-layer search space per storage type: the infiltration box uses
discrete stack levels (default: the brute-force grid levels), the gravel
trench is continuous with bounds coupled to the box level range by
`coupling_factor` (default 3, approximating the usable-porosity ratio
0.95 / 0.3).

## Usage

``` r
default_storage_spec(
  levels = sickerbox_level_presets()$brute_force,
  coupling_factor = 3,
  gravel_tol = 25
)
```

## Arguments

- levels:

  Numeric vector of infiltration-box stack heights in mm.

- coupling_factor:

  Factor between gravel-trench bounds and the box level range.

- gravel_tol:

  Bisection tolerance for the continuous gravel-trench height in mm.

## Value

Named list with entries `infiltration_box` (with `levels` and
`porosity`) and `gravel_trench` (with `bounds`, `tol` and `porosity`).

## Details

Each entry also carries the **usable porosity** of the storage layer
(box 0.95, trench 0.3, matching
[`default_storage_types()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_types.md)).
The bisection optimiser uses it to *derive* its search order from the
cost rates (cost per mm of storage capacity); without a `porosity` entry
it falls back to the default-rate hierarchy (smallest storage level
first).
