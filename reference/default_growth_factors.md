# Monthly growth factors for green-roof vegetation (SWMM-UrbanEVA)

Seasonal modulation factors for the vegetation growth curve (`gfdoy`),
one value per calendar month, as derived by Daniel Wicke (KWB) from
SWMM-UrbanEVA (Hoernschemeyer et al.) for the RainDrop green-roof
scenarios. The factors average exactly 1.00 over the year, i.e. they
redistribute the evapotranspiration potential seasonally without
changing its annual total (before water limitation).

## Usage

``` r
default_growth_factors()
```

## Value

Named numeric vector of length 12 (`Jan` ... `Dez`).

## Details

The engine replaces the static `gfdoy_GrowthFactor` completely with the
referenced growth curve (it does not multiply the two), so these factors
are the effective seasonal modulation (Tandler, 2026-10-08, answer 5).

## Examples

``` r
mean(default_growth_factors())  # exactly 1
#> [1] 1
```
