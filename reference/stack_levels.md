# Achievable storage-layer stack heights from module heights

Enumerates all storage-layer heights that can be built by stacking (and
mixing) the given module heights, e.g. full blocks combined with at most
one half block.

## Usage

``` r
stack_levels(modules, max_count = rep(7L, length(modules)), max_height = 2600)
```

## Arguments

- modules:

  Numeric vector of module heights in mm (e.g. `c(660, 350)` for a full
  block plus a half block).

- max_count:

  Integer vector (recycled to `length(modules)`): maximum number of
  modules of each type in one stack. Defaults to 7 for every module (cf.
  GRAF EcoBloc smart, stackable up to 7 layers).

- max_height:

  Maximum total stack height in mm (default 2600).

## Value

Sorted numeric vector of achievable stack heights in mm.

## Examples

``` r
stack_levels(360)                              # 360, 720, ..., 2520
#> [1]  360  720 1080 1440 1800 2160 2520
stack_levels(c(660, 350), max_count = c(7, 1)) # Rigofill full + half block
#> [1]  350  660 1010 1320 1670 1980 2330
```
