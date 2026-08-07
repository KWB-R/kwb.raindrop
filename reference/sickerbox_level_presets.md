# Sickerbox storage-height presets (brute force default + manufacturers)

Named list of storage-height level vectors (mm) for the infiltration-box
storage layer. `brute_force` is the default used by the workflow
vignettes (300/600/900/1200 mm – itself a combination of several box
types). The manufacturer presets are generated with
[`stack_levels()`](https://kwb-r.github.io/kwb.raindrop/reference/stack_levels.md)
from typical module heights of commercial block systems; verify against
the current data sheets before productive optimisation runs.

## Usage

``` r
sickerbox_level_presets(max_height = 2600)
```

## Arguments

- max_height:

  Maximum total stack height in mm passed to
  [`stack_levels()`](https://kwb-r.github.io/kwb.raindrop/reference/stack_levels.md)
  (default 2600).

## Value

Named list of sorted numeric vectors (mm).
