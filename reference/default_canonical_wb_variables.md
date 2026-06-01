# Canonical water-balance variable names emitted by the Tandler engine

Returns the list of water-balance scalar names the Tandler
"Regenwasserbewirtschaftung" engine writes into the `Wasserbilanz` group
of each result HDF5 (without the `element.` / `connectedarea.` prefix
and without the trailing `_` that
[`add_overflow_events_and_waterbalance()`](https://kwb-r.github.io/kwb.raindrop/reference/add_overflow_events_and_waterbalance.md)
appends when building the output columns). Pass this to that function's
`canonical_variables` argument to keep the expected water-balance column
structure visible in the rendered datatable even when every scenario in
a batch is `NULL` (e.g. the engine returns Status 1 for every input and
writes no result HDF5).

## Usage

``` r
default_canonical_wb_variables()
```

## Value

A [`character()`](https://rdrr.io/r/base/character.html) vector of
variable names.

## Source

Values observed in the May 2026 432-row Wien sweep (Michael Rustler) and
referenced explicitly by
[`plot_wb_tradeoff_overflows()`](https://kwb-r.github.io/kwb.raindrop/reference/plot_wb_tradeoff_overflows.md).

## Examples

``` r
default_canonical_wb_variables()
#> [1] "WB_Regen"                            
#> [2] "WB_Evapotranspiration"               
#> [3] "WB_InfiltrationNetto"                
#> [4] "WB_Oberflaechenablauf_Ueberlauf"     
#> [5] "WB_Oberflaechenablauf_Verschaltungen"
```
