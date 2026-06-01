# Add overflow-event metrics and water-balance shares (percent) to simulation results

Computes (i) overflow event statistics from the element outflow time
series and (ii) water-balance components as percent shares for both
`element` and `connected_area` per scenario.

## Usage

``` r
add_overflow_events_and_waterbalance(
  simulation_results,
  event_separation_hours = 4,
  canonical_variables = NULL
)
```

## Arguments

- simulation_results:

  Named list of scenario results. Each entry is expected to contain:

  - `element$water_balance` with columns `variable`, `value`

  - `connected_area$water_balance` with columns `variable`, `value`

  - `element$rates` with columns `time`, `variable`, `value`

- event_separation_hours:

  Numeric. Minimum time between two overflow events (in hours). Defaults
  to `4`.

- canonical_variables:

  Optional [`character()`](https://rdrr.io/r/base/character.html) vector
  of water-balance variable names (without the `element.` /
  `connectedarea.` prefix and without the trailing `_`), e.g.
  `c("WB_Regen", "WB_Evapotranspiration", "WB_InfiltrationNetto", "WB_Oberflaechenablauf_Ueberlauf", "WB_Oberflaechenablauf_Verschaltungen")`.
  When **every** scenario in `simulation_results` is `NULL` (or
  otherwise provides no water-balance data), the function would normally
  return only the four headline columns. Pass `canonical_variables` to
  attach `element.<var>_` and `connectedarea.<var>_` `NA`-filled stub
  columns to such rows so the rendered datatable still exposes the
  expected column structure. Defaults to `NULL` (no canonical fallback).

## Value

A tibble with one row per scenario containing:

- `s_name`

- `n_overflows`

- `median_duration_overflows_hours`

- `sum_overflows`

- water-balance percentage columns for `element.*_` and
  `connectedarea.*_`

## Details

Water-balance percentages are computed with sign preserved
(`value / denom`):

- **element** denominator:
  `WB_Regen + abs(WB_Oberflaechenablauf_Verschaltungen)`

- **connected_area** denominator: `WB_Regen` (fallback to
  `abs(WB_Oberflaechenablauf_Verschaltungen)` if `WB_Regen` is `NA` or
  `0`)

Overflow events are derived from positive `Oberflaechenablauf_Ueberlauf`
values using
[`kwb.event::hsEvents()`](https://rdrr.io/pkg/kwb.event/man/hsEvents.html).
Assuming the overflow rate is in `mm/h`, event sums (in `mm`) are
obtained by integrating each sample over its **local** time step
(`time[i+1] - time[i]`; the last sample inherits the previous step). A
warning is emitted if the time step is non-uniform, and `sum_overflows`
is returned as `NA` if it cannot be determined (single sample).

Missing components are tolerated: scenarios whose entry in
`simulation_results` is `NULL` (or which lack any of `element`,
`element$water_balance`, `connected_area`,
`connected_area$water_balance` or `element$rates`) still produce a row
of the output tibble. The four "headline" columns (`s_name`,
`n_overflows`, `median_duration_overflows_hours`, `sum_overflows`) are
always present and filled with `NA` where they cannot be computed.

For the `element.*_` and `connectedarea.*_` water-balance columns: if
one side is missing in a given scenario but the other side has data, a
stub of `NA`-filled columns is fabricated for the missing side by
mirroring the populated side's variable names. This preserves the
table's column structure when, for example, every run disables roof ET
(so the engine skips writing Dach.h5 and every scenario has
`connected_area = NULL`): the `connectedarea.*_` columns are kept and
filled with `NA`, instead of being dropped from the output entirely. The
mirror is a best-effort hint for the user, not a guarantee that the
names match what a populated `connected_area` would have produced.

## Examples

``` r
if (FALSE) { # \dontrun{
out <- add_overflow_events_and_waterbalance(simulation_results)
out <- add_overflow_events_and_waterbalance(
  simulation_results,
  event_separation_hours = 6
)
} # }
```
