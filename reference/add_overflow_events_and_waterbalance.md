# Add overflow-event metrics and water-balance shares (percent) to simulation results

Computes (i) overflow event statistics from the element outflow time
series and (ii) water-balance components as *percent shares* for both
`element` and `connected_area` per scenario.

## Usage

``` r
add_overflow_events_and_waterbalance(
  simulation_results,
  event_separation_hours = 4
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
Event separation is controlled via `event_separation_hours` (converted
to seconds).

## Examples

``` r
if (FALSE) { # \dontrun{
out <- add_overflow_events_and_waterbalance(simulation_results)
out <- add_overflow_events_and_waterbalance(simulation_results, event_separation_hours = 6)
} # }
```
