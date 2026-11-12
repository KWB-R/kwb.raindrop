# Build a monthly step growth curve on the engine's time axis

Expands 12 monthly factors into the `data.frame(time, value)` layout the
engine expects for `//Kurven/*` datasets: `time` in hours since
simulation start (the first timestamp of the rain series), one support
point per month boundary inside the simulated period, plus a terminal
point at `end_time_hours`. The engine holds each value from its support
point until the next one (step interpolation), so month boundaries are
the correct support-point locations.

## Usage

``` r
make_growth_curve(origin, end_time_hours, factors = default_growth_factors())
```

## Arguments

- origin:

  `POSIXct`/`Date` (or anything
  [`lubridate::as_datetime()`](https://lubridate.tidyverse.org/reference/as_date.html)
  accepts): the calendar timestamp of simulation hour 0, i.e. the first
  timestamp of the rain series fed to `//Kurven/Regen`.

- end_time_hours:

  Numeric. End of the curve in hours since `origin`; pin this to
  `max(timeseries_rain$time)` exactly as the workflows already do for
  `Growth_1` / `Shading_1`.

- factors:

  Named numeric vector of length 12, January first (default
  [`default_growth_factors()`](https://kwb-r.github.io/kwb.raindrop/reference/default_growth_factors.md)).

## Value

`data.frame` with columns `time` (hours since `origin`) and `value`,
ready to be assigned to a `//Kurven/*` entry of an
[`h5_read_values()`](https://kwb-r.github.io/kwb.raindrop/reference/h5_read_values.md)
list.

## Examples

``` r
curve <- make_growth_curve("2011-01-01", end_time_hours = 2 * 8760)
head(curve)
#>   time value
#> 1    0  0.68
#> 2  744  0.68
#> 3 1416  0.68
#> 4 2160  1.01
#> 5 2880  1.18
#> 6 3624  1.35
```
