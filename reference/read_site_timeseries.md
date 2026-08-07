# Read and prepare site rain/ET0 time series for the engine (mm/h)

Factors the time-series preparation duplicated in the Wien and Bad
Aussee workflow vignettes into one helper: reads the shipped GeoSphere
rain series (`rain.csv.gz`: columns `time` (datetime), `rr` (mm per
interval), `station`, further columns tolerated) and reference ET0
series (`et.csv`: `date;value` with `dd.mm.yyyy`, mm per day), converts
both to hours since series start, aligns the series ends (the shorter
series is extended to the longer one's end, repeating its last value)
and converts the values to the engine's **mm/h** rate convention (rain:
mm per interval / interval hours; ET0: mm per day / 24).

## Usage

``` r
read_site_timeseries(path_rain, path_et, verbose = TRUE)
```

## Arguments

- path_rain:

  Path to the rain CSV (may be gzipped).

- path_et:

  Path to the ET0 CSV (semicolon separated).

- verbose:

  Print alignment messages (default TRUE).

## Value

List with data.frames `rain` and `et` (columns `time` = hours since
start, `value` = mm/h) ready for
`make_swale_runner(timeseries_rain = , timeseries_et = )`.

## See also

[`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md)
