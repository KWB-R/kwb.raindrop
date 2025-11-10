# Plot the influence of single-parameter variations on pond height results

Compares simulated scenarios against a reference scenario (default:
`"s00001"`) and plots the deviation of either `h_pond_max` or
`h_pond_mean` for cases in which *exactly one* input parameter differs
from the reference.

The plot title automatically includes the reference scenario's
corresponding pond height value with unit `"mm"`, e.g.
`"Ref: s00001 (h_pond_max = 577.5 mm)"`.

## Usage

``` r
plot_hpond_vs_ref(
  data,
  response = c("h_pond_max", "h_pond_mean"),
  ref_scenario = "s00001",
  diff = c("abs", "pct"),
  tol = 1e-09,
  quiet = FALSE
)
```

## Arguments

- data:

  A data frame containing the simulation results. Must include columns
  `scenario_name`, `h_pond_max`, `h_pond_mean`, and one or more
  parameter columns (e.g. `mulde_area`, `mulde_height`,
  `filter_hyraulicconductivity`, `filter_height`, `storage_height`).

- response:

  Character string; the response variable to plot. One of `"h_pond_max"`
  or `"h_pond_mean"`.

- ref_scenario:

  Character; the scenario ID used as reference (default: `"s00001"`).

- diff:

  Character; type of difference to plot: `"abs"` for absolute difference
  (`value - ref`) or `"pct"` for percentage difference
  (`(value - ref)/ref * 100`).

- tol:

  Numeric; tolerance for numerical comparisons when detecting deviations
  from the reference (default: `1e-9`).

- quiet:

  Logical; if `FALSE` (default), diagnostic messages about the number of
  detected single-parameter variations are printed.

## Value

A `ggplot` object visualizing the effect of single-parameter variations
on the selected response variable.

## Details

The function automatically identifies parameter columns as all columns
not named `scenario_name`, `h_pond_max`, or `h_pond_mean`. It then
selects scenarios where *exactly one* parameter differs from the
reference scenario within the given numerical tolerance. Only those
scenarios are included in the resulting plot.

The output is a `ggplot2` object with one facet per parameter showing
the deviation (`Δ`) of the selected `response` variable from the
reference.

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(
  scenario_name = sprintf("s%05d", 1:22),
  h_pond_max = 577.5244,
  h_pond_mean = 429.5059,
  mulde_area = c(1,10,50,100,500,1000,
                 1,10,50,100,500,1000,
                 1,10,50,100,500,1000,
                 1,10,50,100),
  mulde_height = rep(c(100,200,300,400), each = 6)[1:22],
  filter_hyraulicconductivity = 1,
  filter_height = 150,
  storage_height = 150
)

# Plot absolute deviations of h_pond_max
plot_hpond_vs_ref(df, response = "h_pond_max", ref_scenario = "s00001")

# Plot percentage deviations of h_pond_mean
plot_hpond_vs_ref(df, response = "h_pond_mean", diff = "pct")
} # }
```
