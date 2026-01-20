# Trade-off-Plot: Infiltration vs. Evapotranspiration (farbcodiert nach Overflows)

Erstellt einen interaktiven Scatter-Plot (plotly) aus
Optimierungsergebnissen: x = Element-Infiltration %, y =
Element-Evapotranspiration %, Farbe = n_overflows (0/1), Tooltip enthält
zusätzlich die variierenden Parameter aus `param_grid`.

## Usage

``` r
plot_wb_tradeoff_overflows(
  simulation_results_optimisation,
  param_grid,
  filter_n_gt1 = TRUE,
  use_jitter = TRUE,
  jitter_width = 0.15,
  jitter_height = 0.15,
  jitter_seed = 1L,
  digits = 2L,
  digits_params = 4L
)
```

## Arguments

- simulation_results_optimisation:

  Data frame mit Simulationsergebnissen. Benötigte Spalten (mindestens):
  `scenario_name`, `n_overflows`, `sum_overflows`,
  `element.WB_InfiltrationNetto_`, `element.WB_Evapotranspiration_`,
  `element.WB_Oberflaechenablauf_Ueberlauf_`.

- param_grid:

  Data frame mit Parametergrid; muss `scenario_name` enthalten.

- filter_n_gt1:

  Logical; wenn TRUE werden Szenarien mit `n_overflows > 1` entfernt.

- use_jitter:

  Logical; wenn TRUE wird ein leichter Jitter gegen Overplotting
  genutzt.

- jitter_width, jitter_height:

  Numerisch; Jitter-Stärke in x/y (nur wenn `use_jitter = TRUE`).

- jitter_seed:

  Integer; Seed für reproduzierbaren Jitter.

- digits:

  Integer; Rundungsstellen im Tooltip für Wasserbilanzwerte.

- digits_params:

  Integer; Rundungsstellen im Tooltip für Parameterwerte (numerisch).

## Value

Ein `plotly`-Objekt (interaktiv).

## Examples

``` r
if (FALSE) { # \dontrun{
p <- plot_wb_tradeoff_overflows(
  simulation_results_optimisation = simulation_results_optimisation,
  param_grid = param_grid,
  filter_n_gt1 = TRUE,
  use_jitter = TRUE
)
p
} # }
```
