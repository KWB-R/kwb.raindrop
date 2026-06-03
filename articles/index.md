# RainDrop Optimierung – Brute Force

## Hintergrund

xxx

## Methodik

Die Modellierung erfolgte in R mit dem R Paket
[kwb.raindrop](https://github.com/kwb-r/kwb.raindrop). Das genaue
Vorgehen ist für jede Fallstudie im folgenden im R Markdown
reproduzierbar dokumentiert.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/workflow_Eisenstadt_2005.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/workflow_Wien.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/workflow_BadAussee.md)

## Ergebnisse

Die Ergebnisse für die einjährige Berechnung (Eisenstadt für Jahr 2005)
und die beiden 15 jährigen Zeitreihen (2011-2025) für Wien und Bad
Aussee finden sich in unten stehenden Links:

### Tabellen

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee.md)

### CSV

Die in den [obenstehenden Tabellen](#tabellen) dargestellten Ergebnisse
können auch als `.csv` Datei heruntergeladen werden.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005.csv)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien.csv)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee.csv)

### Interaktive Visualisierungen

#### Sensitive Modellparameter

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_main-effects.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_main-effects.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_main-effects.md)

#### Design Spaces

In den nachfolgende Abbildungen wird die **Muldenfläche** (x-Achse) mit
**einem weiteren Parameter** (y-Achse) dargestellt. Diese sind im
folgenden:

- ***Muldenhöhe***

- ***Speicherhöhe***

- ***hydraulische Leitfähigkeit*** des Bodenfilters

| Design Space | Eisenstadt_2005 | Wien | BadAussee |
|----|----|----|----|
| filter_hydraulicconductivity | [filter_hydraulicconductivity](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_design-space_mulde-area_vs_filter_hydraulicconductivity.md) | [filter_hydraulicconductivity](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_design-space_mulde-area_vs_filter_hydraulicconductivity.md) | [filter_hydraulicconductivity](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_design-space_mulde-area_vs_filter_hydraulicconductivity.md) |
| mulde_height | [mulde_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_design-space_mulde-area_vs_mulde_height.md) | [mulde_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_design-space_mulde-area_vs_mulde_height.md) | [mulde_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_design-space_mulde-area_vs_mulde_height.md) |
| storage_height | [storage_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_design-space_mulde-area_vs_storage_height.md) | [storage_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_design-space_mulde-area_vs_storage_height.md) | [storage_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_design-space_mulde-area_vs_storage_height.md) |

#### Wasserbilanz

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_water-balance.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_water-balance.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_water-balance.md)
