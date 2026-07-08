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

Als **Gültigkeitskriterium** — die maximal zulässige Anzahl
Überlaufereignisse — wurde passend zur Simulationsdauer gewählt:
**Eisenstadt ≤ 1** (Simulationszeit 1 Jahr) und **Wien / Bad Aussee ≤
5** (15-jährige Regen-/ET-Reihe). Dieser Schwellenwert steuert die
Farbgebung (grün = gültig, rot = zu viele Überläufe) und den in den
Kostenplot-Titeln angegebenen Anteil gültiger Szenarien.

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

#### Kosten

Drei komplementäre, interaktive Sichten auf die **Baukosten** der
Szenarien und ihren Zusammenhang mit Überläufen, Wasserhaushalt und
Design-Parametern. Alle drei teilen denselben Punkt-Tooltip
(Wasserhaushalt, Kostenaufteilung, variierende Parameter).

##### Kosten vs. Überlaufvolumen

Streudiagramm über den kompletten Design-Raum: **x-Achse Gesamtkosten
\[€\]**, **y-Achse Überlaufvolumen \[m³\]** (aus `sum_overflows` \[mm\]
und `mulde_area` \[m²\]), Punktfarbe nach **Anzahl Überlaufereignisse**
(0–5, `>5` = rot, Legende oben). Mouseover zeigt den Wasserhaushalt
(Verdunstung, Versickerung, Überlauf in %), die vollständige
Kostenaufteilung (Aushub, Profilierung, Bodenfilter, Speicherschicht,
Gesamt) plus die variierenden Design-Parameter des Szenarios.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-vs-overflow-volume.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-vs-overflow-volume.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-vs-overflow-volume.md)

Alle Boxplots zeigen die **Gesamtkosten** \[€\] (y-Achse) je **Anzahl
Überlaufereignisse** (x-Achse; `0`–`5` einzeln, `>5` = Rest gebündelt;
im `>5`-Kasten wird das Szenario mit den wenigsten Überläufen markiert),
überlagert mit den Szenarien als Punkte. Je Box ist ein **bestes**
Szenario als Raute in der jeweiligen Gruppenfarbe (schwarz umrandet)
markiert; die Markierungen **aller** Klassen sind zur Frontier-Linie
verbunden. Punkt-Mouseover: Wasserhaushalt, Kostenaufteilung,
variierende Parameter. Die drei Varianten optimieren je Box ein
**anderes Ziel** (Kosten als Tie-Break) und ergeben so drei verschiedene
Frontier-Linien:

##### Boxplot – günstigste je Kategorie

Das **günstigste** Szenario je Box. Punktgröße = Überlaufvolumen \[m³\].

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-by-overflows-boxplot-cheapest.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-by-overflows-boxplot-cheapest.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-by-overflows-boxplot-cheapest.md)

##### Boxplot – geringstes Überlaufvolumen

Das Szenario je Box mit dem **geringsten Überlaufvolumen** (bei
Gleichstand das günstigste). Die Markierung ist mit ihrem
**Überlaufvolumen \[m³\] und dessen Anteil \[%\]** beschriftet.
Punktgröße = Überlaufvolumen.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-by-overflows-boxplot-min-overflow.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-by-overflows-boxplot-min-overflow.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-by-overflows-boxplot-min-overflow.md)

##### Boxplot – höchste Verdunstung

Das Szenario je Box mit der **höchsten Verdunstung** (bei Gleichstand
das günstigste). Die Markierung ist mit ihrer **Verdunstung \[%\]**
beschriftet; hier kodiert die **Punktgröße die Verdunstung** statt des
Überlaufvolumens.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-by-overflows-boxplot-max-evap.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-by-overflows-boxplot-max-evap.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-by-overflows-boxplot-max-evap.md)
