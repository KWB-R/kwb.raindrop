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

Haupteffekte je Parameter (Violin-/Box-/Punkt-Plots, nach Effektstärke
sortiert). Der **Speichertyp** ist als eigenes Panel enthalten
(Sickerbox vs. Schotterrigol).

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

Die beiden **Speichertypen** liegen als **zwei Panels untereinander**
(Sickerbox oben, Schotterrigole unten; die Punkte bleiben Kreise, da die
Panel-Streifen den Typ benennen). Die y-Achsen sind je Panel frei
skaliert, so dass z. B. bei der **Speicherhöhe** jedes Panel nur die für
den Typ getesteten Höhen zeigt (Sickerbox 300–1200 mm, Schotterrigole
900–3600 mm).

| Design Space | Eisenstadt_2005 | Wien | BadAussee |
|----|----|----|----|
| filter_hydraulicconductivity | [filter_hydraulicconductivity](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_design-space_mulde-area_vs_filter_hydraulicconductivity.md) | [filter_hydraulicconductivity](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_design-space_mulde-area_vs_filter_hydraulicconductivity.md) | [filter_hydraulicconductivity](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_design-space_mulde-area_vs_filter_hydraulicconductivity.md) |
| mulde_height | [mulde_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_design-space_mulde-area_vs_mulde_height.md) | [mulde_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_design-space_mulde-area_vs_mulde_height.md) | [mulde_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_design-space_mulde-area_vs_mulde_height.md) |
| storage_height | [storage_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_design-space_mulde-area_vs_storage_height.md) | [storage_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_design-space_mulde-area_vs_storage_height.md) | [storage_height](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_design-space_mulde-area_vs_storage_height.md) |

#### Wasserbilanz

Streudiagramm **Infiltration \[%\]** (x) vs. **Evapotranspiration
\[%\]** (y) je Szenario, Punktfarbe = Anzahl Überlaufereignisse,
Punktform = **Speichertyp** (Viereck = Sickerbox, Dreieck =
Schotterrigole). Der Tooltip zeigt die Wasserbilanz, den Speichertyp und
die variierenden Design-Parameter.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_water-balance.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_water-balance.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_water-balance.md)

#### Kosten

Sechs komplementäre, interaktive Sichten auf die **Baukosten** der
Szenarien und ihren Zusammenhang mit Überläufen, Wasserhaushalt und
Design-Parametern. Alle teilen denselben Punkt-Tooltip (Wasserhaushalt,
Kostenaufteilung, variierende Parameter). Der **Speichertyp** ist
überall einheitlich kodiert: in den Streudiagrammen über die
**Punktform** (Viereck = Sickerbox/Infiltration box, Dreieck =
Schotterrigol/Gravel trench), in den Boxplots über **zwei Panels
untereinander** (Sickerbox oben, Schotterrigol unten).

##### Kosten vs. Überlaufvolumen

Streudiagramm über den kompletten Design-Raum: **x-Achse Gesamtkosten
\[€\]**, **y-Achse Überlaufvolumen \[m³\]** (aus `sum_overflows` \[mm\]
und `mulde_area` \[m²\]), Punktfarbe nach **Anzahl Überlaufereignisse**
(0–5, `>5` = rot, Legende oben), Punktform nach **Speichertyp** (Viereck
= Sickerbox, Dreieck = Schotterrigol). Mouseover zeigt den
Wasserhaushalt (Evapotranspiration, Versickerung, Überlauf in %), das
nutzbare Speichervolumen, die vollständige Kostenaufteilung (Aushub,
Profilierung, Bodenfilter, Speicherschicht, Gesamt), die **Kosten je
Prozent Evapotranspiration über dem Referenz-Minimum der gültigen
Szenarien \[€/%\]** (Referenzwert in der Tooltip-Zeile genannt) plus die
variierenden Design-Parameter des Szenarios.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-vs-overflow-volume.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-vs-overflow-volume.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-vs-overflow-volume.md)

##### Kosten vs. Evapotranspiration

Streudiagramm analog zum vorigen, aber mit der **Evapotranspiration
\[%\]** (Anteil der Evapotranspiration am Gesamtwasserinput des
Elements) auf der y-Achse: **x-Achse Gesamtkosten \[€\]**, Punktfarbe
nach **Anzahl Überlaufereignisse**, Punktform nach **Speichertyp**
(Viereck = Sickerbox, Dreieck = Schotterrigol). Zeigt, wie viel
Evapotranspiration man je Budget bekommt und welche Szenarien dabei
gültig bleiben — identischer Tooltip.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-vs-evaporation.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-vs-evaporation.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-vs-evaporation.md)

Alle Boxplots zeigen die **Gesamtkosten** \[€\] (y-Achse) je **Anzahl
Überlaufereignisse** (x-Achse; `0`–`5` einzeln, `>5` = Rest gebündelt;
im `>5`-Kasten wird das Szenario mit den wenigsten Überläufen markiert),
überlagert mit den Szenarien als Punkte (Kreise; die Panel-Streifen
benennen den Typ), und trennen die beiden **Speichertypen in zwei Panels
untereinander** (Sickerbox oben, Schotterrigol unten). Je Box und Panel
ist ein **bestes** Szenario als Raute in der jeweiligen Gruppenfarbe
(schwarz umrandet) markiert; die Markierungen **aller** Klassen sind je
Panel zur Frontier-Linie verbunden. Punkt-Mouseover: Wasserhaushalt,
Kostenaufteilung, variierende Parameter. Die drei Varianten optimieren
je Box ein **anderes Ziel** (Kosten als Tie-Break) und ergeben so drei
verschiedene Frontier-Linien:

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

##### Boxplot – höchste Evapotranspiration

Das Szenario je Box mit der **höchsten Evapotranspiration** (bei
Gleichstand das günstigste). Die Markierung ist mit ihrer
**Evapotranspiration \[%\]** beschriftet; hier kodiert die **Punktgröße
die Evapotranspiration** statt des Überlaufvolumens.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-by-overflows-boxplot-max-evap.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-by-overflows-boxplot-max-evap.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-by-overflows-boxplot-max-evap.md)

##### Boxplot – Kosten je Prozent Evapotranspiration

**Marginale** Kosteneffizienz der Evapotranspiration: y-Achse sind die
**Kosten je Prozentpunkt Evapotranspiration über dem Referenz-Minimum
\[€/%\]** — Gesamtkosten geteilt durch die Mehr-Evapotranspiration
gegenüber der Referenz. Die Referenz ist die **minimale
Evapotranspiration der gültigen Szenarien** (Anzahl Überlaufereignisse ≤
Schwellenwert; gibt es keine gültigen, der komplette Modelllauf) — diese
Basis-Evapotranspiration ist damit „gratis”; bezahlt wird nur der
Zugewinn. Die **Referenz** (minimale Evapotranspiration \[%\],
Gültigkeitskriterium und zugehörige Szenario-ID) steht in der zweiten
Titelzeile; Szenarien auf oder unter der Referenz (inkl. des
Referenz-Szenarios selbst) haben keine definierte Kennzahl und entfallen
im Plot. Aufteilung je **Anzahl Überlaufereignisse**, wieder mit den
beiden **Speichertypen als zwei Panels untereinander**. Je Box und Panel
ist das Szenario mit den **geringsten Kosten je Prozentpunkt** markiert
und mit seinem Wert \[€/%\] plus dem erkauften Zugewinn **„(+ x.x %
Evapotranspiration)“** beschriftet; die **Punktgröße kodiert die
Evapotranspiration \[%\]**. So lässt sich direkt ablesen, mit welcher
Speichertechnik und welchem Design ein zusätzlicher Prozentpunkt
Evapotranspiration am günstigsten erkauft wird.

- [Eisenstadt_2005](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Eisenstadt_2005_cost-per-evap-boxplot.md)
- [Wien](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_Wien_cost-per-evap-boxplot.md)
- [BadAussee](https://kwb-r.github.io/kwb.raindrop/articles/simulation_results_optimisation_BadAussee_cost-per-evap-boxplot.md)
