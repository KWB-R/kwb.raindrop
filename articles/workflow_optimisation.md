# Workflow Optimierung (Eisenstadt · Wien · Bad Aussee)

## Ziel

Dieser Workflow findet für **alle drei Standorte** (Eisenstadt 2005,
Wien, Bad Aussee) die **günstigste Muldenkonfiguration je Überlaufziel**
x = 0…5 — pro Speichertyp (Sickerbox / Schotterrigol) — mit
[`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
statt eines Brute-Force-Rasters. Das Verfahren ist reine Bisektion
(“Zahlenraten”): Fläche schrumpfen, dann Muldentiefe, Speicher nur
erhöhen, wenn die Fläche am Anschlag klemmt. Voraussetzung ist die in
der Vignette `monotonicity_analysis` belegte Monotonie (“größer = nie
mehr Überläufe”); die dort abgeleiteten Absicherungen (Rand-Guard,
Volumen-Schiedsrichter) sind in
[`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md)
eingebaut. Eine unabhängige **Gegenprobe ohne Monotonie-Annahme** —
simultane Optimierung aller Parameter, plus weitere Suchverfahren zum
Vergleich — führt die separate Vignette
`workflow_optimisation_simultaneous` durch; sie liest die hier
exportierte CSV als Vergleichsbasis.

Die Filterdurchlässigkeit wird fest auf das Maximum gesetzt (kostenfrei
dominant: gleiche Verdunstung, nie mehr Überläufe), Fläche und
Muldentiefe werden stufenlos gesucht, die Sickerbox in den
Default-Stufen 300/600/900/1200 mm
([`sickerbox_level_presets()`](https://kwb-r.github.io/kwb.raindrop/reference/sickerbox_level_presets.md)
bietet Hersteller-Alternativen), der Schotterrigol stufenlos im 3-fachen
Box-Bereich.

**Laufzeit:** Ein Engine-Lauf dauert ~2 s (Eisenstadt, 1 Jahr) bzw. ~15
s (Wien / Bad Aussee, 15-Jahres-Serien). Die 6 Tasks (Standort ×
Speichertyp) laufen parallel; die Gesamtdauer entspricht dem längsten
Einzeltask — ca. 15–20 Minuten.
[`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md)
löscht die Szenario-Dateien jedes Laufs standardmäßig direkt nach dem
Einlesen (`cleanup = TRUE`), damit lange Suchen das Temp-Laufwerk nicht
füllen.

> **Hinweis:** Die Rechen-Chunks wurden übersprungen. Prüfungen:
> Windows: TRUE · CI/GitHub Actions: TRUE · base.h5 gefunden: TRUE
> (D:/a/\_temp/Library/kwb.raindrop/extdata/models/eisenstadt-2005/base.h5).
> Auf einem lokalen Windows-Rechner sollten alle drei Bedingungen
> erfüllt sein — falls base.h5 fehlt: Vignette aus dem Paket-Repo heraus
> rendern oder das Paket installieren.

## Standort-Konfiguration

Die Standorte unterscheiden sich nur in zwei Punkten: dem
Modell-Template (`base.h5`) und der Frage, ob eigene
Regen-/ET0-Zeitreihen in die Engine geschrieben werden (Wien und Bad
Aussee) oder die Kurven aus `base.h5` verwendet werden (Eisenstadt).
Beides kapselt
[`make_swale_runner()`](https://kwb-r.github.io/kwb.raindrop/reference/make_swale_runner.md);
die Zeitreihen-Aufbereitung (mm/h-Konvention, Serien-Angleichung)
übernimmt
[`read_site_timeseries()`](https://kwb-r.github.io/kwb.raindrop/reference/read_site_timeseries.md).

``` r

# Im Quellbaum die Entwicklungsversion laden (immer aktuell, auch wenn
# das installierte Paket aelter ist); ausserhalb: installiertes Paket.
if (file.exists("../DESCRIPTION") &&
    requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all("..", quiet = TRUE)
} else {
  library(kwb.raindrop)
}

sites <- list(
 Eisenstadt_2005 = list(dir = "eisenstadt-2005", timeseries = FALSE,
   prior = "simulation_results_optimisation_Eisenstadt_2005.csv"),
 Wien = list(dir = "wien", timeseries = TRUE,
   prior = "simulation_results_optimisation_Wien.csv"),
 BadAussee = list(dir = "badaussee", timeseries = TRUE,
   prior = "simulation_results_optimisation_BadAussee.csv")
)

fixed <- list(connected_area = 1000,
              filter_height = 300,
              filter_hydraulicconductivity = 360,  # Rastermaximum, gratis
              bottom_hydraulicconductivity = 12)

make_path_list <- function(modelname, model_dir) {
  list(
    modelname = modelname,
    root_path = file.path(tempdir(), paste0("raindrop_opt_", model_dir)),
    dir_input  = "<root_path>/models/<modelname>/input",
    dir_output = "<root_path>/models/<modelname>/output",
    dir_target_output = "<dir_output>/<dir_target>",
    file_errors_hdf5 = "Fehlerprotokoll.h5",
    file_results_hdf5_element = "Mulde_Rigole.h5",
    file_results_hdf5_flaeche = "Dach.h5",
    file_results_hdf5_verschaltungen = "<dir_target>_Verschaltungen.h5",
    file_results_txt = "Mulde_Rigole_RAINDROP.txt",
    file_results_txt_multilayer = "Mulde_Rigole_RAINDROP_multi_layer.txt",
    file_target = "<dir_target>.h5",
    path_base = extdata_path("models", model_dir, "base.h5"),
    path_exe  = download_engine(),
    path_errors_hdf5 = "<dir_target_output>/<file_errors_hdf5>",
    path_results_hdf5_element = "<dir_target_output>/<file_results_hdf5_element>",
    path_results_hdf5_flaeche = "<dir_target_output>/<file_results_hdf5_flaeche>",
    path_results_hdf5_verschaltungen = "<dir_target_output>/<file_results_hdf5_verschaltungen>",
    path_results_txt = "<dir_target_output>/<file_results_txt>",
    path_results_txt_multilayer = "<dir_target_output>/<file_results_txt_multilayer>",
    path_target_input = "<dir_input>/<file_target>"
  )
}
```

## Suchraum und Kostensätze

Die Suchbereiche der variablen Parameter sind die Default-Argumente von
[`optimise_swale_design()`](https://kwb-r.github.io/kwb.raindrop/reference/optimise_swale_design.md)
— bewusst identisch mit den min/max-Bereichen des Brute-Force-Rasters,
denn nur dort ist die Monotonie geprüft. Hier stehen sie explizit, damit
sie sichtbar und anpassbar sind:

``` r

area_bounds   <- c(25, 200)   # Muldenflaeche [m2], stufenlos
area_tol      <- 2            # Aufloesung der Flaechensuche [m2];
                              # jede Halbierung kostet nur 1 Lauf je
                              # Flaechensuche und halbiert den
                              # toleranzbedingten Kostenueberschuss
height_bounds <- c(100, 300)  # Muldentiefe [mm], stufenlos
height_tol    <- 10           # Aufloesung der Tiefensuche [mm]
storage_spec  <- default_storage_spec()
# Alternativ Hersteller-Stufen, z. B.:
# storage_spec <- default_storage_spec(
#   levels = sickerbox_level_presets()$graf_ecobloc_smart)

knitr::kable(tibble::tibble(
  Parameter = c("mulde_area [m2]", "mulde_height [mm]",
                "storage_height Sickerbox [mm]",
                "storage_height Schotterrigol [mm]",
                "filter_hydraulicconductivity [mm/h]",
                "filter_height [mm]"),
  Suchraum = c(
    sprintf("stufenlos %g bis %g (Toleranz %g)",
            area_bounds[1], area_bounds[2], area_tol),
    sprintf("stufenlos %g bis %g (Toleranz %g)",
            height_bounds[1], height_bounds[2], height_tol),
    paste("Stufen:",
          paste(storage_spec$infiltration_box$levels, collapse = " / ")),
    sprintf("stufenlos %g bis %g (Toleranz %g)",
            storage_spec$gravel_trench$bounds[1],
            storage_spec$gravel_trench$bounds[2],
            storage_spec$gravel_trench$tol),
    "fix = 360 (Rastermaximum; kostenfrei dominant)",
    "fix = 300"
  )
))
```

Die Kostensätze sind die Defaults nach Leimgruber (2026-03-27,
[`default_cost_rates()`](https://kwb-r.github.io/kwb.raindrop/reference/default_cost_rates.md));
einzelne Sätze lassen sich hier überschreiben — gerechnet wird zunächst
mit den Defaults. Die Suchreihenfolge der Bisektion wird dabei **aus den
Sätzen hergeleitet** (Spezifikkosten-Proxy: € je mm Speicherkapazität,
Kapazitätsmodell V ≈ Fläche × (Tiefe + Porosität × Speicher); die
Porosität liefert
[`default_storage_spec()`](https://kwb-r.github.io/kwb.raindrop/reference/default_storage_spec.md)):
Die Muldentiefe zu maximieren ist unter diesem Kostenmodell für *jeden*
Satz optimal, die Start-Speicherstufe wählt der Proxy — kleinste Stufe
bei den Default-Sätzen, hohe Stufe z. B. bei billigem Speichermaterial.
Der Proxy ist eine Näherung erster Ordnung (kapazitäts-additive Hebel);
die annahmefreie Gegenprobe — und das Mittel der Wahl bei nichtlinearen
Hebeln — bleibt die Vignette `workflow_optimisation_simultaneous`:

``` r

cost_rates <- default_cost_rates()
# Beispiel fuer eine Anpassung (auskommentiert):
# cost_rates$excavation_eur_per_m3       <- 85
# cost_rates$infiltration_box_eur_per_m3 <- 400

knitr::kable(
  tibble::tibble(Kostensatz = names(cost_rates),
                 `EUR je m2/m3` = unlist(cost_rates)),
  caption = "Kostensaetze (inkl. Einbau)"
)
```

## Optimierung aller Standorte (parallel)

Parallelisiert wird über **Standort × Speichertyp = 6 unabhängige
Tasks**: Die Bisektion innerhalb einer Suche ist prinzipbedingt
sequentiell, und die x-Ziele eines Speichertyps teilen sich den
Evaluations-Cache — aber Box- und Rigol-Läufe teilen keinen einzigen
Engine-Lauf (der Cache-Schlüssel enthält den Typ). Jeder Worker ist ein
eigener R-Prozess mit eigenem
[`tempdir()`](https://rdrr.io/r/base/tempfile.html), Kollisionen sind
damit ausgeschlossen. Die Wall-Time entspricht dem längsten Einzeltask
(Wien/Rigol, ~15–20 min) statt der Summe aller Tasks (~65 min).

Liegen die Rasterergebnisse der Workflow-Vignetten als CSV neben dieser
Vignette, verengen sie als Warmstart die erste Flächensuche auf einen
25-m²-Rasterschritt.

``` r

t_opt_start <- Sys.time()

tasks <- expand.grid(site = names(sites), type = names(storage_spec),
                     stringsAsFactors = FALSE)

future::plan(future::multisession,
             workers = min(nrow(tasks),
                           max(1, parallel::detectCores() - 1)))

opt_list <- future.apply::future_lapply(seq_len(nrow(tasks)), function(i) {
  site <- tasks$site[i]
  type <- tasks$type[i]

  # Worker = eigener Prozess: Paket dort genauso laden wie im Hauptprozess
  if (file.exists("../DESCRIPTION") &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all("..", quiet = TRUE)
  } else {
    library(kwb.raindrop)
  }

  cfg <- sites[[site]]
  ts <- if (cfg$timeseries) {
    read_site_timeseries(
      extdata_path("models", cfg$dir, "rain.csv.gz"),
      extdata_path("models", cfg$dir, "et.csv"),
      verbose = FALSE
    )
  } else {
    NULL
  }

  run_fn <- make_swale_runner(make_path_list(site, cfg$dir),
                              timeseries_rain = ts$rain,
                              timeseries_et = ts$et)

  prior <- if (file.exists(cfg$prior)) {
    readr::read_csv(cfg$prior, show_col_types = FALSE)
  } else {
    NULL
  }

  t0 <- Sys.time()
  opt <- optimise_swale_design(run_fn, x_targets = 0:5,
                               area_bounds = area_bounds,
                               area_tol = area_tol,
                               height_bounds = height_bounds,
                               height_tol = height_tol,
                               storage_spec = storage_spec[type],
                               fixed = fixed,
                               prior_results = prior,
                               cost_rates = cost_rates,
                               verbose = FALSE)
  opt$site <- site
  opt$n_runs_task <- attr(opt, "n_runs_total")
  opt$minutes_task <- round(as.numeric(
    difftime(Sys.time(), t0, units = "mins")), 1)
  opt
}, future.seed = TRUE)

future::plan(future::sequential)
opt_all <- dplyr::bind_rows(opt_list)

t_opt_end <- Sys.time()
```

## Ergebnis: günstigstes Design je Standort und Überlaufziel

``` r

knitr::kable(
  opt_all[, c("site", "x", "storage_type", "status", "mulde_area",
              "mulde_height", "storage_height", "n_overflows",
              "overflow_volume_m3", "et_pct", "cost_total")],
  digits = c(NA, 0, NA, NA, 1, 0, 0, 0, 1, 1, 0)
)
```

``` r

library(ggplot2)

ok <- opt_all[opt_all$status == "ok", ]
ggplot(ok, aes(x, cost_total / 1000, colour = storage_type)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ site, scales = "free_y") +
  scale_x_continuous(breaks = 0:5) +
  labs(title = "Kosten-Wirksamkeits-Kurven: Was kostet ein Ueberlauf weniger?",
       x = "Ueberlaufziel x (zulaessige Ereignisse)",
       y = "Kosten Optimum [Tsd. EUR]",
       colour = "Speichertyp",
       caption = cost_rates_caption("de", cost_rates)) +
  theme_bw()
```

``` r

readr::write_csv(opt_all, "optimisation_results_all_sites.csv")
for (site in unique(opt_all$site)) {
  readr::write_csv(opt_all[opt_all$site == site, ],
                   sprintf("optimisation_results_%s.csv", site))
}

task_stats <- unique(opt_all[, c("site", "storage_type", "n_runs_task",
                                 "minutes_task")])
knitr::kable(
  task_stats,
  col.names = c("Standort", "Speichertyp", "Engine-Laeufe", "Minuten")
)
```

## Monte-Carlo-Analyse: Wie robust ist die Suche selbst?

Die Bisektion ist deterministisch: gleiche Eingaben, gleicher Pfad,
gleiches Ergebnis. Die Monte-Carlo-Frage lautet deshalb: **Hängt das
gefundene Optimum vom Suchpfad ab?** Dazu wird der Teilungspunkt jeder
Bisektion zufällig verschoben (`split_jitter = 0.3`: Teilung zufällig
zwischen 20 % und 80 % des Intervalls statt exakt mittig) und die
Optimierung `n_mc`-mal mit unterschiedlichen Seeds wiederholt — **Regen,
Kostensätze und alle übrigen Eingaben bleiben unverändert** (x = 1, ohne
Warmstart, damit jede Wiederholung den vollen Suchraum durchläuft).
Gerechnet wird der volle Pool **3 Standorte × 2 Speichertypen × `n_mc`
Wiederholungen = 60 unabhängige Tasks**, alle parallel (je Task eine
komplette Neu-Optimierung mit ~15 Engine-Läufen; Wall-Time je nach
Kernzahl ~20–35 min). Erwartung, wenn das Optimum eine Eigenschaft des
Problems ist — und nicht des Wegs, den die Suche genommen hat:
**identische Speicherstufe, Flächen-Spanne ≤ 2 × `area_tol`,
Kostenspanne von wenigen Prozent**. Die Muldentiefe darf etwas weiter
streuen als 2 × `height_tol`: Sie ist über die Hydraulik an die
gefundene Fläche gekoppelt (eine um `area_tol` größere Fläche erlaubt
eine entsprechend geringere Tiefe), sodass sich dort die Toleranzen
beider Suchen addieren.

``` r

n_mc <- 10
mc_seeds <- 1:n_mc
```

``` r

t_mc_start <- Sys.time()

mc_tasks <- expand.grid(site = names(sites), type = names(storage_spec),
                        rep = seq_len(n_mc), stringsAsFactors = FALSE)

future::plan(future::multisession,
             workers = min(nrow(mc_tasks),
                           max(1, parallel::detectCores() - 1)))

mc_search <- future.apply::future_lapply(seq_len(nrow(mc_tasks)), function(i) {
  site <- mc_tasks$site[i]
  type <- mc_tasks$type[i]
  rep  <- mc_tasks$rep[i]

  if (file.exists("../DESCRIPTION") &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all("..", quiet = TRUE)
  } else {
    library(kwb.raindrop)
  }
  set.seed(mc_seeds[rep])

  cfg <- sites[[site]]
  ts <- if (cfg$timeseries) {
    read_site_timeseries(
      extdata_path("models", cfg$dir, "rain.csv.gz"),
      extdata_path("models", cfg$dir, "et.csv"),
      verbose = FALSE
    )
  } else {
    NULL
  }
  run_fn <- make_swale_runner(make_path_list(paste0(site, "_MC"), cfg$dir),
                              timeseries_rain = ts$rain,
                              timeseries_et = ts$et)

  opt <- optimise_swale_design(
    run_fn, x_targets = 1,
    area_bounds = area_bounds, area_tol = area_tol,
    height_bounds = height_bounds, height_tol = height_tol,
    storage_spec = storage_spec[type],
    fixed = fixed,
    split_jitter = 0.3,
    cost_rates = cost_rates, verbose = FALSE
  )
  opt$site <- site
  opt$rep <- rep
  opt$n_runs_rep <- attr(opt, "n_runs_total")
  opt
}, future.seed = TRUE)

future::plan(future::sequential)
mc_search <- dplyr::bind_rows(mc_search)

t_mc_end <- Sys.time()
```

``` r

ok_mc <- mc_search[mc_search$status == "ok", ]

ggplot(ok_mc, aes(rep, cost_total / 1000, colour = storage_type)) +
  geom_point(size = 2) +
  facet_wrap(~ site, scales = "free_y") +
  scale_x_continuous(breaks = seq_len(n_mc)) +
  labs(title = sprintf(
         "Such-Monte-Carlo (x = 1, %d zufaellige Suchpfade je Standort und Typ)",
         n_mc),
       x = "Wiederholung (Seed)",
       y = "Kosten Optimum [Tsd. EUR]",
       colour = "Speichertyp") +
  theme_bw()

knitr::kable(
  ok_mc %>%
    dplyr::group_by(site, storage_type) %>%
    dplyr::summarise(
      flaeche_spanne_m2 = max(mulde_area) - min(mulde_area),
      tiefe_spanne_mm = max(mulde_height) - min(mulde_height),
      speicher_identisch = dplyr::n_distinct(storage_height) == 1,
      kosten_min = min(cost_total),
      kosten_max = max(cost_total),
      kosten_spanne_pct = round(100 * (max(cost_total) - min(cost_total)) /
                                  min(cost_total), 2),
      .groups = "drop"
    ),
  digits = 1,
  caption = paste("Streuung ueber die Suchpfade -- erwartet: identische",
                  "Speicherstufe, Flaechen-Spanne <= 2 x area_tol,",
                  "Kostenspanne wenige Prozent; die Tiefe streut wegen",
                  "der Flaechen-Kopplung weiter")
)
```

## Einordnung

- **Plausibilität:** Die Optima müssen auf oder knapp unter den
  günstigsten zulässigen Rasterzellen liegen (Warmstart-Tabelle der
  Monotonie-Analyse); die Kostenkurven müssen monoton fallen.
  `monotonicity_warning = TRUE` in einer Zeile hieße: Zähler *und*
  Volumen sind gemeinsam gestiegen — dann Branch prüfen.
- **Wien x = 0** ist der Stresstest: Im Raster war das Ziel mit
  minimalem Speicher teils unerreichbar — hier greift die
  Speicher-Eskalation automatisch; “infeasible_within_bounds” wäre ein
  reguläres Ergebnis, kein Fehler.
- **Bad Aussee x = 1** trägt den bekannten +1-Zählwobble am Rasterrand;
  der Rand-Guard in
  [`find_min_feasible()`](https://kwb-r.github.io/kwb.raindrop/reference/find_min_feasible.md)
  deckt ihn ab.
- **Nebenbedingung Tiefe:** `max_total_depth` (mm) begrenzt
  Muldentiefe + Filter + Speicher analytisch (DWA-A 138 / Überdeckung),
  ohne zusätzliche Läufe.
