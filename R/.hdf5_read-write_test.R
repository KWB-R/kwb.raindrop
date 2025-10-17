library(hdf5r)
library(tibble)

# Datei mit Schreibrecht öffnen
h5 <- H5File$new("C:/kwb/projects/raindrop/01102025_Raindrop_Daten/Optimierungsfall/models/input/s00001.h5", mode = "r+")

# 1) Alle Datasets listen
list_h5_datasets(h5)

# 2) Alle Werte lesen (als named list, Keys = absolute Pfade)
vals <- h5_read_values(h5)

# 1) Pfade in vals sicher normalisieren
names(vals) <- sub("^//+", "/", names(vals))

# Beispiel: Pfad ändern (Scalar STRING)
vals$`/Massnahmenelemente/Optimierung_MuldenRigole/Allgemein/Flaeche` <- 100000


# Pfad-Scalar (STRING)
#vals["/Berechnungsparameter/Ergebnispfad"] <- "C:/temp/out.h5"

# Timeseries (2×N) als tibble?
if (is.data.frame(vals[["/Regen/Regenganglinie"]])) {
  vals[["/Regen/Regenganglinie"]]$value <- vals[["/Regen/Regenganglinie"]]$value * 2
}

# 2) Vorab-Validierung ansehen
h5_validate_write(h5, vals) %>% View()

# -> zeigt je Pfad: erkannte Dims (mit ls()-Fallback), Länge/Spalten, Entscheidung (SCALAR/1D/ND/TS)

# 3) Schreiben mit safe-Fallback für echte SCALAR-Fälle
h5_write_values(h5, vals, resize = TRUE, scalar_strategy = "error", verbose = TRUE)


h5$close_all()
