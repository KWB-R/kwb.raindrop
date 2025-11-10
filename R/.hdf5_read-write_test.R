library(hdf5r)
library(tibble)


path_list <- list(
  root_path = "C:/kwb/projects/raindrop/01102025_Raindrop_Daten",
  dir_base = "<root_path>/Optimierungsfall",
  dir_exe = "<root_path>/Berechnungskern",
  dir_input = "<root_path>/Optimierungsfall/models/input",
  dir_output = "<root_path>/Optimierungsfall/models/output", 
  dir_target_output = "<dir_output>/<dir_target>",
  file_base = "Optimierungsfall_kurz.h5",
  file_errors_hdf5 = "Fehlerprotokoll.h5",
  file_exe = "Regenwasserbewirtschaftung.exe",
  file_results_hdf5 = "Optimierung_MuldenRigole.h5",
  file_results_txt = "Optimierung_MuldenRigole_RAINDROP.txt", 
  file_results_txt_multilayer = "Optimierung_MuldenRigole_RAINDROP_multi_layer.txt", 
  file_target = "<dir_target>.h5",
  path_base = "<dir_base>/<file_base>",
  path_exe = "<dir_exe>/<file_exe>",
  path_errors_hdf5 = "<dir_target_output>/<file_errors_hdf5>",
  path_results_hdf5 = "<dir_target_output>/<file_results_hdf5>",
  path_results_txt = "<dir_target_output>/<file_results_txt>", 
  path_results_txt_multilayer = "<dir_target_output>/<file_results_txt_multilayer>", 
  path_target_input = "<dir_input>/<file_target>",
  path_target_output = "<dir_output>/<file_target>"
)

i <- 1
  
paths <- kwb.utils::resolve(path_list, dir_target = sprintf("s%05d", i))
  
fs::dir_create(paths$dir_input)
fs::dir_create(paths$dir_output)
fs::dir_create(paths$dir_target_output)
  
fs::file_copy(path = paths$path_base, 
              new_path = paths$path_target_input, 
              overwrite = TRUE)
  

# Datei mit Schreibrecht öffnen
h5 <- H5File$new(paths$path_target_input, mode = "r+")

h5[["/Berechnungsparameter/Ergebnispfad"]]$read()

new_path <- stringr::str_c(normalizePath(fs::path_abs(paths$dir_target_output)), 
                           "\\")

h5[["/Berechnungsparameter/Ergebnispfad"]][] <- new_path

h5[["/Berechnungsparameter/Ergebnispfad"]]$read()

h5$flush()

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
#h5_validate_write(h5, vals) %>% View()

# -> zeigt je Pfad: erkannte Dims (mit ls()-Fallback), Länge/Spalten, Entscheidung (SCALAR/1D/ND/TS)

# 3) Schreiben mit safe-Fallback für echte SCALAR-Fälle
h5_write_values(h5, vals, resize = TRUE, scalar_strategy = "error", verbose = TRUE)


h5$close_all()


kwb.raindrop::run_model(path_exe = paths$path_exe,
                        path_input = paths$path_target_input)
