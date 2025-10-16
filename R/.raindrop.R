################################################################################
### 1) Datei öffnen (lesen & schreiben) + Überblick
################################################################################

library(hdf5r)

dir_exe <- "01102025_Raindrop_Daten/Berechnungskern"
dir_base <- "01102025_Raindrop_Daten/Optimierungsfall"
dir_input <- "01102025_Raindrop_Daten/Optimierungsfall/models/input"
dir_output <- "01102025_Raindrop_Daten/Optimierungsfall/models/output"

file_base <- "Optimierungsfall_kurz.h5"
file_exe <- "Regenwasserbewirtschaftung.exe"


for(i in 1:100) {

dir_target <- sprintf("s%05d", i) 
dir_target_output <- file.path(dir_output, dir_target)
file_target <- sprintf("%s.h5", dir_target)

path_base <- file.path(dir_base, file_base)
path_exe <- file.path(dir_exe, file_exe)
path_target_input <- file.path(dir_input, file_target)
path_target_output <- file.path(dir_output, file_target)

fs::dir_create(dir_input)
fs::dir_create(dir_output)
fs::dir_create(dir_target_output)

fs::file_copy(path = path_base, new_path = path_target_input, overwrite = TRUE)

h5 <- hdf5r::H5File$new(path_target_input, mode = "a")

h5[["/Berechnungsparameter/Ergebnispfad"]]$read()

new_path <- stringr::str_c(normalizePath(fs::path_abs(dir_target_output)), 
                           "\\")

h5[["/Berechnungsparameter/Ergebnispfad"]][] <- new_path

h5[["/Berechnungsparameter/Ergebnispfad"]]$read()

# "a" = read/write (legt an, falls nicht da); alternativ "r+" = read/write, aber nicht neu anlegen

h5$flush()
## 2) Alle Handles schließen, sonst blockiert Windows das Umbenennen
gc()
h5$close_all()

run_model(path_exe = path_exe, 
          path_input = path_target_input)
}