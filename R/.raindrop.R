################################################################################
### 1) Datei öffnen (lesen & schreiben) + Überblick
################################################################################

path_list <- list(
  root_path = "C:/kwb/projects/raindrop/01102025_Raindrop_Daten",
  dir_base = "<root_path>/Optimierungsfall",
  dir_exe = "<root_path>/Berechnungskern",
  dir_input = "<root_path>/Optimierungsfall/models/input",
  dir_output = "<root_path>/Optimierungsfall/models/output", 
  dir_target_output = "<dir_output>/<dir_target>",
  file_base = "Optimierungsfall_kurz.h5",
  file_errors = "Fehlerprotokoll.h5",
  file_exe = "Regenwasserbewirtschaftung.exe",
  file_results_hdf5 = "Optimierung_MuldenRigole.h5",
  file_results_txt = "Optimierung_MuldenRigole_RAINDROP.txt", 
  file_results_txt_multilayer = "Optimierung_MuldenRigole_RAINDROP_multi_layer.txt", 
  file_target = "<dir_target>.h5",
  path_base = "<dir_base>/<file_base>",
  path_exe = "<dir_exe>/<file_exe>",
  path_results_hdf5 = "<dir_target_output>/<file_results_hdf5>",
  path_results_txt = "<dir_target_output>/<file_results_txt>", 
  path_results_txt_multilayer = "<dir_target_output>/<file_results_txt_multilayer>", 
  path_target_input = "<dir_input>/<file_target>",
  path_target_output = "<dir_output>/<file_target>"
  )

for(i in 1:100) {

paths <- kwb.utils::resolve(path_list, dir_target = sprintf("s%05d", i))

fs::dir_create(paths$dir_input)
fs::dir_create(paths$dir_output)
fs::dir_create(paths$dir_target_output)

fs::file_copy(path = paths$path_base, 
              new_path = paths$path_target_input, 
              overwrite = TRUE)

h5 <- hdf5r::H5File$new(paths$path_target_input, mode = "a")

h5[["/Berechnungsparameter/Ergebnispfad"]]$read()

new_path <- stringr::str_c(normalizePath(fs::path_abs(paths$dir_target_output)), 
                           "\\")

h5[["/Berechnungsparameter/Ergebnispfad"]][] <- new_path

h5[["/Berechnungsparameter/Ergebnispfad"]]$read()

# "a" = read/write (legt an, falls nicht da); alternativ "r+" = read/write, aber nicht neu anlegen

h5$flush()
## 2) Alle Handles schließen, sonst blockiert Windows das Umbenennen
gc()
h5$close_all()

run_model(path_exe = paths$path_exe, 
          path_input = paths$path_target_input)
}


#### Read results

### HDF-5

i <- 1 
paths <- kwb.utils::resolve(path_list, dir_target = sprintf("s%05d", i))


read_hdf5_timeseries <- function(ts_groupvariable) {

# 1) Nur Datasets (keine Subgruppen) auflisten
ds_ts_groupvariable <- ts_groupvariable$ls() %>%
  tibble::as_tibble() %>%
  dplyr::filter(obj_type == "H5I_DATASET") %>%
  dplyr::pull(name)

ds_ts_groupvariable


# als benannte Liste mit Matrizen (2 x N)
states_list <- setNames(lapply(ds_ts_groupvariable, function(nm) ts_groupvariable[[nm]]$read()), ds_ts_groupvariable)

# oder direkt als langes tibble (time/value angenommen: 1. Zeile = Zeit, 2. Zeile = Wert)
states_long <- purrr::map_dfr(ds_ts_groupvariable, function(nm) {
  m <- ts_groupvariable[[nm]]$read()
  tibble::tibble(
    variable = nm,
    time  = as.numeric(m[1, ]),
    value = as.numeric(m[2, ])
  )
})

states_long
}


hdf5_results <- list(
  rates = read_hdf5_timeseries(ts_groupvariable = res_hdf5[["Raten"]]),
  additional_evapotranspiration = read_hdf5_timeseries(ts_groupvariable = res_hdf5[["Zusaetzliche Variablen Evapotranspiration"]]),
  additional_infiltration = read_hdf5_timeseries(ts_groupvariable = res_hdf5[["Zusaetzliche Variablen Infiltration"]]),
  states = read_hdf5_timeseries(ts_groupvariable = res_hdf5[["Zustandsvariablen"]])
)


for(name in names(hdf5_results)) {

gg <- hdf5_results[[name]] %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, y = value)) + 
  ggplot2::facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  ggplot2::geom_line() +
  ggplot2::labs(title = name) +
  ggplot2::theme_bw() 

plot(gg)
}
