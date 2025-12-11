# Workflow V2

### Define Paths and Scenarios

``` r
library(kwb.raindrop)

path_list <- list(
  root_path = "C:/kwb/projects/raindrop/2025-12-11_Raindrop_Daten",
  dir_base = "<root_path>/Optimierungsfall",
  dir_exe = "<root_path>/Berechnungskern",
  dir_input = "<root_path>/Optimierungsfall/models/input",
  dir_output = "<root_path>/Optimierungsfall/models/output", 
  dir_target_output = "<dir_output>/<dir_target>",
  file_base = "FlaecheUndMassnahmenelement.h5",
  file_errors_hdf5 = "Fehlerprotokoll.h5",
  file_exe = "Regenwasserbewirtschaftung.exe",
  file_results_hdf5_element = "Element_1.h5",
  file_results_hdf5_flaeche = "Flaeche_1.h5",
  file_results_hdf5_verschaltungen = "<dir_target>_Verschaltungen.h5",
  file_results_txt = "Element_1_RAINDROP.txt", 
  file_results_txt_multilayer = "Element_1_RAINDROP_multi_layer.txt", 
  file_target = "<dir_target>.h5",
  path_base = "<dir_base>/<file_base>",
  path_exe = "<dir_exe>/<file_exe>",
  path_errors_hdf5 = "<dir_target_output>/<file_errors_hdf5>",
  path_results_hdf5_element = "<dir_target_output>/<file_results_hdf5_element>",
  path_results_hdf5_flaeche = "<dir_target_output>/<file_results_hdf5_flaeche>",
  path_results_hdf5_verschaltungen = "<dir_target_output>/<file_results_hdf5_verschaltungen>",
  path_results_txt = "<dir_target_output>/<file_results_txt>", 
  path_results_txt_multilayer = "<dir_target_output>/<file_results_txt_multilayer>", 
  path_target_input = "<dir_input>/<file_target>"
)


parameters <- tibble::tibble(
  para_nama_short = c("connected_area", 
                    "mulde_area", 
                    "mulde_height",
                    "filter_hydraulicconductivity",
                    "filter_height",
                    "storage_height",
                    "bottom_hydraulicconductivity"
                    ),
  para_name_long = c(
                   "/Massnahmenelemente/Flaeche_1/Allgemein/Flaeche",
                   "/Massnahmenelemente/Element_1/Allgemein/Flaeche",
                   "/Massnahmenelemente/Element_1/Eigenschaften_Oberflaeche/Ueberlaufhoehe",
                   "Bodenarten/Bodenfilter/Ks_HydraulicConductivity",
                   "/Massnahmenelemente/Element_1/Bodenschichtung/Schichtdicken",
                   "/Massnahmenelemente/Element_1/Bodenschichtung/Schichtdicken",
                   "/Massnahmenelemente/Element_1/Allgemein/Endversickerungsrate"
                   ),
  index = c(1L,
            1L,
            1L,
            1L,
            1L,
            2L,
            1L)
)

DT::datatable(parameters)
```

``` r

connected_area <- c(1,10,50,100,500,1000)
mulde_area <- c(1,10,50,100,500,1000)
mulde_height <- 1:5 * 100
filter_hydraulicconductivity <- c(10,20,45,90,180,270,360)
filter_height <- c(200, 400, 600)
storage_height <- c(150, 300, 600, 900, 1200)
rain_factor <- c(1,2,3)
bottom_hydraulicconductivity <- c(1,5,10,20,45,90,180,270,360,1860,3600)


# Alle Kombinationen erzeugen
param_grid_all_combinations <- expand.grid(
  connected_area = connected_area,
  mulde_area = mulde_area,
  mulde_height = mulde_height,
  filter_hydraulicconductivity = filter_hydraulicconductivity,
  filter_height = filter_height,
  storage_height = storage_height,
  bottom_hydraulicconductivity = bottom_hydraulicconductivity,
  rain_factor = rain_factor
)

param_grid_all_combinations <- param_grid_all_combinations %>% 
  dplyr::bind_cols(tibble::tibble(scenario_name = sprintf("s%05d", 
                                                          seq_len(nrow(param_grid_all_combinations)))))

ref_scenario <- param_grid_all_combinations %>% 
       dplyr::filter(connected_area == 1,
                     mulde_area == 1, 
                     filter_hydraulicconductivity == 90, 
                     bottom_hydraulicconductivity == 20, 
                     mulde_height == 300,
                     filter_height == 200,
                     storage_height == 150,
                     rain_factor == 3) %>% 
       dplyr::pull(scenario_name)

stopifnot(length(ref_scenario)==1)

scenarios_with_single_parameter_variation <- kwb.raindrop::find_single_param_variations(
  data = param_grid_all_combinations,
  ref_scenario = ref_scenario
  ) %>% 
  dplyr::pull(scenario_name) %>% unique()
#> Rows with exactly one differing parameter: 38 of 623700
#> Single-parameter variations per parameter: connected_area=5, mulde_area=5, mulde_height=4, filter_hydraulicconductivity=6, filter_height=2, storage_height=4, bottom_hydraulicconductivity=10, rain_factor=2

param_grid <- param_grid_all_combinations  %>% 
  dplyr::filter(scenario_name %in% scenarios_with_single_parameter_variation)

DT::datatable(param_grid)
```

### Run Model

``` r
# Number of cores for parallel processing (or: automatic)
#future::plan(future::multisession, workers = parallel::detectCores() - 1)

lapply(seq_len(nrow(param_grid)), function(i) {
  
  param_grid_tmp <- param_grid[i, ]
  
  paths <- kwb.utils::resolve(path_list, dir_target = param_grid_tmp$scenario_name)
  
  fs::dir_create(paths$dir_input)
  fs::dir_create(paths$dir_output)
  fs::dir_create(paths$dir_target_output)
  
  fs::file_copy(path = paths$path_base, 
                new_path = paths$path_target_input, 
                overwrite = TRUE)
  
  # "a" = read/write (legt an, falls nicht da); alternativ "r+" = read/write, aber nicht neu anlegen
  h5 <- hdf5r::H5File$new(paths$path_target_input, mode = "a")
  

  new_path <- stringr::str_c(normalizePath(fs::path_abs(paths$dir_target_output)), 
                             "\\")

   # 2) Alle Werte lesen (als named list, Keys = absolute Pfade)
vals <- kwb.raindrop::h5_read_values(h5)

vals$`//Berechnungsparameter/Ergebnispfad` <- new_path
vals$`//Massnahmenelemente/Flaeche_1/Allgemein/Flaeche` <- param_grid_tmp$connected_area
vals$`//Massnahmenelemente/Element_1/Allgemein/Flaeche` <- param_grid_tmp$mulde_area
vals$`//Massnahmenelemente/Element_1/Eigenschaften_Oberflaeche/Ueberlaufhoehe` <-  param_grid_tmp$mulde_height
vals$`//Bodenarten/Bodenfilter/Ks_HydraulicConductivity` <- param_grid_tmp$filter_hydraulicconductivity
vals$`//Massnahmenelemente/Element_1/Bodenschichtung/Schichtdicken`[1] <- param_grid_tmp$filter_height 
vals$`//Massnahmenelemente/Element_1/Bodenschichtung/Schichtdicken`[2] <- param_grid_tmp$storage_height
vals$`//Massnahmenelemente/Element_1/Allgemein/Endversickerungsrate` <- param_grid_tmp$bottom_hydraulicconductivity

# Timeseries (2×N) als tibble?
if (is.data.frame(vals[["//Kurven/Regen"]])) {
  vals[["//Kurven/Regen"]]$value <- vals[["//Kurven/Regen"]]$value * param_grid_tmp$rain_factor
}

# 3) Schreiben mit safe-Fallback für echte SCALAR-Fälle
kwb.raindrop::h5_write_values(h5, vals, resize = TRUE, scalar_strategy = "error", verbose = FALSE)
h5$close_all()
  
  kwb.raindrop::run_model(path_exe = paths$path_exe,
                          path_input = paths$path_target_input)
})


### Read results for first run

paths <- kwb.utils::resolve(path_list, dir_target = sprintf("s%05d", i = 1))

simulation_names <- basename(fs::dir_ls(paths$dir_output))
simulation_names <- scenarios_with_single_parameter_variation

debug <- TRUE
errors_df <- lapply(simulation_names, function(s_name) {
  
  s_id <- s_name %>% stringr::str_remove("s") %>%  as.integer()
  paths <- kwb.utils::resolve(path_list, dir_target = s_name, i = s_id)
  
  if(fs::file_exists(paths$path_errors_hdf5)) {
    kwb.utils::catAndRun(messageText = sprintf("Reading error file '%s'",
                                               paths$path_errors_hdf5),
                         expr = {
    error_hdf <- hdf5r::H5File$new(paths$path_errors_hdf5, mode = "r")
    
    tibble::tibble(id = s_id, 
                   path = paths$path_errors_hdf5,
                   number_of_errors = error_hdf[["AnzahlFehler"]]$read()
    )
                         },
    dbg = debug)
  }
}) %>% 
  dplyr::bind_rows()
```

### Analyse Results

``` r
import_results_from_rds <- FALSE
debug <- TRUE
paths <- kwb.utils::resolve(path_list, dir_target = sprintf("s%05d", i = 1))

simulation_names <- basename(fs::dir_ls(paths$dir_output))
simulation_names <- scenarios_with_single_parameter_variation

simulation_results <- if(import_results_from_rds == FALSE) {
  stats::setNames(lapply(simulation_names, function(s_name) {


s_id <- s_name %>% stringr::str_remove("s") %>%  as.integer()

paths <- kwb.utils::resolve(path_list, dir_target = s_name, i = s_id)

if(all(file.exists(c(paths$path_results_hdf5_verschaltungen, 
                 paths$path_results_hdf5_element, 
                 paths$path_results_hdf5_flaeche)))) {

    kwb.utils::catAndRun(messageText = sprintf("Reading results file '%s'",
                                               paths$path_results_hdf5),
                         expr = {

# "a" = read/write (legt an, falls nicht da); alternativ "r+" = read/write, aber nicht neu anlegen
res_hdf5_element <- hdf5r::H5File$new(paths$path_results_hdf5_element, mode = "r")
res_hdf5_flaeche <- hdf5r::H5File$new(paths$path_results_hdf5_flaeche, mode = "r")
res_hdf5_verschaltungen <- hdf5r::H5File$new(paths$path_results_hdf5_verschaltungen, mode = "r")

hdf5_results <- list(
  element = list(
    meta = kwb.raindrop::read_hdf5_scalars(res_hdf5_element[["Metainfo"]], numeric_only = FALSE),
    rates = kwb.raindrop::read_hdf5_timeseries(res_hdf5_element[["Raten"]]),
    water_balance = kwb.raindrop::read_hdf5_scalars(res_hdf5_element[["Wasserbilanz"]]),
    additional_evapotranspiration = kwb.raindrop::read_hdf5_timeseries(res_hdf5_element[["Zusaetzliche Variablen Evapotranspiration"]]),
    additional_infiltration = kwb.raindrop::read_hdf5_timeseries(res_hdf5_element[["Zusaetzliche Variablen Infiltration"]]),
    states = kwb.raindrop::read_hdf5_timeseries(res_hdf5_element[["Zustandsvariablen"]])),
  connected_area = list(
    meta = kwb.raindrop::read_hdf5_scalars(res_hdf5_flaeche[["Metainfo"]], numeric_only = FALSE),
    rates = kwb.raindrop::read_hdf5_timeseries(res_hdf5_flaeche[["Raten"]]),
    water_balance = kwb.raindrop::read_hdf5_scalars(res_hdf5_flaeche[["Wasserbilanz"]]),
    additional_evapotranspiration = kwb.raindrop::read_hdf5_timeseries(res_hdf5_flaeche[["Zusaetzliche Variablen Evapotranspiration"]]),
    additional_infiltration = kwb.raindrop::read_hdf5_timeseries(res_hdf5_flaeche[["Zusaetzliche Variablen Infiltration"]]),
    states = kwb.raindrop::read_hdf5_timeseries(res_hdf5_flaeche[["Zustandsvariablen"]])),
  connections =  kwb.raindrop::read_hdf5_connections(res_hdf5_verschaltungen)
  )

hdf5_results
}, 
dbg = debug)}}), nm = simulation_names)
} else {
  readRDS(file = "../simulation_results.Rds")
}


pdff <- "simulation_results_per_scenario_v2.pdf"
kwb.utils::preparePdf(pdff)
lapply(scenarios_with_single_parameter_variation, function(s_name) {

selected_scenario <- param_grid %>% 
  dplyr::filter(scenario_name == s_name)

simulation_results[[s_name]]$element$states %>% 
  dplyr::bind_rows(simulation_results[[s_name]]$element$rates) %>% 
  dplyr::filter(variable %in% c("h_pond", 
                                "fPI_InfiltrationRate",
                                #"fPI_InfiltrationRate_deeperLayers", 
                                "f_Endversickerungsrate")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = time, y = value)) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~ variable, nrow = 2, ncol = 2, scales = "free_y") +
  ggplot2::labs(title = sprintf("Scenario ID: %s", 
                                s_name),
                subtitle = sprintf("connected area: %d m2, mulde: area %d m2, height: %d mm; filter: kf: %d mm/h, height: %d mm; storage_height: %d mm, bottom_kf: %.2f mm/h; rain_factor: %.2f", 
                                selected_scenario$connected_area,
                                selected_scenario$mulde_area,
                                selected_scenario$mulde_height, 
                                selected_scenario$filter_hydraulicconductivity, 
                                selected_scenario$filter_height, 
                                selected_scenario$storage_height, 
                                selected_scenario$bottom_hydraulicconductivity, 
                                selected_scenario$rain_factor)) +
  ggplot2::theme_bw()
})
kwb.utils::finishAndShowPdf(pdff)


simulation_results_h_pond_list <- stats::setNames(lapply(names(simulation_results), function(s_name) {
  
simulation_results[[s_name]]$element$states %>% 
  dplyr::filter(variable == "h_pond") %>% 
  dplyr::summarise(h_pond_max = max(value), 
                   h_pond_mean = mean(value)) 
}), names(simulation_results))


simulation_results_h_pond <- simulation_results_h_pond_list %>% 
  dplyr::bind_rows(, .id = "scenario_name") %>% 
  dplyr::left_join(param_grid,
                   by = "scenario_name")
  
### Plot results

pdff <- "simulation_results_h_pond_max_v2.pdf"
kwb.utils::preparePdf(pdff)
kwb.raindrop::plot_hpond_vs_ref(data = simulation_results_h_pond,
                                response = "h_pond_max",
                                ref_scenario = ref_scenario,
                                diff = "abs")
kwb.utils::finishAndShowPdf(pdff)

pdff <- "simulation_results_h_pond_mean_v2.pdf"
kwb.utils::preparePdf(pdff)
kwb.raindrop::plot_hpond_vs_ref(data = simulation_results_h_pond,
                                response = "h_pond_mean",
                                ref_scenario = ref_scenario,
                                diff = "abs")
kwb.utils::finishAndShowPdf(pdff)
```
