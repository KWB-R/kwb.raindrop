#' Capillary suction from hydraulic conductivity (Rawls fit)
#'
#' Suction head Psi_s in mm as a function of the saturated hydraulic
#' conductivity in mm/h, as used by the workflow vignettes.
#'
#' @param kf_mmh Saturated hydraulic conductivity in mm/h.
#' @return Suction head in mm.
#' @keywords internal
#' @noRd
psi_s_mm <- function(kf_mmh) {
  (3.237 * (kf_mmh / 25.4)^(-0.328)) * 25.4
}

#' Create a site-specific single-scenario runner for the optimiser
#'
#' Factors the `run_one()` function that was duplicated across the three
#' workflow vignettes (Eisenstadt 2005, Wien, Bad Aussee) into one
#' package-level closure factory. The returned function runs the RainDrop
#' engine for one parameter set and returns the thinned one-row
#' optimisation result (overflow events + water balance), augmented with
#' the input parameters and the overflow volume in m3.
#'
#' Site differences are covered by the arguments: Eisenstadt scales the
#' rain curve shipped in `base.h5` by `rain_factor` (leave
#' `timeseries_rain` = `NULL`), Wien and Bad Aussee replace the rain and
#' ET0 curves entirely (`timeseries_rain` / `timeseries_et`, values in
#' mm/h as written by the vignettes).
#'
#' On the first call the runner prepares a **site master file** once:
#' `base.h5` plus everything identical for every run (calculation
#' settings, ET/rain time series). Each run then copies the master and
#' writes only its ~15 small parameter datasets. Compared to the
#' previous full read/rewrite of *all* datasets per run this removes
#' the dominant per-run overhead of the optimisation searches
#' (hundreds of runs; for Wien / Bad Aussee it skips rewriting the
#' 15-year rain series on every single engine run).
#'
#' @param path_list Path definition list as used by the workflow vignettes
#'   (resolvable with `kwb.utils::resolve()`, must contain `path_base`,
#'   `path_exe`, `dir_input`, `dir_output`, `dir_target_output`,
#'   `path_target_input`, `path_results_hdf5_element`,
#'   `path_results_hdf5_flaeche`, `file_target`).
#' @param timestep_hours Engine time step in hours (default 0.1).
#' @param timeseries_rain Optional data.frame `time`/`value` (mm/h) written
#'   to `//Kurven/Regen` (the dataset must exist in `base.h5`); when
#'   given, the `//Kurven/Growth_1` and `//Kurven/Shading_1` end times
#'   are extended to the rain series end (skipped for templates without
#'   these curves) and `rain_factor` is ignored. Without
#'   `timeseries_rain`, a per-run `rain_factor != 1` requires
#'   `//Kurven/Regen` to exist as a time series in `base.h5` -- a clear
#'   error is thrown otherwise.
#' @param timeseries_et Optional data.frame `time`/`value` (mm/h) written
#'   to `//Kurven/ET0`.
#' @param storage_types Soil presets of the storage layer per storage type,
#'   see [default_storage_types()].
#' @param event_separation_hours Event separation for overflow counting
#'   (default 4, as in the vignettes and the monotonicity analysis).
#' @param scenario_prefix Prefix for generated scenario names (default
#'   `"o"` -> `o00001`, `o00002`, ... -- distinct from the grid runs
#'   `s00001` ...).
#' @param cleanup Delete each scenario's copied input file and output
#'   directory right after the thinned one-row result has been read
#'   (default `TRUE`). The optimisers only need that row; without the
#'   cleanup an optimisation run (hundreds of engine runs per task, each
#'   with its own copy of `base.h5` plus all output HDF5s) fills the
#'   temp drive and the engine aborts with HDF5 `errno = 28` ("No space
#'   left on device"). Set `FALSE` to keep all scenario files for
#'   debugging. Files of a *failed* run are always kept.
#' @param debug Passed on to the engine/reader helpers.
#'
#' @return `function(params)` where `params` is a named list (or one-row
#'   data.frame) with `mulde_area`, `mulde_height` (mm), `storage_type`,
#'   `storage_height` (mm), `connected_area` (m2), `filter_height` (mm),
#'   `filter_hydraulicconductivity` (mm/h), `bottom_hydraulicconductivity`
#'   (mm/h) and optionally `rain_factor` (default 1) and `lai`
#'   (default 3.9). It returns a one-row tibble with the parameters, the
#'   scenario name and the optimisation metrics (`n_overflows`,
#'   `sum_overflows` in mm, `overflow_volume_m3`, water-balance shares).
#'
#' @seealso [optimise_swale_design()], [find_min_feasible()]
#' @export
make_swale_runner <- function(path_list,
                              timestep_hours = 0.1,
                              timeseries_rain = NULL,
                              timeseries_et = NULL,
                              storage_types = default_storage_types(),
                              event_separation_hours = 4,
                              scenario_prefix = "o",
                              cleanup = TRUE,
                              debug = FALSE) {

  counter <- 0L
  master_path <- NULL
  template_rain <- NULL

  # One-time site master: base.h5 plus everything that is identical for
  # every run (calculation settings, ET/rain time series). Each run then
  # copies the master and writes only its ~15 small parameter datasets --
  # the full read/write cycle of all datasets (incl. multi-year curves)
  # per engine run dominated the runtime of the optimisation searches.
  prepare_master <- function(paths) {
    mp <- file.path(paths$dir_input,
                    sprintf("%s_master.h5", scenario_prefix))
    fs::dir_create(paths$dir_input, recurse = TRUE)
    fs::file_copy(path = paths$path_base, new_path = mp, overwrite = TRUE)

    h5m <- hdf5r::H5File$new(mp, mode = "a")
    on.exit(try(h5m$close_all(), silent = TRUE), add = TRUE)

    static_vals <- list(
      `//Berechnungsparameter/Zeitschritt_Infiltration` = timestep_hours,
      `//Berechnungsparameter/Zeitschritt_ET` = timestep_hours,
      `//Berechnungsparameter/Zeitschritt_Verschaltungen` = timestep_hours,
      `//Berechnungsparameter/R-Plots` = 0,
      `//Berechnungsparameter/Ausgabemodus` = "Optimierung",
      `//Berechnungsparameter/Evapotranspiration_aktiv` = 1,
      `//Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv` = 1,
      `//Massnahmenelemente/Mulde_Rigole/Berechnungsparameter/Evapotranspiration_aktiv` = 1,
      `//Massnahmenelemente/Mulde_Rigole/Allgemein/Regen-Skalierungsfaktor` = 1
    )
    if (!is.null(timeseries_et)) {
      static_vals$`//Kurven/ET0` <- timeseries_et
    }
    # base.h5 templates differ in which curves they ship -- only touch
    # datasets that actually exist (missing Growth/Shading just skips
    # the end-time fix; a missing rain curve only matters if a run
    # later asks for rain_factor != 1, which then errors clearly)
    existing <- list_h5_datasets(h5m)$path
    if (!is.null(timeseries_rain)) {
      if (!"//Kurven/Regen" %in% existing) {
        stop("make_swale_runner(): base.h5 has no dataset //Kurven/Regen ",
             "to replace with timeseries_rain: ", paths$path_base)
      }
      static_vals$`//Kurven/Regen` <- timeseries_rain
      for (curve in c("//Kurven/Growth_1", "//Kurven/Shading_1")) {
        if (!curve %in% existing) next
        cv <- h5_read_values(h5m, paths = curve)[[curve]]
        if (is.data.frame(cv) && length(cv$time) >= 2) {
          cv$time[2] <- max(timeseries_rain$time)
          static_vals[[curve]] <- cv
        }
      }
    } else if ("//Kurven/Regen" %in% existing) {
      # kept for per-run rain_factor scaling (Eisenstadt variant)
      template_rain <<- h5_read_values(
        h5m, paths = "//Kurven/Regen"
      )[["//Kurven/Regen"]]
    }

    h5_write_values(h5m, static_vals, resize = TRUE,
                    scalar_strategy = "error", verbose = FALSE)
    h5m$close_all()
    master_path <<- mp
  }

  function(params) {
    params <- as.list(params)
    required <- c("mulde_area", "mulde_height", "storage_type",
                  "storage_height", "connected_area", "filter_height",
                  "filter_hydraulicconductivity",
                  "bottom_hydraulicconductivity")
    missing <- setdiff(required, names(params))
    if (length(missing) > 0) {
      stop("make_swale_runner(): params is missing: ",
           paste(missing, collapse = ", "))
    }
    st <- storage_types[[params$storage_type]]
    if (is.null(st)) {
      stop("make_swale_runner(): unknown storage_type '",
           params$storage_type, "'")
    }
    rain_factor <- if (is.null(params$rain_factor)) 1 else params$rain_factor
    lai <- if (is.null(params$lai)) 3.9 else params$lai

    counter <<- counter + 1L
    s_name <- sprintf("%s%05d", scenario_prefix, counter)
    paths <- kwb.utils::resolve(path_list, dir_target = s_name)

    if (is.null(master_path)) prepare_master(paths)

    fs::dir_create(paths$dir_input, recurse = TRUE)
    fs::dir_create(paths$dir_output, recurse = TRUE)
    fs::dir_create(paths$dir_target_output, recurse = TRUE)

    fs::file_copy(path = master_path,
                  new_path = paths$path_target_input,
                  overwrite = TRUE)

    h5 <- hdf5r::H5File$new(paths$path_target_input, mode = "a")
    on.exit(try(h5$close_all(), silent = TRUE), add = TRUE)

    new_path <- stringr::str_c(
      normalizePath(fs::path_abs(paths$dir_target_output)), "\\"
    )

    vals <- list(
      `//Berechnungsparameter/Ergebnispfad` = new_path,
      `//Massnahmenelemente/Dach/Allgemein/Flaeche` = params$connected_area,
      `//Massnahmenelemente/Mulde_Rigole/Allgemein/Flaeche` = params$mulde_area,
      `//Massnahmenelemente/Mulde_Rigole/Eigenschaften_Oberflaeche/Ueberlaufhoehe` = params$mulde_height,
      `//Massnahmenelemente/Mulde_Rigole/Bodenschichtung/Startwerte_theta_ActualSoilMoisture` =
        c(0.3, st$Startwerte_theta_ActualSoilMoisture),
      `//Massnahmenelemente/Mulde_Rigole/Bodenschichtung/Schichtdicken` =
        c(params$filter_height, params$storage_height),
      `//Bodenarten/Speicher/thetaWP_MoistureAtWiltingPoint`  = st$thetaWP_MoistureAtWiltingPoint,
      `//Bodenarten/Speicher/thetaFC_MoistureAtFieldCapacity` = st$thetaFC_MoistureAtFieldCapacity,
      `//Bodenarten/Speicher/thetaS_MoistureAtSaturation`     = st$thetaS_MoistureAtSaturation,
      `//Massnahmenelemente/Mulde_Rigole/Allgemein/Endversickerungsrate` =
        params$bottom_hydraulicconductivity,
      `//Massnahmenelemente/Mulde_Rigole/Parameter_Evapotranspiration/LAI_LeafAreaIndex` = lai,
      `//Bodenarten/Bodenfilter/Ks_HydraulicConductivity` =
        params$filter_hydraulicconductivity,
      `//Bodenarten/Bodenfilter/Psi_Saugspannung_CapillarySuction` =
        psi_s_mm(params$filter_hydraulicconductivity)
    )
    # rain_factor is documented to be ignored when own rain series are
    # written (timeseries_rain); it scales the base.h5 curve otherwise
    if (is.null(timeseries_rain) && rain_factor != 1) {
      if (!is.data.frame(template_rain)) {
        stop("make_swale_runner(): rain_factor != 1 requires base.h5 to ",
             "contain //Kurven/Regen as a time series (time/value)")
      }
      scaled <- template_rain
      scaled$value <- scaled$value * rain_factor
      vals$`//Kurven/Regen` <- scaled
    }

    h5_write_values(h5, vals, resize = TRUE,
                    scalar_strategy = "error", verbose = FALSE)
    h5$close_all()

    run_model(path_exe = paths$path_exe,
              path_input = paths$path_target_input,
              debug = debug)

    # Thin immediately (lean read), exactly like the vignettes' run_one()
    sim_one <- get_simulation_results_optim(
      paths            = paths,
      path_list        = path_list,
      simulation_names = s_name,
      debug            = debug,
      lean             = TRUE
    )

    row <- add_overflow_events_and_waterbalance(
      simulation_results     = sim_one,
      event_separation_hours = event_separation_hours,
      canonical_variables    = default_canonical_wb_variables()
    )

    # the thinned row is all the optimiser needs -- drop the scenario's
    # input copy and output directory so long searches (hundreds of
    # engine runs) do not fill the temp drive. Reached only on success:
    # a failed run errors above and keeps its files for debugging.
    if (isTRUE(cleanup)) {
      try(fs::file_delete(paths$path_target_input), silent = TRUE)
      try(fs::dir_delete(paths$dir_target_output), silent = TRUE)
    }

    dplyr::bind_cols(
      tibble::as_tibble(params[required]),
      tibble::tibble(rain_factor = rain_factor, lai = lai),
      row
    ) %>%
      dplyr::mutate(
        # sum_overflows is mm water column over the swale area
        overflow_volume_m3 = .data$sum_overflows * .data$mulde_area / 1000
      )
  }
}
