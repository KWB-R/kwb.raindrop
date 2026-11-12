#' Monthly growth factors for green-roof vegetation (SWMM-UrbanEVA)
#'
#' Seasonal modulation factors for the vegetation growth curve
#' (`gfdoy`), one value per calendar month, as derived by Daniel Wicke
#' (KWB) from SWMM-UrbanEVA (Hoernschemeyer et al.) for the RainDrop
#' green-roof scenarios. The factors average exactly 1.00 over the year,
#' i.e. they redistribute the evapotranspiration potential seasonally
#' without changing its annual total (before water limitation).
#'
#' The engine replaces the static `gfdoy_GrowthFactor` completely with
#' the referenced growth curve (it does not multiply the two), so these
#' factors are the effective seasonal modulation
#' (Tandler, 2026-10-08, answer 5).
#'
#' @return Named numeric vector of length 12 (`Jan` ... `Dez`).
#'
#' @examples
#' mean(default_growth_factors())  # exactly 1
#'
#' @export
default_growth_factors <- function() {
  c(
    Jan = 0.68, Feb = 0.68, Mrz = 0.68,
    Apr = 1.01, Mai = 1.18, Jun = 1.35,
    Jul = 1.35, Aug = 1.35, Sep = 1.18,
    Okt = 1.01, Nov = 0.85, Dez = 0.68
  )
}

#' Build a monthly step growth curve on the engine's time axis
#'
#' Expands 12 monthly factors into the `data.frame(time, value)` layout
#' the engine expects for `//Kurven/*` datasets: `time` in hours since
#' simulation start (the first timestamp of the rain series), one
#' support point per month boundary inside the simulated period, plus a
#' terminal point at `end_time_hours`. The engine holds each value from
#' its support point until the next one (step interpolation), so month
#' boundaries are the correct support-point locations.
#'
#' @param origin `POSIXct`/`Date` (or anything `lubridate::as_datetime()`
#'   accepts): the calendar timestamp of simulation hour 0, i.e. the
#'   first timestamp of the rain series fed to `//Kurven/Regen`.
#' @param end_time_hours Numeric. End of the curve in hours since
#'   `origin`; pin this to `max(timeseries_rain$time)` exactly as the
#'   workflows already do for `Growth_1` / `Shading_1`.
#' @param factors Named numeric vector of length 12, January first
#'   (default [default_growth_factors()]).
#'
#' @return `data.frame` with columns `time` (hours since `origin`) and
#'   `value`, ready to be assigned to a `//Kurven/*` entry of an
#'   [h5_read_values()] list.
#'
#' @examples
#' curve <- make_growth_curve("2011-01-01", end_time_hours = 2 * 8760)
#' head(curve)
#'
#' @export
make_growth_curve <- function(origin,
                              end_time_hours,
                              factors = default_growth_factors()) {

  stopifnot(length(factors) == 12L, is.numeric(factors), !anyNA(factors),
            is.numeric(end_time_hours), length(end_time_hours) == 1L,
            end_time_hours > 0)
  if (!is.null(names(factors)) &&
      !identical(names(factors), names(default_growth_factors()))) {
    stop("factors must be ordered January-first (names ",
         paste(names(default_growth_factors()), collapse = ", "), ")")
  }

  origin <- lubridate::as_datetime(origin)
  end <- origin + lubridate::dhours(end_time_hours)

  month_starts <- seq(
    lubridate::floor_date(origin, unit = "month"),
    lubridate::floor_date(end, unit = "month"),
    by = "month"
  )

  times <- as.double(difftime(month_starts, origin, units = "hours"))
  values <- as.double(factors[lubridate::month(month_starts)])

  # The origin may fall inside a month: the first support point then lies
  # before hour 0. Clamp it to 0 so the curve starts at simulation start
  # with that month's factor.
  times[1L] <- 0

  # Terminal point at the end of the simulated period (same convention as
  # the Growth_1 / Shading_1 end-time pinning in the workflows); drop any
  # month boundary at or beyond it first so times stay strictly increasing.
  keep <- times < end_time_hours
  times <- times[keep]
  values <- values[keep]

  data.frame(
    time = c(times, end_time_hours),
    value = c(values, values[length(values)])
  )
}

#' H5 overrides that turn the base.h5 roof into a roof-type scenario
#'
#' Returns a named list of `base.h5` parameter overrides (same layout as
#' [h5_read_values()] output, merge via `utils::modifyList()`) that
#' parametrises the `Dach` element as one of three roof variants:
#'
#' * `"conventional"` -- the `base.h5` defaults, untouched. The sealed
#'   roof of the published status-quo runs, whose ET is exactly 0 even
#'   when the workflows switch `Evapotranspiration_aktiv` on, because
#'   (a) `Infiltration_aktiv = 0` keeps water out of the substrate while
#'   the Verschaltung empties the pond before pond evaporation acts, and
#'   (b) `SL = 0` plus a start moisture below the wilting point disable
#'   interception and transpiration (Tandler, 2026-10-08, answers 1-2).
#' * `"extensive"` -- green roof, 80 mm substrate, `KC = 0.94`,
#'   `LAI = 3.9`.
#' * `"intensive"` -- green roof, 200 mm substrate, `KC = 1.05`,
#'   `LAI = 5.0`.
#'
#' Green-roof values were derived by Daniel Wicke (KWB) from
#' SWMM-UrbanEVA (Hoernschemeyer et al.), v2 of 2026-10-13: the LAI
#' values of the two variants were swapped relative to v1 (they were
#' inconsistent with KC; Tandler review, 2026-10-09). The substrate soil
#' ("Bodenart Dach") is deliberately identical for both green roofs --
#' consistent with the KWB KEYS parameterisation (`KWB-R/keys.lid`,
#' `swmm_lid-parameterisation.xlsx`, sheet `green_roof`), which also
#' differentiates extensive/intensive only by thickness. Differentiation
#' happens via substrate thickness and vegetation (KC/LAI).
#'
#' Both green roofs set `Evapotranspiration_aktiv = 1` and
#' `Infiltration_aktiv = 1` for the roof (base.h5 ships both per-element
#' switches off; rain must enter the substrate to be stored and
#' evaporated) and `Endversickerungsrate = 0`
#' (a roof is sealed underneath; the base.h5 value of 36 mm/h would
#' silently drain the substrate out of the balance once infiltration is
#' active) -- Tandler, 2026-10-08, answers 1-3, and 2026-10-09, point 3.
#'
#' The global `Hoernschemeyer_aktiv` switch is part of the scenario
#' frame: it exists only at `//Berechnungsparameter/` (no per-element
#' variant; Tandler, 2026-10-08, answer 4) and gates the seasonal
#' Growth/Shading modulation, so it defaults to `1` here for every roof
#' type -- including `"conventional"`, which keeps the swale comparable
#' across all three variants. Pass `hoernschemeyer_aktiv = NULL` to
#' leave the base.h5 value (0, legacy ET path) untouched, e.g. to
#' replicate the published status-quo runs bit for bit.
#'
#' The growth curve itself is not part of these overrides: write the
#' monthly factors with [make_growth_curve()] into `//Kurven/Growth_1`
#' (shared by Dach and Mulde_Rigole) or into an additional named curve
#' (e.g. `//Kurven/Growth_Dach`, created via
#' [h5_ensure_datasets_from_values()]) referenced by
#' `//Massnahmenelemente/Dach/Parameter_Evapotranspiration/Kurve_gfdoy`.
#'
#' @param roof_type One of `"conventional"`, `"extensive"`,
#'   `"intensive"`.
#' @param hoernschemeyer_aktiv Integer `0`/`1` (default `1`), or `NULL`
#'   to omit the global switch from the overrides.
#'
#' @return Named list of `base.h5` overrides; empty except for the
#'   global switch when `roof_type = "conventional"`.
#'
#' @examples
#' str(roof_type_values("extensive"))
#' roof_type_values("conventional", hoernschemeyer_aktiv = NULL)
#'
#' @export
roof_type_values <- function(roof_type = c("conventional", "extensive",
                                           "intensive"),
                             hoernschemeyer_aktiv = 1L) {

  roof_type <- match.arg(roof_type)

  if (!is.null(hoernschemeyer_aktiv)) {
    if (length(hoernschemeyer_aktiv) != 1L ||
        !hoernschemeyer_aktiv %in% c(0, 1)) {
      stop("hoernschemeyer_aktiv must be a single 0 or 1, or NULL ",
           "to leave the base.h5 value untouched")
    }
  }

  vals <- if (is.null(hoernschemeyer_aktiv)) {
    list()
  } else {
    list(
      `//Berechnungsparameter/Hoernschemeyer_aktiv` =
        as.integer(hoernschemeyer_aktiv)
    )
  }

  if (roof_type == "conventional") {
    return(vals)
  }

  veg <- switch(
    roof_type,
    extensive = list(thickness_mm = 80, kc = 0.94, lai = 3.9),
    intensive = list(thickness_mm = 200, kc = 1.05, lai = 5.0)
  )

  utils::modifyList(vals, list(
    # Substrate ("Bodenart Dach"): identical for both green roofs, per the
    # KWB KEYS parameterisation (keys.lid).
    `//Bodenarten/Dach/Ks_HydraulicConductivity` = 90,
    `//Bodenarten/Dach/Psi_Saugspannung_CapillarySuction` = 10,
    `//Bodenarten/Dach/thetaFC_MoistureAtFieldCapacity` = 0.35,
    `//Bodenarten/Dach/thetaS_MoistureAtSaturation` = 0.6,
    `//Bodenarten/Dach/thetaWP_MoistureAtWiltingPoint` = 0.05,
    # Water path: ET on (base.h5 ships the per-element switch OFF -- the
    # workflows have been turning it on frame-side, but a green roof must
    # not depend on that), infiltration into the substrate on, exfiltration
    # to the ground off (a roof is sealed underneath).
    `//Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv` = 1L,
    `//Massnahmenelemente/Dach/Berechnungsparameter/Infiltration_aktiv` = 1L,
    `//Massnahmenelemente/Dach/Allgemein/Endversickerungsrate` = 0,
    `//Massnahmenelemente/Dach/Bodenschichtung/Schichtdicken` =
      veg$thickness_mm,
    # Start moisture must sit above the wilting point (0.05), otherwise
    # transpiration never starts (one of the two reasons the status-quo
    # roof evaporates exactly nothing).
    `//Massnahmenelemente/Dach/Bodenschichtung/Startwerte_theta_ActualSoilMoisture` = 0.1,
    `//Massnahmenelemente/Dach/Parameter_Evapotranspiration/KC_CropFactor` =
      veg$kc,
    `//Massnahmenelemente/Dach/Parameter_Evapotranspiration/LAI_LeafAreaIndex` =
      veg$lai,
    # Leaf storage: enables interception (SL * LAI); 0 in base.h5.
    `//Massnahmenelemente/Dach/Parameter_Evapotranspiration/SL_LeafStorageCoefficient` = 0.29
  ))
}
