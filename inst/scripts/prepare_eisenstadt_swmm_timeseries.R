#' Prepare Eisenstadt rainfall and ET0 time series for SWMM import
#'
#' Unlike Wien / Bad Aussee (which ship station CSVs from GeoSphere
#' Austria), the Eisenstadt case study currently exists only as a Tandler
#' engine HDF5 template (`inst/extdata/models/eisenstadt-2005/base.h5`).
#' This script extracts the rain and ET0 *curves* directly from that HDF5
#' (or from any other engine `.h5`, e.g. a real Eisenstadt input file from
#' the project server) and writes two SWMM-5-compatible external
#' time-series files:
#'
#'   * `eisenstadt_rain.dat` — rainfall as mm per (native) interval,
#'      suitable for a SWMM `[RAINGAGES]` entry of type VOLUME.
#'   * `eisenstadt_et0.dat`  — daily reference evapotranspiration (ET0),
#'      mm/day, suitable for the SWMM `[EVAPORATION]` block via
#'      `TIMESERIES <name>`.
#'
#' ## UNITS — read this before trusting the output
#'
#' The Tandler engine stores **both** curves as *rates* on an
#' **hour-based** time axis (`/Kurven/Regen`, `/Kurven/ET0`; the axis runs
#' 0 .. 8759.833 h = one year). The values are therefore in **mm/h**, not
#' mm per interval and not mm/day:
#'
#'   * Rain: only the mm/h reading yields a physically sane annual depth
#'     (sum(rate) * (1/6 h) ~= 657 mm/a for the bundled template; reading
#'     the same numbers as "mm per 10-min interval" would imply ~3943 mm/a,
#'     which is impossible for eastern Austria).
#'   * The Wien / Bad Aussee vignettes confirm this: they explicitly
#'     convert rain from "mm per interval" to mm/h (`value / period`)
#'     before writing `/Kurven/Regen`.
#'
#' This exporter converts **out** of the engine's mm/h convention into the
#' units SWMM expects:
#'
#'   * Rain : SWMM VOLUME wants depth per interval  -> value[mm/h] * period[h]
#'   * ET0  : SWMM EVAPORATION wants mm/day          -> integral of mm/h over each day
#'
#' Keep this asymmetry in mind when comparing against the engine results:
#' if an engine ET0 curve was filled with **daily** ET0 numbers (mm/d)
#' without first dividing by 24, the engine — which reads the curve as
#' mm/h — would over-estimate ET0 by a factor of 24.
#'
#' Usage (R):
#' ```
#' source(system.file("scripts/prepare_eisenstadt_swmm_timeseries.R",
#'                    package = "kwb.raindrop"))
#' # bundled template:
#' export_eisenstadt_swmm_timeseries(out_dir = "C:/swmm/eisenstadt")
#' # a real server input file:
#' export_eisenstadt_swmm_timeseries(
#'   path_h5 = "P:/.../Eisenstadt_input.h5",
#'   out_dir = "C:/swmm/eisenstadt",
#'   start   = as.POSIXct("2005-01-01", tz = "UTC"))
#' ```
#'
#' Source of the bundled template: `inst/extdata/models/eisenstadt-2005/base.h5`
#' (Tandler "Regenwasserbewirtschaftung" engine). NOTE: in that template the
#' ET0 curve is a constant placeholder (0.2 mm/h = 4.8 mm/d), *not* a measured
#' series — see the generated file header.

export_eisenstadt_swmm_timeseries <- function(
    path_h5    = system.file("extdata/models/eisenstadt-2005/base.h5",
                             package = "kwb.raindrop"),
    out_dir    = tempdir(),
    start      = as.POSIXct("2005-01-01 00:00", tz = "UTC"),
    site_label = "Eisenstadt 2005",
    rain_curve = "Kurven/Regen",
    et0_curve  = "Kurven/ET0"
) {
  stopifnot(nzchar(path_h5), file.exists(path_h5))
  if (!requireNamespace("hdf5r", quietly = TRUE)) {
    stop("export_eisenstadt_swmm_timeseries() needs the 'hdf5r' package.")
  }
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

  # ---- read the two engine curves ----------------------------------------
  h5 <- hdf5r::H5File$new(path_h5, mode = "r")
  on.exit(h5$close_all(), add = TRUE)
  rain_raw <- h5[[rain_curve]]$read()
  et0_raw  <- h5[[et0_curve]]$read()

  # Engine curves are 2-column (time[h], value[mm/h]) but may come back in
  # either orientation; pick the layout whose 'time' candidate is sorted and
  # starts near 0.
  as_time_value <- function(m) {
    m <- as.matrix(m)
    cand <- if (ncol(m) == 2L && nrow(m) != 2L) {
      list(time = m[, 1], value = m[, 2])
    } else if (nrow(m) == 2L && ncol(m) != 2L) {
      list(time = m[1, ], value = m[2, ])
    } else {
      # square / ambiguous: try both, keep the sorted-from-~0 orientation
      byrow <- list(time = m[1, ], value = m[2, ])
      bycol <- list(time = m[, 1], value = m[, 2])
      ok <- function(x) !is.unsorted(x$time) && min(x$time) <= 1e-6
      if (ok(bycol)) bycol else if (ok(byrow)) byrow else bycol
    }
    list(time = as.numeric(cand$time), value = as.numeric(cand$value))
  }

  rain <- as_time_value(rain_raw)
  et0  <- as_time_value(et0_raw)
  if (length(rain$time) < 2L) stop("rain curve '", rain_curve, "' has < 2 points")

  # ---- Rainfall: mm/h -> mm per interval (SWMM VOLUME) --------------------
  period_h <- c(diff(rain$time), mean(diff(rain$time)))  # hours per sample
  depth_mm <- rain$value * period_h                      # mm/h * h = mm
  rain_dt  <- start + as.difftime(rain$time, units = "hours")
  annual   <- sum(depth_mm) / ((max(rain$time) - min(rain$time)) / 24 / 365)

  rain_lines <- c(
    "; SWMM 5 external rainfall time series",
    sprintf("; Site:    %s", site_label),
    sprintf("; Source:  %s  ->  /%s", path_h5, rain_curve),
    "; Engine curve stores RATES in mm/h on an hour-based time axis.",
    "; Exported as VOLUME = depth per interval (mm) = rate[mm/h] * period[h].",
    sprintf("; Annual depth: %.1f mm/a   (peak %.2f mm/h)", annual, max(rain$value)),
    sprintf("; Period:  %s - %s",
            format(min(rain_dt), "%Y-%m-%d %H:%M"),
            format(max(rain_dt), "%Y-%m-%d %H:%M")),
    "; Format:  MM/DD/YYYY  HH:MM  value   (SWMM [RAINGAGES] VOLUME)",
    "; Generated by inst/scripts/prepare_eisenstadt_swmm_timeseries.R",
    sprintf("%s  %.3f", format(rain_dt, "%m/%d/%Y  %H:%M"), depth_mm)
  )
  rain_out <- file.path(out_dir, "eisenstadt_rain.dat")
  writeLines(rain_lines, rain_out)

  # ---- ET0: mm/h -> mm/day (SWMM EVAPORATION) -----------------------------
  # Integrate the (possibly constant or sparse) mm/h curve onto an hourly
  # grid, then sum per calendar day to get mm/day. For the bundled template
  # the curve is constant 0.2 mm/h -> 4.8 mm/day every day.
  span_h    <- max(rain$time)
  hours     <- seq(0, span_h, by = 1)        # 0..8759 h -> 365 full days in 2005
  rate_mmh  <- stats::approx(et0$time, et0$value, xout = hours,
                             method = "linear", rule = 2)$y
  hour_dt   <- start + as.difftime(hours, units = "hours")
  day       <- as.Date(hour_dt, tz = "UTC")
  et0_daily <- tapply(rate_mmh, day, sum)        # sum(mm/h * 1 h) = mm/day
  et0_dates <- as.Date(names(et0_daily))
  is_const  <- length(unique(round(et0$value, 6))) == 1L

  et0_lines <- c(
    "; SWMM 5 external ET0 time series",
    sprintf("; Site:    %s", site_label),
    sprintf("; Source:  %s  ->  /%s", path_h5, et0_curve),
    "; Engine curve stores RATES in mm/h. Exported as mm/day (= integral over each day).",
    if (is_const) sprintf(
      ";          NOTE: source curve is CONSTANT %.3f mm/h (= %.3f mm/d) -- a PLACEHOLDER, not a measured series.",
      et0$value[1], et0$value[1] * 24) else
      sprintf("; ET0 range: %.2f - %.2f mm/day", min(et0_daily), max(et0_daily)),
    sprintf("; Annual ET0: %.0f mm/a", sum(et0_daily)),
    sprintf("; Period:  %s - %s",
            format(min(et0_dates), "%Y-%m-%d"),
            format(max(et0_dates), "%Y-%m-%d")),
    "; Format:  MM/DD/YYYY  HH:MM  value   (daily, HH:MM fixed at 00:00)",
    "; Generated by inst/scripts/prepare_eisenstadt_swmm_timeseries.R",
    sprintf("%s  00:00  %.3f", format(et0_dates, "%m/%d/%Y"), as.numeric(et0_daily))
  )
  et0_out <- file.path(out_dir, "eisenstadt_et0.dat")
  writeLines(et0_lines, et0_out)

  message("SWMM time-series files written:\n",
          "  rainfall (", length(depth_mm), " rows, ",
          sprintf("%.0f mm/a", annual), "): ", rain_out, "\n",
          "  ET0      (", length(et0_daily), " rows, ",
          sprintf("%.0f mm/a", sum(et0_daily)), "): ", et0_out)
  invisible(list(rain = rain_out, et = et0_out))
}

# Rscript prepare_eisenstadt_swmm_timeseries.R [out_dir] [path_h5]
if (sys.nframe() == 0L) {
  args    <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[[1L]] else tempdir()
  if (length(args) >= 2L) {
    export_eisenstadt_swmm_timeseries(out_dir = out_dir, path_h5 = args[[2L]])
  } else {
    export_eisenstadt_swmm_timeseries(out_dir = out_dir)
  }
}
