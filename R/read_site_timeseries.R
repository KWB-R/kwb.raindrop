#' Read and prepare site rain/ET0 time series for the engine (mm/h)
#'
#' Factors the time-series preparation duplicated in the Wien and Bad
#' Aussee workflow vignettes into one helper: reads the shipped GeoSphere
#' rain series (`rain.csv.gz`: columns `time` (datetime), `rr` (mm per
#' interval), `station`, further columns tolerated) and reference ET0
#' series (`et.csv`:
#' `date;value` with `dd.mm.yyyy`, mm per day), converts both to hours
#' since series start, aligns the series ends (the shorter series is
#' extended to the longer one's end, repeating its last value) and
#' converts the values to the engine's **mm/h** rate convention (rain:
#' mm per interval / interval hours; ET0: mm per day / 24).
#'
#' @param path_rain Path to the rain CSV (may be gzipped).
#' @param path_et Path to the ET0 CSV (semicolon separated).
#' @param verbose Print alignment messages (default TRUE).
#'
#' @return List with data.frames `rain` and `et` (columns `time` = hours
#'   since start, `value` = mm/h) ready for
#'   `make_swale_runner(timeseries_rain = , timeseries_et = )`.
#'
#' @seealso [make_swale_runner()]
#' @export
read_site_timeseries <- function(path_rain, path_et, verbose = TRUE) {

  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("read_site_timeseries() requires the 'readr' package")
  }

  timeseries_et <- readr::read_delim(path_et, delim = ";",
                                     col_types = "cd") %>%
    dplyr::mutate(
      date = lubridate::dmy(.data$date),
      time = as.integer(difftime(.data$date, min(.data$date),
                                 units = "hours"))
    ) %>%
    dplyr::select(-"date") %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::relocate("time", .before = "value")
  timeseries_et$time[nrow(timeseries_et)] <-
    ceiling(timeseries_et$time[nrow(timeseries_et)])

  timeseries_rain <- readr::read_csv(path_rain, show_col_types = FALSE) %>%
    dplyr::rename(datetime = "time", value = "rr") %>%
    dplyr::mutate(
      time = as.double(difftime(.data$datetime, min(.data$datetime),
                                units = "secs")) / 3600
    ) %>%
    dplyr::filter(!is.na(.data$value)) %>%
    # robust gegen zusaetzliche Spalten (Bad Aussee hat z.B. "substation"):
    # die Engine erwartet exakt time + value
    dplyr::select("time", "value")
  timeseries_rain$time[nrow(timeseries_rain)] <-
    ceiling(timeseries_rain$time[nrow(timeseries_rain)])

  # Serien-Enden angleichen: die kuerzere Serie wird mit ihrem letzten
  # Wert bis zum Ende der laengeren verlaengert (wie in den Vignetten).
  extend_to <- function(df, t_end, label) {
    if (t_end <= max(df$time)) return(df)
    if (isTRUE(verbose)) {
      message(sprintf(
        "%s series extended by %.1f hours to %.0f h (last value %.4f)",
        label, t_end - max(df$time), t_end, df$value[nrow(df)]
      ))
    }
    dplyr::bind_rows(df, tibble::tibble(time = t_end,
                                        value = df$value[nrow(df)]))
  }
  t_end <- max(max(timeseries_rain$time), max(timeseries_et$time))
  timeseries_et   <- extend_to(timeseries_et, t_end, "ET0")
  timeseries_rain <- extend_to(timeseries_rain, t_end, "Rain")

  # mm je Intervall -> mm/h (Engine liest beide Kurven als mm/h-Rate;
  # ET0-Tageswerte ohne /24 wuerden 24x zu hoch integriert)
  period_rain <- c(diff(timeseries_rain$time),
                   mean(diff(timeseries_rain$time)))
  timeseries_rain$value <- timeseries_rain$value / period_rain
  period_et <- c(diff(timeseries_et$time), mean(diff(timeseries_et$time)))
  timeseries_et$value <- timeseries_et$value / period_et

  list(rain = as.data.frame(timeseries_rain),
       et   = as.data.frame(timeseries_et))
}
