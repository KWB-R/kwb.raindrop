#' Add overflow-event metrics and water-balance shares (percent) to simulation results
#'
#' Computes (i) overflow event statistics from the element outflow time series and
#' (ii) water-balance components as percent shares for both `element` and
#' `connected_area` per scenario.
#'
#' Water-balance percentages are computed with sign preserved (`value / denom`):
#' - **element** denominator: `WB_Regen + abs(WB_Oberflaechenablauf_Verschaltungen)`
#' - **connected_area** denominator: `WB_Regen` (fallback to
#'   `abs(WB_Oberflaechenablauf_Verschaltungen)` if `WB_Regen` is `NA` or `0`)
#'
#' Overflow events are derived from positive `Oberflaechenablauf_Ueberlauf` values
#' using `kwb.event::hsEvents()`. Assuming the overflow rate is in `mm/h`, event
#' sums (in `mm`) are obtained by integrating each sample over its **local**
#' time step (`time[i+1] - time[i]`; the last sample inherits the previous
#' step). A warning is emitted if the time step is non-uniform, and
#' `sum_overflows` is returned as `NA` if it cannot be determined (single
#' sample).
#'
#' Missing components are tolerated: scenarios whose entry in `simulation_results`
#' is `NULL` (or which lack any of `element`, `element$water_balance`,
#' `connected_area`, `connected_area$water_balance` or `element$rates`) still
#' produce a row of the output tibble. The four "headline" columns (`s_name`,
#' `n_overflows`, `median_duration_overflows_hours`, `sum_overflows`) are
#' always present and filled with `NA` where they cannot be computed. The
#' `element.*_` and `connectedarea.*_` water-balance columns follow
#' `dplyr::bind_rows()` semantics: a column is added to the output as soon as
#' at least one scenario contributes it, with `NA` for the rows that don't. If
#' *every* scenario in `simulation_results` lacks a side (e.g. every run
#' disables roof ET, so no scenario contributes any `connectedarea.*_`), that
#' side's columns are absent from the output entirely. Downstream code that
#' addresses those columns by name should therefore check for their presence
#' rather than assume they exist.
#'
#' @param simulation_results Named list of scenario results. Each entry is expected
#'   to contain:
#'   - `element$water_balance` with columns `variable`, `value`
#'   - `connected_area$water_balance` with columns `variable`, `value`
#'   - `element$rates` with columns `time`, `variable`, `value`
#' @param event_separation_hours Numeric. Minimum time between two overflow events
#'   (in hours). Defaults to `4`.
#'
#' @return A tibble with one row per scenario containing:
#'   - `s_name`
#'   - `n_overflows`
#'   - `median_duration_overflows_hours`
#'   - `sum_overflows`
#'   - water-balance percentage columns for `element.*_` and `connectedarea.*_`
#'
#' @examples
#' \dontrun{
#' out <- add_overflow_events_and_waterbalance(simulation_results)
#' out <- add_overflow_events_and_waterbalance(
#'   simulation_results,
#'   event_separation_hours = 6
#' )
#' }
#'
#' @importFrom dplyr %>% filter mutate relocate transmute if_else pull select bind_cols bind_rows arrange
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate as_datetime
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom kwb.event hsEvents
#' @importFrom rlang .data
#' @export
add_overflow_events_and_waterbalance <- function(simulation_results,
                                                 event_separation_hours = 4) {

  na_row <- function(s_name) {
    tibble::tibble(
      s_name = s_name,
      n_overflows = NA_integer_,
      median_duration_overflows_hours = NA_real_,
      sum_overflows = NA_real_
    )
  }

  has_rows <- function(x) !is.null(x) && nrow(x) > 0L

  wb_percent <- function(wb_raw, prefix, denom_fn) {
    if (!has_rows(wb_raw)) return(tibble::tibble())
    wb_wide <- wb_raw %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "value")
    denom <- denom_fn(wb_wide)
    wb_raw %>%
      dplyr::mutate(
        variable      = sprintf("%s.%s_", prefix, .data$variable),
        value_percent = round(100 * .data$value / denom, 2)
      ) %>%
      dplyr::select(-"value") %>%
      tidyr::pivot_wider(names_from = "variable",
                         values_from = "value_percent")
  }

  denom_element <- function(d) {
    d$WB_Regen + abs(d$WB_Oberflaechenablauf_Verschaltungen)
  }
  denom_connectedarea <- function(d) {
    dplyr::if_else(
      !is.na(d$WB_Regen) & d$WB_Regen != 0,
      d$WB_Regen,
      abs(d$WB_Oberflaechenablauf_Verschaltungen)
    )
  }

  compute_overflows <- function(rates, s_name) {
    if (!has_rows(rates)) {
      return(list(n = NA_integer_, med_dur = NA_real_, sum = NA_real_))
    }

    rates_all <- rates %>%
      dplyr::filter(.data$variable == "Oberflaechenablauf_Ueberlauf") %>%
      dplyr::arrange(.data$time)
    n_rates <- nrow(rates_all)

    if (n_rates >= 2) {
      dt_vec <- diff(rates_all$time)
      if (diff(range(dt_vec, na.rm = TRUE)) > 1e-6) {
        warning(sprintf(
          paste0("Scenario '%s': non-uniform time step in ",
                 "'Oberflaechenablauf_Ueberlauf' (range %g to %g h); ",
                 "integrating with per-step dt."),
          s_name, min(dt_vec), max(dt_vec)
        ))
      }
      rates_all$dt_hours <- c(dt_vec, dt_vec[length(dt_vec)])
    } else {
      if (n_rates == 1) {
        warning(sprintf(
          paste0("Scenario '%s': only one 'Oberflaechenablauf_Ueberlauf' ",
                 "sample; cannot determine time step, sum_overflows is NA."),
          s_name
        ))
      }
      rates_all$dt_hours <- rep(NA_real_, n_rates)
    }

    xx <- rates_all %>%
      dplyr::filter(.data$value > 0) %>%
      dplyr::mutate(datetime = lubridate::as_datetime(.data$time * 3600)) %>%
      dplyr::relocate("datetime", .before = "time")

    if (nrow(xx) == 0L) {
      return(list(n = 0L, med_dur = NA_real_, sum = 0))
    }

    xx_events <- kwb.event::hsEvents(
      tseries = xx$datetime,
      evtSepTime = 3600 * event_separation_hours,
      signalWidth = 0.1 * 3600,
      tUnit = "h"
    )

    xx_events$overflow_sum <- vapply(seq_len(nrow(xx_events)), function(event) {
      idx <- xx_events$iBeg[event]:xx_events$iEnd[event]
      contributions <- xx$value[idx] * xx$dt_hours[idx]
      if (all(is.na(contributions))) NA_real_
      else sum(contributions, na.rm = TRUE)
    }, numeric(1))

    list(
      n       = nrow(xx_events),
      med_dur = round(stats::median(xx_events$dur), 2),
      sum     = if (all(is.na(xx_events$overflow_sum))) NA_real_
                else round(sum(xx_events$overflow_sum, na.rm = TRUE), 2)
    )
  }

  compute_one <- function(s_name) {
    res <- simulation_results[[s_name]]

    if (is.null(res)) {
      warning(sprintf(
        "Scenario '%s' is NULL — returning a row with NA for all metrics.",
        s_name
      ))
      return(na_row(s_name))
    }

    wb_element       <- wb_percent(res$element$water_balance,
                                   prefix   = "element",
                                   denom_fn = denom_element)
    wb_connectedarea <- wb_percent(res$connected_area$water_balance,
                                   prefix   = "connectedarea",
                                   denom_fn = denom_connectedarea)

    ov <- compute_overflows(res$element$rates, s_name)

    tibble::tibble(
      s_name                          = s_name,
      n_overflows                     = ov$n,
      median_duration_overflows_hours = ov$med_dur,
      sum_overflows                   = ov$sum
    ) %>%
      dplyr::bind_cols(wb_element) %>%
      dplyr::bind_cols(wb_connectedarea)
  }

  dplyr::bind_rows(lapply(names(simulation_results), compute_one))
}
