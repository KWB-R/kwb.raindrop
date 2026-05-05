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
#' @export
add_overflow_events_and_waterbalance <- function(simulation_results,
                                                 event_separation_hours = 4) {
  
  stats::setNames(lapply(names(simulation_results), function(s_name) {
    
    # --- element: Prozent auf (Regen + Zufluss aus Verschaltungen) ----------
    wb_el_raw <- simulation_results[[s_name]]$element$water_balance
    
    denom_element <- wb_el_raw %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::transmute(
        denom = WB_Regen + abs(WB_Oberflaechenablauf_Verschaltungen)
      ) %>%
      dplyr::pull(denom)
    
    wb_element <- wb_el_raw %>%
      dplyr::mutate(
        variable = sprintf("element.%s_", variable),
        value_percent = round(100 * value / denom_element, 2)
      ) %>%
      dplyr::select(-value) %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "value_percent")
    
    # --- connected_area: Prozent auf Regen; Fallback Verschaltungen ----------
    wb_ca_raw <- simulation_results[[s_name]]$connected_area$water_balance
    
    denom_connectedarea <- wb_ca_raw %>%
      tidyr::pivot_wider(names_from = variable, values_from = value) %>%
      dplyr::transmute(
        denom = dplyr::if_else(
          !is.na(WB_Regen) & WB_Regen != 0,
          WB_Regen,
          abs(WB_Oberflaechenablauf_Verschaltungen)
        )
      ) %>%
      dplyr::pull(denom)
    
    wb_connectedarea <- wb_ca_raw %>%
      dplyr::mutate(
        variable = sprintf("connectedarea.%s_", variable),
        value_percent = round(100 * value / denom_connectedarea, 2)
      ) %>%
      dplyr::select(-value) %>%
      tidyr::pivot_wider(names_from = "variable", values_from = "value_percent")
    
    # --- Overflow-Zeitreihe vorbereiten --------------------------------------
    rates_all <- simulation_results[[s_name]]$element$rates %>%
      dplyr::filter(variable == "Oberflaechenablauf_Ueberlauf") %>%
      dplyr::arrange(time)

    n_rates <- nrow(rates_all)

    # Per-row time step (h): each rate sample represents the interval to the
    # next sample (left-rectangle integration). The last sample inherits the
    # previous step so the vector lines up with `rates_all`.
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
      dplyr::filter(value > 0) %>%
      dplyr::mutate(
        datetime = lubridate::as_datetime(time * 3600)
      ) %>%
      dplyr::relocate(datetime, .before = time)
    
    if (nrow(xx) > 0) {
      
      xx_events <- kwb.event::hsEvents(
        tseries = xx$datetime,
        evtSepTime = 3600 * event_separation_hours,
        signalWidth = 0.1 * 3600,
        tUnit = "h"
      )
      
      xx_events$overflow_sum <- vapply(seq_len(nrow(xx_events)), function(event) {
        idx <- xx_events$iBeg[event]:xx_events$iEnd[event]
        contributions <- xx$value[idx] * xx$dt_hours[idx]
        if (all(is.na(contributions))) {
          NA_real_
        } else {
          sum(contributions, na.rm = TRUE)
        }
      }, numeric(1))

      n_overflows <- nrow(xx_events)
      median_duration_overflows_hours <- round(stats::median(xx_events$dur), 2)
      sum_overflows <- if (all(is.na(xx_events$overflow_sum))) {
        NA_real_
      } else {
        round(sum(xx_events$overflow_sum, na.rm = TRUE), 2)
      }

    } else {
      n_overflows <- 0
      median_duration_overflows_hours <- NA_real_
      sum_overflows <- 0
    }
    
    tibble::tibble(
      s_name = s_name,
      n_overflows = n_overflows,
      median_duration_overflows_hours = median_duration_overflows_hours,
      sum_overflows = sum_overflows
    ) %>%
      dplyr::bind_cols(wb_element) %>%
      dplyr::bind_cols(wb_connectedarea)
    
  }), names(simulation_results)) %>%
    dplyr::bind_rows()
}