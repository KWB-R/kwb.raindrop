#' Add overflow-event metrics and water-balance shares (percent) to simulation results
#'
#' Computes (i) overflow event statistics from the element outflow time series and
#' (ii) water-balance components as *percent shares* for both `element` and
#' `connected_area` per scenario.
#'
#' Water-balance percentages are computed with sign preserved (`value / denom`):
#' - **element** denominator: `WB_Regen + abs(WB_Oberflaechenablauf_Verschaltungen)`
#' - **connected_area** denominator: `WB_Regen` (fallback to
#'   `abs(WB_Oberflaechenablauf_Verschaltungen)` if `WB_Regen` is `NA` or `0`)
#'
#' Overflow events are derived from positive `Oberflaechenablauf_Ueberlauf` values
#' using `kwb.event::hsEvents()`. Event separation is controlled via
#' `event_separation_hours` (converted to seconds).
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
#' out <- add_overflow_events_and_waterbalance(simulation_results, event_separation_hours = 6)
#' }
#'
#' @importFrom dplyr %>% filter mutate relocate transmute if_else pull select bind_cols bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom lubridate as_datetime
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom kwb.event hsEvents
#' @export
add_overflow_events_and_waterbalance <- function(simulation_results,
                                                 event_separation_hours = 4) {
  
  stats::setNames(lapply(names(simulation_results), function(s_name) {
  
  # --- element: Prozent korrekt auf (Regen + Zufluss aus Verschaltungen) ----
  wb_el_raw <- simulation_results[[s_name]]$element$water_balance
  
  # denom = Regen + |Verschaltungen| (beide skalar aus der WB ziehen)
  denom_element <- wb_el_raw %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dplyr::transmute(denom = WB_Regen + abs(WB_Oberflaechenablauf_Verschaltungen)) %>%
    dplyr::pull(denom)
  
  wb_element <- wb_el_raw %>%
    dplyr::mutate(
      variable = sprintf("element.%s_", variable),
      value_percent = round(100 * value / denom_element, 2)   # <-- SIGN behalten!
    ) %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value_percent")
  
  # --- connected_area: Prozent korrekt (Basis = Regen; keine Rückkopplung) ----
  wb_ca_raw <- simulation_results[[s_name]]$connected_area$water_balance
  
  denom_connectedarea <- wb_ca_raw %>%
    tidyr::pivot_wider(names_from = variable, values_from = value) %>%
    dplyr::transmute(
      denom = dplyr::if_else(
        !is.na(WB_Regen) & WB_Regen != 0,
        WB_Regen,
        abs(WB_Oberflaechenablauf_Verschaltungen)  # Fallback
      )
    ) %>%
    dplyr::pull(denom)
  
  wb_connectedarea <- wb_ca_raw %>%
    dplyr::mutate(
      variable = sprintf("connectedarea.%s_", variable),
      value_percent = round(100 * value / denom_connectedarea, 2)  # SIGN behalten
    ) %>%
    dplyr::select(-value) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value_percent")
  
  xx <- simulation_results[[s_name]]$element$rates %>% 
    dplyr::filter(variable == "Oberflaechenablauf_Ueberlauf", 
                  value > 0) %>% 
    dplyr::mutate(datetime = lubridate::as_datetime(time*3600)) %>% 
    dplyr::relocate(datetime, .before = time)
  
  
  if(nrow(xx) > 0) {
    xx_events <-  kwb.event::hsEvents(tseries = xx$datetime,
                                      evtSepTime = 3600 * event_separation_hours,
                                      signalWidth = 0.1*3600,
                                      tUnit = "h")
    
    
    xx_events$overflow_sum <- sapply(seq_len(nrow(xx_events)), function(event)  {
      sum(xx[xx_events$iBeg[event]:xx_events$iEnd[event], "value"])
      
    })
  } 
  
  tibble::tibble(s_name = s_name,
                 n_overflows = ifelse(nrow(xx) > 0, nrow(xx_events), 0),
                 median_duration_overflows_hours = ifelse(nrow(xx) > 0, round(median(xx_events$dur), 2), NA),
                 sum_overflows = ifelse(nrow(xx) > 0, round(sum(xx_events$overflow_sum),2), 0)) %>% 
    dplyr::bind_cols(wb_element) %>% 
    dplyr::bind_cols(wb_connectedarea)
}), names(simulation_results)) %>% dplyr::bind_rows()
}