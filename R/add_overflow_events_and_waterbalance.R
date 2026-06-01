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
#' always present and filled with `NA` where they cannot be computed.
#'
#' For the `element.*_` and `connectedarea.*_` water-balance columns: if one
#' side is missing in a given scenario but the other side has data, a stub of
#' `NA`-filled columns is fabricated for the missing side by mirroring the
#' populated side's variable names. This preserves the table's column
#' structure when, for example, every run disables roof ET (so the engine
#' skips writing Dach.h5 and every scenario has `connected_area = NULL`):
#' the `connectedarea.*_` columns are kept and filled with `NA`, instead of
#' being dropped from the output entirely. The mirror is a best-effort hint
#' for the user, not a guarantee that the names match what a populated
#' `connected_area` would have produced.
#'
#' @param simulation_results Named list of scenario results. Each entry is expected
#'   to contain:
#'   - `element$water_balance` with columns `variable`, `value`
#'   - `connected_area$water_balance` with columns `variable`, `value`
#'   - `element$rates` with columns `time`, `variable`, `value`
#' @param event_separation_hours Numeric. Minimum time between two overflow events
#'   (in hours). Defaults to `4`.
#' @param canonical_variables Optional `character()` vector of water-balance
#'   variable names (without the `element.` / `connectedarea.` prefix and
#'   without the trailing `_`), e.g. `default_canonical_wb_variables()`.
#'   This is a **per-scenario** fallback: for any scenario whose
#'   `wb_element` and `wb_connectedarea` are both empty after the regular
#'   pivot / mirror logic (including scenarios that are entirely `NULL`),
#'   the function attaches `element.<var>_` and `connectedarea.<var>_`
#'   `NA`-filled stub columns built from this list. This guarantees the
#'   output tibble keeps the expected water-balance column structure even
#'   when no scenario contributes real data — `dplyr::bind_rows()` would
#'   otherwise drop columns that no row supplies. Defaults to `NULL` (no
#'   canonical fallback).
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
                                                 event_separation_hours = 4,
                                                 canonical_variables = NULL) {

  # Build the canonical NA stub templates once -- canonical_variables is a
  # closure-captured constant, so the two tibbles are reused by every
  # fallback branch below instead of rebuilt per scenario.
  canonical_stub <- function(prefix) {
    if (is.null(canonical_variables) || length(canonical_variables) == 0L) {
      return(tibble::tibble())
    }
    stub_names <- sprintf("%s.%s_", prefix, canonical_variables)
    tibble::as_tibble(stats::setNames(
      rep(list(NA_real_), length(stub_names)),
      stub_names
    ))
  }
  element_stub_template       <- canonical_stub("element")
  connectedarea_stub_template <- canonical_stub("connectedarea")

  # Diagnostic accumulators: scenario names that hit each fallback path.
  # Aggregated into one summary message at the end of the function, instead
  # of one near-identical line per affected scenario.
  null_scenarios            <- character()
  mirrored_from_element     <- character()
  mirrored_from_connected   <- character()
  fallback_canonical        <- character()

  na_row <- function(s_name) {
    tibble::tibble(
      s_name = s_name,
      n_overflows = NA_integer_,
      median_duration_overflows_hours = NA_real_,
      sum_overflows = NA_real_
    ) %>%
      dplyr::bind_cols(element_stub_template) %>%
      dplyr::bind_cols(connectedarea_stub_template)
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

  # If one side's water balance is missing but the other's is populated,
  # fabricate NA stub columns mirroring the populated side's variable
  # names. Without this, dplyr::bind_rows() drops columns that no
  # scenario contributed, and the final table loses the entire side --
  # which is what happens to connectedarea.*_ when every scenario in a
  # batch disables roof ET (the engine then skips writing Dach.h5).
  #
  # Naming contract: this depends on wb_percent() emitting column names
  # of shape `<prefix>.<variable>_` (sprintf("%s.%s_", prefix, variable)).
  # If wb_percent's naming scheme changes, the regex below has to follow.
  # Names are sliced positionally with the known reference prefix and
  # trailing "_" so columns that don't match the wb_percent contract are
  # dropped rather than turned into a misnamed stub.
  mirror_stub <- function(reference_wb, reference_prefix, target_prefix) {
    if (ncol(reference_wb) == 0L) return(tibble::tibble())
    pat <- paste0("^", reference_prefix, "\\..*_$")
    keep <- grepl(pat, names(reference_wb))
    if (!any(keep)) return(tibble::tibble())
    vars <- sub(paste0("^", reference_prefix, "\\."), "",
                names(reference_wb)[keep])
    vars <- sub("_$", "", vars)
    stub_names <- sprintf("%s.%s_", target_prefix, vars)
    tibble::as_tibble(stats::setNames(
      rep(list(NA_real_), length(stub_names)),
      stub_names
    ))
  }

  compute_one <- function(s_name) {
    res <- simulation_results[[s_name]]

    if (is.null(res)) {
      null_scenarios <<- c(null_scenarios, s_name)
      return(na_row(s_name))
    }

    wb_element       <- wb_percent(res$element$water_balance,
                                   prefix   = "element",
                                   denom_fn = denom_element)
    wb_connectedarea <- wb_percent(res$connected_area$water_balance,
                                   prefix   = "connectedarea",
                                   denom_fn = denom_connectedarea)

    if (ncol(wb_connectedarea) == 0L && ncol(wb_element) > 0L) {
      wb_connectedarea <- mirror_stub(wb_element,
                                      reference_prefix = "element",
                                      target_prefix    = "connectedarea")
      if (ncol(wb_connectedarea) > 0L) {
        mirrored_from_element <<- c(mirrored_from_element, s_name)
      }
    }
    if (ncol(wb_element) == 0L && ncol(wb_connectedarea) > 0L) {
      wb_element <- mirror_stub(wb_connectedarea,
                                reference_prefix = "connectedarea",
                                target_prefix    = "element")
      if (ncol(wb_element) > 0L) {
        mirrored_from_connected <<- c(mirrored_from_connected, s_name)
      }
    }
    # If both sides are still empty for this scenario, fall back to the
    # caller's canonical variable list so the column structure still
    # appears in the rendered datatable (otherwise dplyr::bind_rows
    # downstream drops every wb column when no scenario in the batch
    # contributes any).
    if (ncol(wb_element) == 0L && ncol(wb_connectedarea) == 0L) {
      wb_element       <- element_stub_template
      wb_connectedarea <- connectedarea_stub_template
      if (ncol(wb_element) > 0L || ncol(wb_connectedarea) > 0L) {
        fallback_canonical <<- c(fallback_canonical, s_name)
      }
    }

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

  out <- dplyr::bind_rows(lapply(names(simulation_results), compute_one))

  # One summary line per fallback path, instead of one line per scenario.
  if (length(null_scenarios) > 0L) {
    warning(sprintf(
      "Scenario(s) NULL -- row(s) with NA for all metrics: %s",
      paste(null_scenarios, collapse = ", ")
    ), call. = FALSE)
  }
  if (length(mirrored_from_element) > 0L) {
    message(sprintf(
      "connected_area absent for %d scenario(s); mirrored NA stub columns from element: %s",
      length(mirrored_from_element),
      paste(mirrored_from_element, collapse = ", ")
    ))
  }
  if (length(mirrored_from_connected) > 0L) {
    message(sprintf(
      "element absent for %d scenario(s); mirrored NA stub columns from connected_area: %s",
      length(mirrored_from_connected),
      paste(mirrored_from_connected, collapse = ", ")
    ))
  }
  if (length(fallback_canonical) > 0L) {
    message(sprintf(
      "Both wb sides absent for %d scenario(s); using canonical NA stub columns: %s",
      length(fallback_canonical),
      paste(fallback_canonical, collapse = ", ")
    ))
  }

  out
}
