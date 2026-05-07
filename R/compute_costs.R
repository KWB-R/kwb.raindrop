#' Default unit-cost rates for Mulde-Rigole infiltration installations
#'
#' Returns the default Austrian unit-cost rates used by [`compute_costs()`].
#' Rates were provided by Johannes Leimgruber (OeStaP) on 2026-03-27 for the
#' RAINDROP cost-optimisation work.
#'
#' @return A named `list` of rates in EUR per m² or m³:
#'   `aushub_eur_per_m3`, `profilierung_eur_per_m2`,
#'   `bodenfilter_eur_per_m3`, `sickerbox_eur_per_m3`,
#'   `schotterrigol_eur_per_m3`.
#'
#' @section References:
#' Johannes Leimgruber, 27.03.2026 — "Kostenansätze Optimierung":
#'   * Aushub (incl. loading + transport):   70 EUR/m³
#'   * Profilierung und Begrünung:           10 EUR/m²
#'   * Bodenfilter (incl. installation):    200 EUR/m³
#'   * Sickerbox (incl. installation):      350 EUR/m³
#'   * Schotterrigol (incl. installation):   50 EUR/m³
#'
#' @export
default_cost_rates <- function() {
  list(
    aushub_eur_per_m3        = 70,
    profilierung_eur_per_m2  = 10,
    bodenfilter_eur_per_m3   = 200,
    sickerbox_eur_per_m3     = 350,
    schotterrigol_eur_per_m3 = 50
  )
}

#' Compute construction costs for a Mulde-Rigole parameter grid
#'
#' Given a parameter grid that drives the simulation, attach a per-scenario
#' breakdown of construction costs plus a `cost_total` column suitable for
#' filtering / sorting in the results datatable.
#'
#' Required columns in `param_grid`:
#'   * `mulde_area` — element footprint area in m²
#'   * `mulde_height` — surface depression depth in **mm**
#'   * `filter_height` — soil-filter layer thickness in **mm**
#'   * `storage_height` — storage layer thickness in **mm**
#'
#' Optional column `storage_type` (character: `"Sickerbox"` or
#' `"Schotterrigol"`) selects the per-scenario storage rate. If absent, the
#' value of the `storage_type` argument is used for every row.
#'
#' Cost formulas (Johannes Leimgruber, 27.03.2026):
#' \preformatted{
#'   cost_aushub       = mulde_area * (mulde_h + filter_h + storage_h)/1000 * 70
#'   cost_profilierung = mulde_area * 10
#'   cost_bodenfilter  = mulde_area * filter_h/1000 * 200
#'   cost_speicher     = mulde_area * storage_h/1000 * <350 | 50>
#'   cost_total        = sum of the four above
#' }
#'
#' @param param_grid `data.frame` / `tibble` — must contain the four geometry
#'   columns listed above. Other columns are passed through unchanged.
#' @param storage_type `character(1)` — `"Sickerbox"` (default; ~95% porosity)
#'   or `"Schotterrigol"` (~30% porosity). Only used when `param_grid` does
#'   not have a `storage_type` column.
#' @param cost_rates `list` — unit costs as returned by
#'   [`default_cost_rates()`]. Override individual entries to run cost
#'   sensitivity analyses.
#'
#' @return The input `param_grid` with the columns `storage_type`,
#'   `cost_aushub`, `cost_profilierung`, `cost_bodenfilter`,
#'   `cost_speicher`, `cost_total` (all in EUR) appended.
#'
#' @examples
#' grid <- tibble::tibble(
#'   scenario_name = c("s00001", "s00002"),
#'   mulde_area = c(50, 50),
#'   mulde_height = c(300, 300),
#'   filter_height = c(300, 300),
#'   storage_height = c(600, 600)
#' )
#'
#' # Sickerbox (default)
#' compute_costs(grid)
#'
#' # Schotterrigol
#' compute_costs(grid, storage_type = "Schotterrigol")
#'
#' # Per-scenario storage type
#' compute_costs(dplyr::mutate(grid,
#'                             storage_type = c("Sickerbox", "Schotterrigol")))
#'
#' @export
compute_costs <- function(param_grid,
                          storage_type = c("Sickerbox", "Schotterrigol"),
                          cost_rates = default_cost_rates()) {

  required <- c("mulde_area", "mulde_height", "filter_height", "storage_height")
  missing <- setdiff(required, names(param_grid))
  if (length(missing) > 0L) {
    stop(sprintf("compute_costs(): param_grid is missing required columns: %s",
                 paste(missing, collapse = ", ")))
  }

  default_storage <- match.arg(storage_type)

  storage_type_vec <- if ("storage_type" %in% names(param_grid)) {
    as.character(param_grid$storage_type)
  } else {
    rep(default_storage, nrow(param_grid))
  }

  unknown <- setdiff(unique(storage_type_vec), c("Sickerbox", "Schotterrigol"))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "compute_costs(): unknown storage_type value(s): %s. Expected 'Sickerbox' or 'Schotterrigol'.",
      paste(shQuote(unknown), collapse = ", ")
    ))
  }

  storage_rate <- ifelse(
    storage_type_vec == "Sickerbox",
    cost_rates$sickerbox_eur_per_m3,
    cost_rates$schotterrigol_eur_per_m3
  )

  # Heights are stored in mm; convert to m for the volumetric rates.
  mulde_h_m   <- param_grid$mulde_height   / 1000
  filter_h_m  <- param_grid$filter_height  / 1000
  storage_h_m <- param_grid$storage_height / 1000
  total_h_m   <- mulde_h_m + filter_h_m + storage_h_m

  cost_aushub       <- param_grid$mulde_area * total_h_m  * cost_rates$aushub_eur_per_m3
  cost_profilierung <- param_grid$mulde_area              * cost_rates$profilierung_eur_per_m2
  cost_bodenfilter  <- param_grid$mulde_area * filter_h_m * cost_rates$bodenfilter_eur_per_m3
  cost_speicher     <- param_grid$mulde_area * storage_h_m * storage_rate

  param_grid %>%
    dplyr::mutate(
      storage_type      = storage_type_vec,
      cost_aushub       = cost_aushub,
      cost_profilierung = cost_profilierung,
      cost_bodenfilter  = cost_bodenfilter,
      cost_speicher     = cost_speicher,
      cost_total        = cost_aushub + cost_profilierung +
                          cost_bodenfilter + cost_speicher
    )
}
