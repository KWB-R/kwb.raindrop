#' Default unit-cost rates for infiltration-swale installations
#'
#' Returns the default Austrian unit-cost rates used by [`compute_costs()`].
#' Rates were provided by Johannes Leimgruber (OeStaP) on 2026-03-27 for the
#' RAINDROP cost-optimisation work.
#'
#' @return A named `list` of rates in EUR per m² or m³:
#'   `excavation_eur_per_m3`, `profiling_eur_per_m2`,
#'   `filter_eur_per_m3`, `infiltration_box_eur_per_m3`,
#'   `gravel_trench_eur_per_m3`.
#'
#' @section Source rates (Leimgruber, 2026-03-27, "Kostenansätze Optimierung"):
#'   * Aushub (excavation, incl. loading + transport):    70 EUR/m³
#'   * Profilierung und Begrünung (profiling + greening): 10 EUR/m²
#'   * Bodenfilter (soil filter, incl. installation):    200 EUR/m³
#'   * Sickerbox (infiltration box, incl. installation): 350 EUR/m³
#'   * Schotterrigol (gravel trench, incl. installation): 50 EUR/m³
#'
#' @export
default_cost_rates <- function() {
  list(
    excavation_eur_per_m3       = 70,
    profiling_eur_per_m2        = 10,
    filter_eur_per_m3           = 200,
    infiltration_box_eur_per_m3 = 350,
    gravel_trench_eur_per_m3    = 50
  )
}

#' Compute construction costs for an infiltration-swale parameter grid
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
#' Optional column `storage_type` (character: `"infiltration_box"` or
#' `"gravel_trench"`) selects the per-scenario storage rate. If absent, the
#' value of the `storage_type` argument is used for every row.
#'
#' Cost formulas (Leimgruber, 2026-03-27):
#' \preformatted{
#'   cost_excavation = mulde_area * (mulde_h + filter_h + storage_h)/1000 * 70
#'   cost_profiling  = mulde_area * 10
#'   cost_filter     = mulde_area * filter_h/1000 * 200
#'   cost_storage    = mulde_area * storage_h/1000 * <350 | 50>
#'   cost_total      = sum of the four above
#' }
#'
#' @param param_grid `data.frame` / `tibble` — must contain the four geometry
#'   columns listed above. Other columns are passed through unchanged.
#' @param storage_type `character(1)` — `"infiltration_box"` (default;
#'   ~95% porosity, the Austrian "Sickerbox") or `"gravel_trench"`
#'   (~30% porosity, the Austrian "Schotterrigol"). Only used when
#'   `param_grid` does not have a `storage_type` column.
#' @param cost_rates `list` — unit costs as returned by
#'   [`default_cost_rates()`]. Override individual entries to run cost
#'   sensitivity analyses.
#'
#' @return The input `param_grid` with the columns `storage_type`,
#'   `cost_excavation`, `cost_profiling`, `cost_filter`,
#'   `cost_storage`, `cost_total` (all in EUR) appended.
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
#' # Infiltration box (default)
#' compute_costs(grid)
#'
#' # Gravel trench
#' compute_costs(grid, storage_type = "gravel_trench")
#'
#' # Per-scenario storage type
#' compute_costs(dplyr::mutate(grid,
#'                             storage_type = c("infiltration_box",
#'                                              "gravel_trench")))
#'
#' @export
compute_costs <- function(param_grid,
                          storage_type = c("infiltration_box", "gravel_trench"),
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

  unknown <- setdiff(unique(storage_type_vec),
                     c("infiltration_box", "gravel_trench"))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "compute_costs(): unknown storage_type value(s): %s. Expected 'infiltration_box' or 'gravel_trench'.",
      paste(shQuote(unknown), collapse = ", ")
    ))
  }

  storage_rate <- ifelse(
    storage_type_vec == "infiltration_box",
    cost_rates$infiltration_box_eur_per_m3,
    cost_rates$gravel_trench_eur_per_m3
  )

  # Heights are stored in mm; convert to m for the volumetric rates.
  mulde_h_m   <- param_grid$mulde_height   / 1000
  filter_h_m  <- param_grid$filter_height  / 1000
  storage_h_m <- param_grid$storage_height / 1000
  total_h_m   <- mulde_h_m + filter_h_m + storage_h_m

  cost_excavation <- param_grid$mulde_area * total_h_m  * cost_rates$excavation_eur_per_m3
  cost_profiling  <- param_grid$mulde_area              * cost_rates$profiling_eur_per_m2
  cost_filter     <- param_grid$mulde_area * filter_h_m * cost_rates$filter_eur_per_m3
  cost_storage    <- param_grid$mulde_area * storage_h_m * storage_rate

  param_grid %>%
    dplyr::mutate(
      storage_type    = storage_type_vec,
      cost_excavation = cost_excavation,
      cost_profiling  = cost_profiling,
      cost_filter     = cost_filter,
      cost_storage    = cost_storage,
      cost_total      = cost_excavation + cost_profiling +
                        cost_filter + cost_storage
    )
}
