#' Achievable storage-layer stack heights from module heights
#'
#' Enumerates all storage-layer heights that can be built by stacking
#' (and mixing) the given module heights, e.g. full blocks combined with
#' at most one half block.
#'
#' @param modules Numeric vector of module heights in mm (e.g. `c(660, 350)`
#'   for a full block plus a half block).
#' @param max_count Integer vector (recycled to `length(modules)`): maximum
#'   number of modules of each type in one stack. Defaults to 7 for every
#'   module (cf. GRAF EcoBloc smart, stackable up to 7 layers).
#' @param max_height Maximum total stack height in mm (default 2600).
#'
#' @return Sorted numeric vector of achievable stack heights in mm.
#'
#' @examples
#' stack_levels(360)                              # 360, 720, ..., 2520
#' stack_levels(c(660, 350), max_count = c(7, 1)) # Rigofill full + half block
#'
#' @export
stack_levels <- function(modules,
                         max_count = rep(7L, length(modules)),
                         max_height = 2600) {
  stopifnot(is.numeric(modules), all(modules > 0))
  max_count <- rep_len(as.integer(max_count), length(modules))
  counts <- Map(function(m, k) 0:min(k, floor(max_height / m)),
                modules, max_count)
  grid <- do.call(expand.grid, counts)
  h <- as.vector(as.matrix(grid) %*% modules)
  sort(unique(h[h > 0 & h <= max_height]))
}

#' Sickerbox storage-height presets (brute force default + manufacturers)
#'
#' Named list of storage-height level vectors (mm) for the infiltration-box
#' storage layer. `brute_force` is the default used by the workflow
#' vignettes (300/600/900/1200 mm -- itself a combination of several box
#' types). The manufacturer presets are generated with [stack_levels()]
#' from typical module heights of commercial block systems; verify against
#' the current data sheets before productive optimisation runs.
#'
#' @param max_height Maximum total stack height in mm passed to
#'   [stack_levels()] (default 2600).
#'
#' @return Named list of sorted numeric vectors (mm).
#'
#' @export
sickerbox_level_presets <- function(max_height = 2600) {
  list(
    brute_force          = c(300, 600, 900, 1200),
    graf_ecobloc_smart   = stack_levels(360, max_height = max_height),
    graf_ecobloc_420     = stack_levels(660, max_height = max_height),
    fraenkische_rigofill = stack_levels(c(660, 350), max_count = c(7L, 1L),
                                        max_height = max_height),
    aco_stormbrixx_hd    = stack_levels(614, max_height = max_height),
    aco_stormbrixx_sd    = stack_levels(342, max_height = max_height),
    wavin_aquacell       = stack_levels(400, max_height = max_height)
  )
}

#' Default storage specification for the swale-design optimiser
#'
#' Storage-layer search space per storage type: the infiltration box uses
#' discrete stack levels (default: the brute-force grid levels), the gravel
#' trench is continuous with bounds coupled to the box level range by
#' `coupling_factor` (default 3, approximating the usable-porosity ratio
#' 0.95 / 0.3).
#'
#' Each entry also carries the **usable porosity** of the storage layer
#' (box 0.95, trench 0.3, matching [default_storage_types()]). The
#' bisection optimiser uses it to *derive* its search order from the
#' cost rates (cost per mm of storage capacity); without a `porosity`
#' entry it falls back to the default-rate hierarchy (smallest storage
#' level first).
#'
#' @param levels Numeric vector of infiltration-box stack heights in mm.
#' @param coupling_factor Factor between gravel-trench bounds and the box
#'   level range.
#' @param gravel_tol Bisection tolerance for the continuous gravel-trench
#'   height in mm.
#'
#' @return Named list with entries `infiltration_box` (with `levels` and
#'   `porosity`) and `gravel_trench` (with `bounds`, `tol` and
#'   `porosity`).
#'
#' @export
default_storage_spec <- function(levels = sickerbox_level_presets()$brute_force,
                                 coupling_factor = 3,
                                 gravel_tol = 25) {
  list(
    infiltration_box = list(levels = sort(unique(levels)),
                            porosity = 0.95),
    gravel_trench    = list(bounds = coupling_factor * range(levels),
                            tol    = gravel_tol,
                            porosity = 0.3)
  )
}

#' Default storage-type soil presets (Speicher layer)
#'
#' Soil parameters of the storage (2nd Bodenschichtung) layer per storage
#' type, as used by the workflow vignettes: infiltration box ("Sickerbox",
#' thetaS 0.95) and gravel trench ("Schotterrigol", thetaS 0.3).
#'
#' @return Named list (per storage type) of lists with
#'   `Startwerte_theta_ActualSoilMoisture`, `thetaWP_MoistureAtWiltingPoint`,
#'   `thetaFC_MoistureAtFieldCapacity`, `thetaS_MoistureAtSaturation`.
#'
#' @export
default_storage_types <- function() {
  list(
    infiltration_box = list(
      Startwerte_theta_ActualSoilMoisture = 0,
      thetaWP_MoistureAtWiltingPoint      = 0,
      thetaFC_MoistureAtFieldCapacity     = 0,
      thetaS_MoistureAtSaturation         = 0.95
    ),
    gravel_trench = list(
      Startwerte_theta_ActualSoilMoisture = 0,
      thetaWP_MoistureAtWiltingPoint      = 0,
      thetaFC_MoistureAtFieldCapacity     = 0,
      thetaS_MoistureAtSaturation         = 0.3
    )
  )
}
