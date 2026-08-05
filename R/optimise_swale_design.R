#' Warm-start area bracket from prior (brute-force) results
#'
#' Narrows the mulde_area search interval to one grid step around the
#' cheapest feasible grid cell of the matching branch, if prior results
#' contain it. Falls back to the full bounds otherwise.
#'
#' @keywords internal
#' @noRd
area_bracket_from_prior <- function(prior, type, h_s, h_m, x, bounds) {
  needed <- c("storage_type", "storage_height", "mulde_height", "mulde_area",
              "n_overflows", "filter_hydraulicconductivity")
  if (is.null(prior) || !all(needed %in% names(prior))) return(bounds)
  kf_max <- suppressWarnings(
    max(prior$filter_hydraulicconductivity, na.rm = TRUE)
  )
  d <- prior[prior$storage_type == type &
               prior$filter_hydraulicconductivity == kf_max &
               prior$storage_height == h_s &
               prior$mulde_height == h_m, , drop = FALSE]
  if (nrow(d) < 2) return(bounds)
  ok <- d$mulde_area[!is.na(d$n_overflows) & d$n_overflows <= x]
  if (length(ok) == 0) return(bounds)
  first_ok <- min(ok)
  areas <- sort(unique(d$mulde_area))
  step <- if (length(areas) > 1) min(diff(areas)) else diff(bounds)
  lo <- max(bounds[1], first_ok - step)
  hi <- min(bounds[2], first_ok)
  if (hi <= lo) bounds else c(lo, hi)
}

#' Find the cost-optimal swale design per overflow target
#'
#' Coordinate-descent optimiser built from a single primitive
#' ([find_min_feasible()], bisection over one parameter): shrink the
#' expensive lever first (`mulde_area`), then the cheap one
#' (`mulde_height`); the storage layer starts at its smallest level and is
#' only escalated when the area is stuck at its upper bound. The filter
#' conductivity is expected to be fixed at the maximum via `fixed` (it is
#' cost-free and dominant, see the `monotonicity_analysis` vignette). Every
#' engine run is cached, so the sweep over all `x_targets` and both storage
#' types shares evaluations.
#'
#' **The search order is derived from `cost_rates`** via a
#' specific-cost proxy (EUR per mm of storage capacity, capacity model
#' `V ~ area * (mulde_height + porosity * storage_height)`; the layer
#' porosity comes from `storage_spec`, see [default_storage_spec()]):
#' maximising `mulde_height` first is optimal for *any* rates under
#' this cost model (it costs only excavation, while area pays every
#' component), and the starting storage level is chosen as the
#' cheapest level per mm of capacity -- the smallest level under the
#' default rates, a high level when e.g. the storage material is cheap.
#' Without a `porosity` entry in `storage_spec` the legacy order
#' (smallest level first) is used. The proxy is a first-order
#' heuristic: it assumes capacity-additive levers and cannot rank
#' parameters with nonlinear hydraulic effects (e.g. a variable filter
#' conductivity) -- for those, and as the assumption-free cross-check,
#' use [optimise_swale_design_simultaneous()], which carries
#' `cost_rates` directly inside its objective.
#'
#' @param run_fn `function(params)` running one scenario and returning at
#'   least `n_overflows` plus `sum_overflows` (mm) or `overflow_volume_m3`;
#'   typically created with [make_swale_runner()]. `params` is a named list
#'   of `mulde_area`, `mulde_height`, `storage_type`, `storage_height` plus
#'   everything in `fixed`.
#' @param x_targets Integer vector of overflow targets (feasible :<=>
#'   `n_overflows <= x`), default `0:5`.
#' @param area_bounds,area_tol Search range (m2) and resolution for
#'   `mulde_area`.
#' @param height_bounds,height_tol Search range (mm) and resolution for
#'   `mulde_height`.
#' @param storage_spec Storage search space per type, see
#'   [default_storage_spec()]: discrete `levels` (infiltration box) or
#'   continuous `bounds` + `tol` (gravel trench).
#' @param fixed Named list of parameters passed unchanged to `run_fn`
#'   (connected area, filter geometry, kf at maximum, ...). Must contain
#'   `filter_height` for the cost model.
#' @param prior_results Optional data.frame with prior (grid) results in
#'   the workflow CSV schema, used as warm start (narrows the first area
#'   bracket to one grid step).
#' @param split_jitter Passed to [find_min_feasible()]: 0 (default) =
#'   deterministic halving; > 0 randomises every bisection split point
#'   (Monte-Carlo of the search path -- repeated runs with different
#'   seeds must agree within the search tolerances).
#' @param max_total_depth Optional analytic depth constraint in mm:
#'   `mulde_height + filter_height + storage_height <= max_total_depth`
#'   (e.g. from DWA-A 138 groundwater clearance or cover requirements).
#'   Enforced without any simulation runs.
#' @param cost_rates Unit costs, see [default_cost_rates()].
#' @param verbose Print one progress line per solved cell.
#'
#' @return Tibble with one row per (storage type, x): the optimal design
#'   (`mulde_area`, `mulde_height`, `storage_height`), its metrics
#'   (`n_overflows`, `overflow_volume_m3`, `et_pct`), cost columns from
#'   [compute_costs()], a `status` (`"ok"` or `"infeasible_within_bounds"`),
#'   `monotonicity_warning` (volume referee) and `n_runs_new` (fresh engine
#'   runs spent on this cell). All evaluated designs are attached as
#'   attribute `"evaluations"`.
#'
#' @seealso [optimise_swale_design_simultaneous()] (alternative: all
#'   parameters at once via penalised Nelder-Mead, as an independent
#'   cross-check of the coordinate descent), [find_min_feasible()],
#'   [make_swale_runner()], [default_storage_spec()]
#' @export
optimise_swale_design <- function(run_fn,
                                  x_targets = 0:5,
                                  area_bounds = c(25, 200),
                                  area_tol = 2,
                                  height_bounds = c(100, 300),
                                  height_tol = 10,
                                  storage_spec = default_storage_spec(),
                                  fixed = list(
                                    connected_area = 1000,
                                    filter_height = 300,
                                    filter_hydraulicconductivity = 360,
                                    bottom_hydraulicconductivity = 12
                                  ),
                                  prior_results = NULL,
                                  split_jitter = 0,
                                  max_total_depth = NULL,
                                  cost_rates = default_cost_rates(),
                                  verbose = TRUE) {

  stopifnot(is.function(run_fn), !is.null(fixed$filter_height))
  filter_height <- fixed$filter_height

  # --- shared evaluation cache (one engine run per distinct design) -------
  cache <- new.env(parent = emptyenv())
  runs_executed <- 0L

  eval_design <- function(type, area, h_m, h_s) {
    key <- paste(type, format(area, digits = 10), format(h_m, digits = 10),
                 format(h_s, digits = 10), sep = "|")
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)
    params <- c(list(mulde_area = area, mulde_height = h_m,
                     storage_type = type, storage_height = h_s), fixed)
    res <- as.list(run_fn(params))
    if (!"n_overflows" %in% names(res)) {
      stop("optimise_swale_design(): run_fn() must return 'n_overflows'")
    }
    vol <- res[["overflow_volume_m3"]]
    if (is.null(vol) && !is.null(res[["sum_overflows"]])) {
      vol <- res[["sum_overflows"]] * area / 1000  # mm x m2 / 1000 = m3
    }
    et <- res[["element.WB_Evapotranspiration_"]]
    out <- list(storage_type = type, mulde_area = area, mulde_height = h_m,
                storage_height = h_s,
                n_overflows = as.numeric(res$n_overflows),
                overflow_volume_m3 = if (is.null(vol)) NA_real_ else as.numeric(vol),
                et_pct = if (is.null(et)) NA_real_ else as.numeric(et))
    runs_executed <<- runs_executed + 1L
    assign(key, out, envir = cache)
    out
  }

  # --- analytic depth constraint ------------------------------------------
  hm_upper <- function(h_s) {
    up <- rep(height_bounds[2], length(h_s))
    if (!is.null(max_total_depth)) {
      up <- pmin(up, max_total_depth - filter_height - h_s)
    }
    up
  }

  # area search with warm-start bracket; widens the bracket when the
  # optimum turns out to lie below it
  search_area <- function(eval_a, x, bracket) {
    res <- find_min_feasible(eval_a, x_max = x,
                             lower = bracket[1], upper = bracket[2],
                             tol = area_tol, split_jitter = split_jitter)
    if (identical(res$status, "at_lower_bound") &&
        bracket[1] > area_bounds[1]) {
      res <- find_min_feasible(eval_a, x_max = x,
                               lower = area_bounds[1], upper = bracket[1],
                               tol = area_tol, split_jitter = split_jitter)
    }
    if (identical(res$status, "infeasible") &&
        bracket[2] < area_bounds[2]) {
      # warm start was too optimistic -> retry up to the full upper bound
      res <- find_min_feasible(eval_a, x_max = x,
                               lower = bracket[1], upper = area_bounds[2],
                               tol = area_tol, split_jitter = split_jitter)
    }
    res
  }

  # --- solve one (storage type, x) cell ------------------------------------
  solve_cell <- function(type, x) {
    runs_before <- runs_executed
    spec <- storage_spec[[type]]
    if (is.null(spec)) {
      stop("optimise_swale_design(): storage_spec has no entry '", type, "'")
    }
    discrete <- !is.null(spec$levels)
    mono_warn <- FALSE

    infeasible_row <- function() tibble::tibble(
      x = x, storage_type = type, status = "infeasible_within_bounds",
      mulde_area = NA_real_, mulde_height = NA_real_,
      storage_height = NA_real_, n_overflows = NA_real_,
      overflow_volume_m3 = NA_real_, et_pct = NA_real_,
      monotonicity_warning = mono_warn,
      n_runs_new = runs_executed - runs_before
    )

    # start storage level: derived from the cost rates when the spec
    # carries the layer porosity -- the cheapest level per mm of storage
    # capacity (capacity model V ~ area * (h_m + porosity * h_s), all
    # cost terms ~ area, so the area cancels out of the comparison).
    # Under the default rates this picks the smallest level (storage is
    # the expensive lever); under e.g. cheap storage material it starts
    # high where coordinate descent would otherwise never look. Without
    # a porosity entry: legacy order (smallest level first).
    choose_start_hs <- function(candidates) {
      p <- spec$porosity
      storage_rate <- switch(
        type,
        infiltration_box = cost_rates$infiltration_box_eur_per_m3,
        gravel_trench    = cost_rates$gravel_trench_eur_per_m3
      )
      if (is.null(p) || is.null(storage_rate)) return(min(candidates))
      sc <- vapply(candidates, function(s) {
        hm <- hm_upper(s)
        if (hm < height_bounds[1]) return(Inf)
        f <- cost_rates$excavation_eur_per_m3 *
               (hm + filter_height + s) / 1000 +
             cost_rates$profiling_eur_per_m2 +
             cost_rates$filter_eur_per_m3 * filter_height / 1000 +
             storage_rate * s / 1000
        f / (hm + p * s)
      }, numeric(1))
      candidates[which.min(sc)]
    }

    if (discrete) {
      levels_all <- sort(spec$levels)
      levels_all <- levels_all[hm_upper(levels_all) >= height_bounds[1]]
      if (length(levels_all) == 0) return(infeasible_row())
      h_s <- choose_start_hs(levels_all)
    } else {
      gb <- spec$bounds
      if (!is.null(max_total_depth)) {
        gb[2] <- min(gb[2], max_total_depth - filter_height - height_bounds[1])
      }
      # gb[2] == gb[1] is a degenerate but valid axis (exactly one
      # admissible storage height), only gb[2] < gb[1] is infeasible
      if (gb[2] < gb[1]) return(infeasible_row())
      gravel_tol <- if (is.null(spec$tol)) 25 else spec$tol
      # linear-fractional in h_s -> the proxy optimum is at an endpoint
      h_s <- choose_start_hs(c(gb[1], gb[2]))
    }

    a_star <- NA_real_
    repeat {
      h_m_up <- hm_upper(h_s)
      eval_a <- function(a) eval_design(type, a, h_m_up, h_s)
      bracket <- area_bracket_from_prior(prior_results, type, h_s, h_m_up,
                                         x, area_bounds)
      res_a <- search_area(eval_a, x, bracket)
      mono_warn <- mono_warn || res_a$monotonicity_violation
      if (!identical(res_a$status, "infeasible")) {
        a_star <- res_a$value
        break
      }
      # area stuck at the upper bound -> escalate the storage layer
      eval_s <- function(h) eval_design(type, area_bounds[2], hm_upper(h), h)
      if (discrete) {
        rest <- levels_all[levels_all > h_s]
        if (length(rest) == 0) return(infeasible_row())
        res_s <- find_min_feasible(eval_s, x_max = x, levels = rest,
                                   split_jitter = split_jitter)
      } else {
        if (h_s >= gb[2]) return(infeasible_row())
        res_s <- find_min_feasible(eval_s, x_max = x,
                                   lower = h_s, upper = gb[2],
                                   tol = gravel_tol,
                                   split_jitter = split_jitter)
      }
      mono_warn <- mono_warn || res_s$monotonicity_violation
      if (identical(res_s$status, "infeasible")) return(infeasible_row())
      h_s <- res_s$value
    }

    # shrink the cheap lever last: mulde_height at fixed (a*, h_s).
    # A second area pass is provably redundant: a smaller mulde_height
    # only weakens the hydraulics, so the minimal feasible area cannot
    # decrease any further.
    h_m_up <- hm_upper(h_s)
    h_m_star <- h_m_up
    if (h_m_up > height_bounds[1]) {
      res_h <- find_min_feasible(
        function(h) eval_design(type, a_star, h, h_s),
        x_max = x, lower = height_bounds[1], upper = h_m_up,
        tol = height_tol, split_jitter = split_jitter
      )
      mono_warn <- mono_warn || res_h$monotonicity_violation
      if (!identical(res_h$status, "infeasible")) h_m_star <- res_h$value
    }

    final <- eval_design(type, a_star, h_m_star, h_s)
    if (isTRUE(verbose)) {
      message(sprintf(
        "[%s | x = %d] area %s m2, height %s mm, storage %s mm (%d neue Laeufe)",
        type, x, format(a_star), format(h_m_star), format(h_s),
        runs_executed - runs_before
      ))
    }
    tibble::tibble(
      x = x, storage_type = type, status = "ok",
      mulde_area = a_star, mulde_height = h_m_star, storage_height = h_s,
      n_overflows = final$n_overflows,
      overflow_volume_m3 = final$overflow_volume_m3,
      et_pct = final$et_pct,
      monotonicity_warning = mono_warn,
      n_runs_new = runs_executed - runs_before
    )
  }

  # --- sweep all cells (shared cache makes repeats cheap) ------------------
  cells <- expand.grid(type = names(storage_spec),
                       x = sort(unique(as.integer(x_targets))),
                       stringsAsFactors = FALSE)
  out <- dplyr::bind_rows(
    lapply(seq_len(nrow(cells)),
           function(i) solve_cell(cells$type[i], cells$x[i]))
  )

  out$filter_height <- filter_height
  out <- compute_costs(out, cost_rates = cost_rates)
  out <- dplyr::arrange(out, .data$storage_type, .data$x)

  evaluations <- dplyr::bind_rows(
    lapply(ls(cache), function(k) tibble::as_tibble(get(k, envir = cache)))
  )
  if (nrow(evaluations) > 0) {
    # empty when every cell is analytically infeasible (no engine run)
    evaluations <- dplyr::arrange(
      evaluations, .data$storage_type, .data$mulde_area
    )
  }
  attr(out, "evaluations") <- evaluations
  attr(out, "n_runs_total") <- runs_executed
  out
}
