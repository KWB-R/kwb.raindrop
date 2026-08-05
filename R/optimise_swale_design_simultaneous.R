#' Cheapest feasible prior design as warm start for the simultaneous search
#'
#' Picks the cheapest feasible grid cell of the matching branch from prior
#' (brute-force) results in the workflow CSV schema. Returns a one-row
#' data.frame with `mulde_area`, `mulde_height`, `storage_height`, or
#' `NULL` if the prior does not contain a feasible cell for this branch.
#'
#' @keywords internal
#' @noRd
prior_start_design <- function(prior, type, x, filter_height, cost_rates) {
  needed <- c("storage_type", "storage_height", "mulde_height", "mulde_area",
              "n_overflows", "filter_hydraulicconductivity")
  if (is.null(prior) || !all(needed %in% names(prior))) return(NULL)
  kf_max <- suppressWarnings(
    max(prior$filter_hydraulicconductivity, na.rm = TRUE)
  )
  d <- prior[prior$storage_type == type &
               prior$filter_hydraulicconductivity == kf_max &
               !is.na(prior$n_overflows) & prior$n_overflows <= x, ,
             drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  if (!"filter_height" %in% names(d)) d$filter_height <- filter_height
  d <- compute_costs(d, cost_rates = cost_rates)
  d[which.min(d$cost_total),
    c("mulde_area", "mulde_height", "storage_height"), drop = FALSE]
}

#' Find the cost-optimal swale design by simultaneous parameter search
#'
#' Alternative to the coordinate-descent optimiser
#' ([optimise_swale_design()], bisection per parameter): all design
#' parameters -- `mulde_area`, `mulde_height` and `storage_height` -- are
#' optimised **simultaneously** with a penalised Nelder-Mead search
#' (`stats::optim()`). Infeasible designs (`n_overflows > x`) are not
#' excluded but penalised (any infeasible design is worse than any feasible
#' one; the number of excess events grades the penalty, steering the
#' simplex back towards feasibility), so the search moves freely through
#' the full parameter space and can trade the parameters against each
#' other in a single step -- it does not rely on the per-parameter
#' monotonicity that the bisection exploits.
#'
#' Three ingredients keep the number of engine runs in check:
#' \itemize{
#'   \item \strong{Snapping}: every candidate is snapped to the search
#'     tolerances (`area_tol`, `height_tol`, storage `tol` / discrete
#'     `levels`) before evaluation, so the shared cache absorbs repeated
#'     visits and the sweep over all `x_targets` reuses runs.
#'   \item \strong{Multistart}: `n_starts` deterministic starting points
#'     (prior warm start and the optimum of the previous overflow target
#'     first, then a *storage ladder* -- one anchor start per storage
#'     level, smallest level first -- then fixed space-filling points)
#'     guard against the simplex stalling on the plateaus that the
#'     snapping and the integer overflow count create, and make sure every
#'     storage level competes: along the feasibility boundary the cost
#'     valley is flat, so the cheapest (usually smallest) storage level is
#'     easily missed from a single start. Different starts take different
#'     search paths -- the counterpart of `split_jitter` in the bisection
#'     optimiser. Every start receives an equal slice of the remaining
#'     `max_evals` budget (unused runs roll over).
#'   \item \strong{Lattice polish}: from the best feasible design found,
#'     single tolerance steps downwards (cheaper by construction) are
#'     tested until no parameter can be reduced any further -- the result
#'     is locally optimal on the tolerance lattice.
#' }
#'
#' The discrete infiltration-box levels are mapped onto a continuous
#' latent axis (each level owns an equal share of `[0, 1]`), the gravel
#' trench is searched continuously. The filter conductivity is expected to
#' be fixed at the maximum via `fixed` (cost-free and dominant, see the
#' `monotonicity_analysis` vignette). `max_total_depth` is enforced by
#' construction (the `mulde_height` axis is compressed to the remaining
#' depth), so no simulation runs are spent on depth-invalid designs.
#'
#' Compared to [optimise_swale_design()] this needs more engine runs per
#' cell (typically 30-60 instead of ~15) but serves as an independent
#' cross-check: it can discover cheaper corners of the design space that
#' coordinate descent would miss if the parameter interaction were
#' stronger than the monotonicity analysis suggests.
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
#'   the workflow CSV schema, used as warm start (the cheapest feasible
#'   grid cell of the branch becomes the first Nelder-Mead start).
#' @param n_starts Number of Nelder-Mead starts per (storage type, x)
#'   cell (default 4). Warm starts (prior, previous target) count towards
#'   this number, then the storage-ladder anchors, then the space-filling
#'   points.
#' @param max_evals Soft cap on fresh engine runs per cell: once reached,
#'   the Nelder-Mead phase winds down (already cached designs remain
#'   free); the final lattice polish may add a few runs beyond the cap.
#'   Default 80 -- thanks to the shared cache the later `x_targets` of a
#'   storage type stay far below this.
#' @param wobble Maximum counting-artefact size tolerated at the upper
#'   corner (default 1, matching the +1 event-counting wobble): only if
#'   the maximal design overflows by more than `wobble` events is the
#'   cell declared infeasible without a search.
#' @param max_total_depth Optional analytic depth constraint in mm:
#'   `mulde_height + filter_height + storage_height <= max_total_depth`
#'   (e.g. from DWA-A 138 groundwater clearance or cover requirements).
#'   Enforced without any simulation runs.
#' @param cost_rates Unit costs, see [default_cost_rates()].
#' @param verbose Print one progress line per solved cell.
#'
#' @return Tibble with one row per (storage type, x), same schema as
#'   [optimise_swale_design()]: the optimal design (`mulde_area`,
#'   `mulde_height`, `storage_height`), its metrics (`n_overflows`,
#'   `overflow_volume_m3`, `et_pct`), cost columns from [compute_costs()],
#'   a `status` (`"ok"` or `"infeasible_within_bounds"`),
#'   `monotonicity_warning` (`TRUE` if a strictly larger design produced
#'   more overflows *and* more overflow volume among the cell's
#'   evaluations) and `n_runs_new` (fresh engine runs spent on this cell).
#'   All evaluated designs are attached as attribute `"evaluations"`.
#'
#' @examples
#' # synthetic monotone model: overflows fall with retention capacity
#' run <- function(params) {
#'   cap <- params$mulde_area *
#'     (params$mulde_height + 0.95 * params$storage_height)
#'   list(n_overflows = max(0, floor(3.6e5 / cap) - 3),
#'        sum_overflows = 800 * max(0, 3.6e5 / cap - 3))
#' }
#' opt <- optimise_swale_design_simultaneous(
#'   run, x_targets = 1,
#'   storage_spec = default_storage_spec()["infiltration_box"],
#'   verbose = FALSE
#' )
#' opt[, c("x", "mulde_area", "mulde_height", "storage_height", "cost_total")]
#'
#' @seealso [optimise_swale_design()] (coordinate descent / bisection),
#'   [make_swale_runner()], [default_storage_spec()]
#' @export
optimise_swale_design_simultaneous <- function(run_fn,
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
                                               n_starts = 4,
                                               max_evals = 80,
                                               wobble = 1L,
                                               max_total_depth = NULL,
                                               cost_rates = default_cost_rates(),
                                               verbose = TRUE) {

  stopifnot(is.function(run_fn), !is.null(fixed$filter_height),
            n_starts >= 1, max_evals >= 10)
  filter_height <- fixed$filter_height

  # --- shared evaluation cache (one engine run per distinct design) -------
  cache <- new.env(parent = emptyenv())
  runs_executed <- 0L

  cache_key <- function(type, area, h_m, h_s) {
    paste(type, format(area, digits = 10), format(h_m, digits = 10),
          format(h_s, digits = 10), sep = "|")
  }

  eval_design <- function(type, area, h_m, h_s) {
    key <- cache_key(type, area, h_m, h_s)
    hit <- cache[[key]]
    if (!is.null(hit)) return(hit)
    params <- c(list(mulde_area = area, mulde_height = h_m,
                     storage_type = type, storage_height = h_s), fixed)
    res <- as.list(run_fn(params))
    if (!"n_overflows" %in% names(res)) {
      stop("optimise_swale_design_simultaneous(): ",
           "run_fn() must return 'n_overflows'")
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

  cost_total_of <- function(type, area, h_m, h_s) {
    compute_costs(
      tibble::tibble(mulde_area = area, mulde_height = h_m,
                     filter_height = filter_height, storage_height = h_s,
                     storage_type = type),
      cost_rates = cost_rates
    )$cost_total
  }

  # --- analytic depth constraint ------------------------------------------
  hm_upper <- function(h_s) {
    up <- rep(height_bounds[2], length(h_s))
    if (!is.null(max_total_depth)) {
      up <- pmin(up, max_total_depth - filter_height - h_s)
    }
    up
  }

  snap_to <- function(v, origin, step) {
    origin + round((v - origin) / step) * step
  }

  # fixed space-filling starts in the unit cube (area, height, storage);
  # deterministic on purpose -- repeated calls give identical results
  default_starts <- list(
    c(0.70, 0.85, 0.25), c(0.35, 0.50, 0.65), c(0.15, 0.95, 0.85),
    c(0.55, 0.25, 0.45), c(0.80, 0.35, 0.75), c(0.25, 0.70, 0.15)
  )

  # --- pairwise dominance check over the evaluations of one cell ----------
  # violation :<=> a strictly larger design (all three parameters >=, at
  # least one >) has MORE overflows AND MORE overflow volume -- the
  # simultaneous analogue of the bisection's volume referee
  dominance_violation <- function(evs) {
    if (length(evs) < 2) return(FALSE)
    m <- do.call(rbind, lapply(evs, function(e) {
      c(e$area, e$h_m, e$h_s, e$n, e$vol)
    }))
    for (i in seq_len(nrow(m) - 1)) {
      for (j in (i + 1):nrow(m)) {
        if (any(is.na(m[i, 4:5])) || any(is.na(m[j, 4:5]))) next
        d <- m[i, 1:3] - m[j, 1:3]
        big <- if (all(d >= 0) && any(d > 0)) i
               else if (all(d <= 0) && any(d < 0)) j
               else next
        small <- if (big == i) j else i
        if (m[big, 4] > m[small, 4] && m[big, 5] > m[small, 5] + 1e-9) {
          return(TRUE)
        }
      }
    }
    FALSE
  }

  # --- solve one (storage type, x) cell ------------------------------------
  # returns list(row = tibble, best_u = unit-cube position of the optimum,
  # used as warm start for the next overflow target of the same type)
  solve_cell <- function(type, x, extra_start = NULL) {
    runs_before <- runs_executed
    spec <- storage_spec[[type]]
    if (is.null(spec)) {
      stop("optimise_swale_design_simultaneous(): ",
           "storage_spec has no entry '", type, "'")
    }
    discrete <- !is.null(spec$levels)
    mono_warn <- FALSE
    cell_evals <- list()

    infeasible_row <- function() list(
      row = tibble::tibble(
        x = x, storage_type = type, status = "infeasible_within_bounds",
        mulde_area = NA_real_, mulde_height = NA_real_,
        storage_height = NA_real_, n_overflows = NA_real_,
        overflow_volume_m3 = NA_real_, et_pct = NA_real_,
        monotonicity_warning = mono_warn,
        n_runs_new = runs_executed - runs_before
      ),
      best_u = NULL
    )

    if (discrete) {
      levels_all <- sort(spec$levels)
      levels_all <- levels_all[hm_upper(levels_all) >= height_bounds[1]]
      if (length(levels_all) == 0) return(infeasible_row())
      hs_max <- levels_all[length(levels_all)]
    } else {
      gb <- spec$bounds
      if (!is.null(max_total_depth)) {
        gb[2] <- min(gb[2], max_total_depth - filter_height - height_bounds[1])
      }
      if (gb[2] <= gb[1]) return(infeasible_row())
      s_tol <- if (is.null(spec$tol)) 25 else spec$tol
      hs_max <- gb[2]
    }

    # unit cube [0,1]^3 -> snapped physical design (depth-valid by
    # construction: the mulde_height axis is compressed to hm_upper(h_s))
    decode <- function(u) {
      u <- pmin(1, pmax(0, u))
      a <- snap_to(area_bounds[1] + u[1] * diff(area_bounds),
                   area_bounds[1], area_tol)
      a <- min(max(a, area_bounds[1]), area_bounds[2])
      h_s <- if (discrete) {
        levels_all[min(length(levels_all),
                       1L + as.integer(floor(u[3] * length(levels_all))))]
      } else {
        s <- snap_to(gb[1] + u[3] * (gb[2] - gb[1]), gb[1], s_tol)
        min(max(s, gb[1]), gb[2])
      }
      up <- hm_upper(h_s)
      h_m <- snap_to(height_bounds[1] + u[2] * (up - height_bounds[1]),
                     height_bounds[1], height_tol)
      h_m <- min(max(h_m, height_bounds[1]), up)
      list(area = a, h_m = h_m, h_s = h_s)
    }

    encode <- function(area, h_m, h_s) {
      u3 <- if (discrete) {
        i <- which.min(abs(levels_all - h_s))
        (i - 0.5) / length(levels_all)
      } else {
        (h_s - gb[1]) / (gb[2] - gb[1])
      }
      up <- hm_upper(h_s)
      u2 <- if (up > height_bounds[1]) {
        (h_m - height_bounds[1]) / (up - height_bounds[1])
      } else {
        0
      }
      u1 <- (area - area_bounds[1]) / diff(area_bounds)
      pmin(1, pmax(0, c(u1, u2, u3)))
    }

    # any infeasible design must be worse than any feasible one
    cost_cap <- cost_total_of(type, area_bounds[2], height_bounds[2], hs_max)
    best <- NULL

    consider <- function(area, h_m, h_s) {
      ev <- eval_design(type, area, h_m, h_s)
      cell_evals[[cache_key(type, area, h_m, h_s)]] <<- list(
        area = area, h_m = h_m, h_s = h_s,
        n = ev$n_overflows, vol = ev$overflow_volume_m3
      )
      cost <- cost_total_of(type, area, h_m, h_s)
      feasible <- !is.na(ev$n_overflows) && ev$n_overflows <= x
      if (feasible && (is.null(best) || cost < best$cost)) {
        best <<- list(area = area, h_m = h_m, h_s = h_s, cost = cost)
      }
      list(ev = ev, cost = cost, feasible = feasible)
    }

    # fast path: if even the maximal design is infeasible beyond the
    # counting wobble, the whole cell is (monotonicity) -- no search
    top <- consider(area_bounds[2], hm_upper(hs_max), hs_max)
    if (!top$feasible &&
        (is.na(top$ev$n_overflows) || top$ev$n_overflows > x + wobble)) {
      mono_warn <- dominance_violation(cell_evals)
      return(infeasible_row())
    }

    start_cap <- max_evals
    budget_hit <- function() runs_executed - runs_before >= start_cap

    objective <- function(u) {
      p <- decode(u)
      if (budget_hit() &&
          is.null(cache[[cache_key(type, p$area, p$h_m, p$h_s)]])) {
        return(4 * cost_cap)   # budget spent: only cached designs are free
      }
      r <- consider(p$area, p$h_m, p$h_s)
      n <- r$ev$n_overflows
      if (is.na(n)) return(4 * cost_cap)
      if (n <= x) {
        r$cost
      } else {
        cost_cap + r$cost + 0.05 * cost_cap * (n - x)
      }
    }

    # --- starts: prior warm start / previous target first, then one
    # anchor per storage level (min storage first -- the storage ladder
    # guards the flat cost valley along the feasibility boundary), then
    # fixed space-filling points --------------------------------------------
    starts <- list()
    ps <- prior_start_design(prior_results, type, x, filter_height,
                             cost_rates)
    if (!is.null(ps)) {
      starts <- c(starts, list(encode(ps$mulde_area, ps$mulde_height,
                                      ps$storage_height)))
    }
    if (!is.null(extra_start)) starts <- c(starts, list(extra_start))
    ladder_u3 <- if (discrete) {
      (seq_along(levels_all) - 0.5) / length(levels_all)
    } else {
      c(0.02, 0.30, 0.60, 0.90)
    }
    ladder <- lapply(seq_along(ladder_u3), function(i) {
      c(if (i %% 2 == 1) 0.85 else 0.45, 0.90, ladder_u3[[i]])
    })
    starts <- c(starts, ladder, default_starts)
    starts <- starts[seq_len(min(length(starts), n_starts))]

    # every start gets a slice of the remaining run budget, unused runs
    # roll over to the following starts
    for (si in seq_along(starts)) {
      used <- runs_executed - runs_before
      if (max_evals - used <= 2) break
      start_cap <- used + ceiling((max_evals - used) /
                                    (length(starts) - si + 1))
      stats::optim(starts[[si]], objective, method = "Nelder-Mead",
                   control = list(maxit = 200, reltol = 1e-4,
                                  warn.1d.NelderMead = FALSE))
    }

    if (is.null(best)) {
      mono_warn <- dominance_violation(cell_evals)
      return(infeasible_row())
    }

    # --- lattice polish: single tolerance steps downwards ----------------
    # (any reduction is cheaper by construction; stop when none is
    # feasible any more -> locally optimal on the tolerance lattice)
    for (polish_round in seq_len(20)) {
      b <- best
      candidates <- list()
      if (b$area - area_tol >= area_bounds[1] - 1e-9) {
        candidates <- c(candidates, list(
          list(area = b$area - area_tol, h_m = b$h_m, h_s = b$h_s)
        ))
      }
      if (b$h_m - height_tol >= height_bounds[1] - 1e-9) {
        candidates <- c(candidates, list(
          list(area = b$area, h_m = b$h_m - height_tol, h_s = b$h_s)
        ))
      }
      h_s_down <- if (discrete) {
        lower <- levels_all[levels_all < b$h_s]
        if (length(lower)) max(lower) else NA_real_
      } else {
        if (b$h_s - s_tol >= gb[1] - 1e-9) b$h_s - s_tol else NA_real_
      }
      if (!is.na(h_s_down)) {
        candidates <- c(candidates, list(
          list(area = b$area, h_m = b$h_m, h_s = h_s_down)
        ))
      }
      for (p in candidates) consider(p$area, p$h_m, p$h_s)
      if (best$cost >= b$cost - 1e-9) break
    }

    mono_warn <- dominance_violation(cell_evals)
    if (mono_warn) {
      warning(sprintf(
        paste0("optimise_swale_design_simultaneous(): a larger design ",
               "produced more overflows AND more overflow volume ",
               "(%s, x = %d) -- real non-monotonicity, result may be ",
               "unreliable for this cell."),
        type, x
      ), call. = FALSE)
    }

    final <- eval_design(type, best$area, best$h_m, best$h_s)
    if (isTRUE(verbose)) {
      message(sprintf(
        "[%s | x = %d] area %s m2, height %s mm, storage %s mm (%d neue Laeufe)",
        type, x, format(best$area), format(best$h_m), format(best$h_s),
        runs_executed - runs_before
      ))
    }
    list(
      row = tibble::tibble(
        x = x, storage_type = type, status = "ok",
        mulde_area = best$area, mulde_height = best$h_m,
        storage_height = best$h_s,
        n_overflows = final$n_overflows,
        overflow_volume_m3 = final$overflow_volume_m3,
        et_pct = final$et_pct,
        monotonicity_warning = mono_warn,
        n_runs_new = runs_executed - runs_before
      ),
      best_u = encode(best$area, best$h_m, best$h_s)
    )
  }

  # --- sweep: per storage type in ascending x (the optimum of x - 1 is
  # feasible for x too and seeds the next search) --------------------------
  xs <- sort(unique(as.integer(x_targets)))
  rows <- list()
  for (type in names(storage_spec)) {
    last_u <- NULL
    for (x in xs) {
      solved <- solve_cell(type, x, extra_start = last_u)
      rows[[length(rows) + 1L]] <- solved$row
      if (!is.null(solved$best_u)) last_u <- solved$best_u
    }
  }
  out <- dplyr::bind_rows(rows)

  out$filter_height <- filter_height
  out <- compute_costs(out, cost_rates = cost_rates)
  out <- dplyr::arrange(out, .data$storage_type, .data$x)

  evaluations <- dplyr::bind_rows(
    lapply(ls(cache), function(k) tibble::as_tibble(get(k, envir = cache)))
  )
  attr(out, "evaluations") <- dplyr::arrange(
    evaluations, .data$storage_type, .data$mulde_area
  )
  attr(out, "n_runs_total") <- runs_executed
  out
}
