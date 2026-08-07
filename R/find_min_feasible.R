#' Smallest feasible parameter value via bisection (monotone threshold search)
#'
#' Core building block of the swale-design optimiser: finds the smallest
#' value of one design parameter for which the overflow target is met
#' (`n_overflows <= x_max`), assuming quasi-monotone feasibility (larger
#' value = never more overflows; verified for the RAINDROP model in the
#' monotonicity analysis,
#' <https://raindrop.kompetenz-wasser.io/optimisation/monotonicity_analysis/>).
#' Each evaluation halves the search interval, so
#' `ceiling(log2(range / tol))` evaluations suffice.
#'
#' Two safety rules from the monotonicity analysis are built in:
#' \itemize{
#'   \item \strong{Edge guard}: if the upper bound is infeasible by no more
#'     than `wobble` events (the +1 counting artefact of the 4-h event
#'     separation), a descending ladder below the edge searches for a
#'     feasible anchor before the branch is declared infeasible.
#'   \item \strong{Volume referee}: whenever `n_overflows` increases with
#'     the parameter (a counting flip), the overflow volume must have
#'     decreased; if the volume increased as well, a warning is emitted and
#'     `monotonicity_violation` is set (real non-monotonicity -- never
#'     observed at the three validation sites).
#' }
#'
#' @param evaluate `function(value)` returning a list / one-row data.frame
#'   with at least `n_overflows`; if it also contains `volume_column`, the
#'   volume referee is active. Evaluations are memoised per value.
#' @param x_max Feasibility target: feasible iff `n_overflows <= x_max`.
#' @param lower,upper Numeric search bounds (continuous mode).
#' @param tol Resolution of the continuous search (same unit as the value).
#' @param levels Sorted numeric vector of discrete candidate values
#'   (discrete mode, e.g. Sickerbox stack heights). If given, `lower`,
#'   `upper` and `tol` are ignored and a binary search over the levels is
#'   performed.
#' @param wobble Maximum counting-artefact size tolerated by the edge guard
#'   (default 1, matching the observed +1 flips).
#' @param split_jitter Numeric in `[0, 0.45]`, default 0. With 0 the
#'   interval is split exactly in half (deterministic). A positive value
#'   draws the split fraction uniformly from `0.5 +- split_jitter` --
#'   a Monte-Carlo of the *search path*: repeated runs with different
#'   seeds take different routes to the threshold and must agree within
#'   `tol` if the result is a property of the problem, not of the path.
#' @param volume_column Name of the volume element in the `evaluate` result
#'   used by the volume referee (default `"overflow_volume_m3"`).
#' @param verbose Print one line per evaluation.
#'
#' @return List with
#' \describe{
#'   \item{value}{smallest feasible value, or `NA` if infeasible}
#'   \item{n_overflows}{overflow count at `value`}
#'   \item{status}{`"ok"`, `"at_lower_bound"` (already feasible at the lower
#'     end -- caller may widen the bracket) or `"infeasible"`}
#'   \item{evaluations}{tibble of all evaluated values (value, n_overflows,
#'     volume), sorted by value}
#'   \item{n_evaluations}{number of distinct evaluations}
#'   \item{monotonicity_violation}{`TRUE` if the volume referee fired}
#' }
#'
#' @examples
#' # synthetic monotone step function: feasible from 137.4 m2 on
#' f <- function(v) list(n_overflows = if (v >= 137.4) 0L else 10L)
#' find_min_feasible(f, x_max = 0, lower = 25, upper = 200, tol = 2)$value
#'
#' @export
find_min_feasible <- function(evaluate,
                              x_max,
                              lower = NULL,
                              upper = NULL,
                              tol = 1,
                              levels = NULL,
                              wobble = 1L,
                              split_jitter = 0,
                              volume_column = "overflow_volume_m3",
                              verbose = FALSE) {

  stopifnot(split_jitter >= 0, split_jitter <= 0.45)

  discrete <- !is.null(levels)
  if (discrete) {
    grid <- sort(unique(levels))
    axis_lo <- 0                       # virtual: below the smallest level
    axis_hi <- length(grid)
    axis_tol <- 1
    to_value <- function(i) grid[[i]]
  } else {
    stopifnot(is.numeric(lower), is.numeric(upper), upper > lower, tol > 0)
    axis_lo <- lower
    axis_hi <- upper
    axis_tol <- tol
    to_value <- identity
  }

  evals <- new.env(parent = emptyenv())
  eval_at <- function(axis_pos) {
    v <- to_value(axis_pos)
    key <- format(v, digits = 15)
    if (!is.null(evals[[key]])) return(evals[[key]])
    res <- as.list(evaluate(v))
    if (!"n_overflows" %in% names(res)) {
      stop("find_min_feasible(): evaluate() must return an element 'n_overflows'")
    }
    row <- list(
      value = v,
      n_overflows = as.numeric(res$n_overflows),
      volume = if (volume_column %in% names(res)) {
        as.numeric(res[[volume_column]])
      } else {
        NA_real_
      }
    )
    assign(key, row, envir = evals)
    if (isTRUE(verbose)) {
      message(sprintf("  eval %s -> n_overflows = %s",
                      format(v), format(row$n_overflows)))
    }
    row
  }
  feasible <- function(axis_pos) {
    n <- eval_at(axis_pos)$n_overflows
    !is.na(n) && n <= x_max
  }

  status <- "ok"

  # 1) Upper edge: with monotone feasibility the whole range is infeasible
  #    if the upper edge is -- unless the edge is a +1 counting wobble, in
  #    which case a descending ladder looks for a feasible anchor below.
  if (!feasible(axis_hi)) {
    n_hi <- eval_at(axis_hi)$n_overflows
    anchor <- NA_real_
    if (!is.na(n_hi) && n_hi <= x_max + wobble) {
      offset <- axis_tol
      repeat {
        p <- axis_hi - offset
        if (discrete) p <- ceiling(p)
        if (p <= axis_lo) break
        if (feasible(p)) {
          anchor <- p
          break
        }
        offset <- offset * 2
      }
    }
    if (is.na(anchor)) {
      status <- "infeasible"
    } else {
      axis_hi <- anchor
    }
  }

  # 2) Bisection: invariant lo infeasible (or virtual/edge), hi feasible.
  best <- NA_real_
  if (!identical(status, "infeasible")) {
    lo <- axis_lo
    hi <- axis_hi
    if (!discrete && feasible(lo)) {
      hi <- lo                          # optimum at (or below) the lower end
    }
    while ((hi - lo) > axis_tol) {
      frac <- if (split_jitter > 0) {
        stats::runif(1, 0.5 - split_jitter, 0.5 + split_jitter)
      } else {
        0.5
      }
      mid <- lo + frac * (hi - lo)
      if (discrete) {
        mid <- floor(mid)
        if (mid <= lo) mid <- lo + 1
        if (mid >= hi) mid <- hi - 1
      }
      if (mid <= lo || mid >= hi) break
      if (feasible(mid)) hi <- mid else lo <- mid
    }
    best <- hi
    at_lower <- if (discrete) best <= 1 else best <= axis_lo
    if (at_lower) status <- "at_lower_bound"
  }

  # 3) Volume referee over all evaluations of this search
  ev <- do.call(rbind, lapply(ls(evals), function(k) {
    e <- get(k, envir = evals)
    data.frame(value = e$value, n_overflows = e$n_overflows,
               volume = e$volume)
  }))
  ev <- ev[order(ev$value), , drop = FALSE]
  rownames(ev) <- NULL
  monotonicity_violation <- FALSE
  if (nrow(ev) >= 2) {
    dn <- diff(ev$n_overflows)
    dv <- diff(ev$volume)
    flips <- which(!is.na(dn) & dn > 0)
    real <- flips[!is.na(dv[flips]) & dv[flips] > 1e-9]
    if (length(real) > 0) {
      monotonicity_violation <- TRUE
      warning(sprintf(
        paste0("find_min_feasible(): n_overflows AND overflow volume ",
               "increase between value %s and %s -- real non-monotonicity, ",
               "bisection result unreliable for this branch."),
        format(ev$value[real[1]]), format(ev$value[real[1] + 1])
      ), call. = FALSE)
    }
  }

  list(
    value = if (identical(status, "infeasible")) NA_real_ else to_value(best),
    n_overflows = if (identical(status, "infeasible")) {
      NA_real_
    } else {
      eval_at(best)$n_overflows
    },
    status = status,
    evaluations = tibble::as_tibble(ev),
    n_evaluations = nrow(ev),
    monotonicity_violation = monotonicity_violation
  )
}
