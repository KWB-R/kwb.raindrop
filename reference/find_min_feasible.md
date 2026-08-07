# Smallest feasible parameter value via bisection (monotone threshold search)

Core building block of the swale-design optimiser: finds the smallest
value of one design parameter for which the overflow target is met
(`n_overflows <= x_max`), assuming quasi-monotone feasibility (larger
value = never more overflows; verified for the RAINDROP model in the
`monotonicity_analysis` vignette). Each evaluation halves the search
interval, so `ceiling(log2(range / tol))` evaluations suffice.

## Usage

``` r
find_min_feasible(
  evaluate,
  x_max,
  lower = NULL,
  upper = NULL,
  tol = 1,
  levels = NULL,
  wobble = 1L,
  split_jitter = 0,
  volume_column = "overflow_volume_m3",
  verbose = FALSE
)
```

## Arguments

- evaluate:

  `function(value)` returning a list / one-row data.frame with at least
  `n_overflows`; if it also contains `volume_column`, the volume referee
  is active. Evaluations are memoised per value.

- x_max:

  Feasibility target: feasible iff `n_overflows <= x_max`.

- lower, upper:

  Numeric search bounds (continuous mode).

- tol:

  Resolution of the continuous search (same unit as the value).

- levels:

  Sorted numeric vector of discrete candidate values (discrete mode,
  e.g. Sickerbox stack heights). If given, `lower`, `upper` and `tol`
  are ignored and a binary search over the levels is performed.

- wobble:

  Maximum counting-artefact size tolerated by the edge guard (default 1,
  matching the observed +1 flips).

- split_jitter:

  Numeric in `[0, 0.45]`, default 0. With 0 the interval is split
  exactly in half (deterministic). A positive value draws the split
  fraction uniformly from `0.5 +- split_jitter` – a Monte-Carlo of the
  *search path*: repeated runs with different seeds take different
  routes to the threshold and must agree within `tol` if the result is a
  property of the problem, not of the path.

- volume_column:

  Name of the volume element in the `evaluate` result used by the volume
  referee (default `"overflow_volume_m3"`).

- verbose:

  Print one line per evaluation.

## Value

List with

- value:

  smallest feasible value, or `NA` if infeasible

- n_overflows:

  overflow count at `value`

- status:

  `"ok"`, `"at_lower_bound"` (already feasible at the lower end – caller
  may widen the bracket) or `"infeasible"`

- evaluations:

  tibble of all evaluated values (value, n_overflows, volume), sorted by
  value

- n_evaluations:

  number of distinct evaluations

- monotonicity_violation:

  `TRUE` if the volume referee fired

## Details

Two safety rules from the monotonicity analysis are built in:

- **Edge guard**: if the upper bound is infeasible by no more than
  `wobble` events (the +1 counting artefact of the 4-h event
  separation), a descending ladder below the edge searches for a
  feasible anchor before the branch is declared infeasible.

- **Volume referee**: whenever `n_overflows` increases with the
  parameter (a counting flip), the overflow volume must have decreased;
  if the volume increased as well, a warning is emitted and
  `monotonicity_violation` is set (real non-monotonicity – never
  observed at the three validation sites).

## Examples

``` r
# synthetic monotone step function: feasible from 137.4 m2 on
f <- function(v) list(n_overflows = if (v >= 137.4) 0L else 10L)
find_min_feasible(f, x_max = 0, lower = 25, upper = 200, tol = 2)$value
#> [1] 138.4766
```
