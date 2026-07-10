test_that("kontinuierliche Bisektion findet die Schwelle innerhalb der Toleranz", {
  calls <- 0L
  f <- function(v) {
    calls <<- calls + 1L
    list(n_overflows = if (v >= 137.4) 0L else 10L)
  }
  res <- find_min_feasible(f, x_max = 0, lower = 25, upper = 200, tol = 2)
  expect_identical(res$status, "ok")
  expect_gte(res$value, 137.4)
  expect_lte(res$value, 137.4 + 2)
  # log2(175 / 2) ~ 6.5 -> hoechstens ~9 Laeufe inkl. Randtests
  expect_lte(res$n_evaluations, 9)
  # Memoisierung: jede Stelle nur einmal evaluiert
  expect_equal(calls, res$n_evaluations)
  expect_false(res$monotonicity_violation)
})

test_that("unterer Rand zulaessig -> at_lower_bound", {
  f <- function(v) list(n_overflows = if (v >= 10) 0L else 10L)
  res <- find_min_feasible(f, x_max = 0, lower = 25, upper = 200, tol = 2)
  expect_identical(res$status, "at_lower_bound")
  expect_equal(res$value, 25)
})

test_that("tief unzulaessiger Rand -> sofort infeasible (1 Lauf)", {
  f <- function(v) list(n_overflows = 99L)
  res <- find_min_feasible(f, x_max = 0, lower = 25, upper = 200, tol = 2)
  expect_identical(res$status, "infeasible")
  expect_true(is.na(res$value))
  expect_equal(res$n_evaluations, 1L)
})

test_that("Rand-Guard: +1-Zaehl-Wobble am oberen Rand frisst keine Loesung", {
  # zulaessig in [100, 180), am oberen Rand springt der Zaehler auf x+1
  n_fun <- function(v) if (v >= 180) 2L else if (v >= 100) 1L else 50L
  f <- function(v) list(n_overflows = n_fun(v))
  res <- find_min_feasible(f, x_max = 1, lower = 25, upper = 200, tol = 2)
  expect_identical(res$status, "ok")
  expect_gte(res$value, 100)
  expect_lte(res$value, 102)
})

test_that("Volumen-Schiedsrichter: Warnung nur bei echter Nicht-Monotonie", {
  # Zaehler springt bei 150 von 1 auf 2 -- Volumen steigt MIT: echte Verletzung
  f_bad <- function(v) list(
    n_overflows = if (v >= 150) 2L else if (v >= 100) 1L else 9L,
    overflow_volume_m3 = if (v >= 150) 200 else if (v >= 100) 100 else 5000
  )
  expect_warning(
    res_bad <- find_min_feasible(f_bad, x_max = 1, lower = 25, upper = 200,
                                 tol = 2),
    "non-monotonicity"
  )
  expect_true(res_bad$monotonicity_violation)

  # gleicher Zaehler-Sprung, aber Volumen faellt weiter: harmloser Wobble
  f_ok <- function(v) list(
    n_overflows = if (v >= 150) 2L else if (v >= 100) 1L else 9L,
    overflow_volume_m3 = 5000 - 20 * v
  )
  expect_no_warning(
    res_ok <- find_min_feasible(f_ok, x_max = 1, lower = 25, upper = 200,
                                tol = 2)
  )
  expect_false(res_ok$monotonicity_violation)
})

test_that("diskrete Levels: Binaersuche ueber Stufen", {
  lv <- c(300, 600, 900, 1200)
  f <- function(v) list(n_overflows = if (v >= 900) 0L else 7L)
  res <- find_min_feasible(f, x_max = 0, levels = lv)
  expect_identical(res$status, "ok")
  expect_equal(res$value, 900)
  expect_lte(res$n_evaluations, 3)

  # schon die kleinste Stufe reicht
  f2 <- function(v) list(n_overflows = 0L)
  res2 <- find_min_feasible(f2, x_max = 0, levels = lv)
  expect_identical(res2$status, "at_lower_bound")
  expect_equal(res2$value, 300)

  # keine Stufe reicht (tief unzulaessig)
  f3 <- function(v) list(n_overflows = 9L)
  res3 <- find_min_feasible(f3, x_max = 0, levels = lv)
  expect_identical(res3$status, "infeasible")
  expect_equal(res3$n_evaluations, 1L)
})
