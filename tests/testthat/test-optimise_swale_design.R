# Synthetisches, monotones Hydraulikmodell: Ueberlaeufe fallen mit der
# Rueckhaltekapazitaet cap = Flaeche x (Muldentiefe + Porositaet x
# Speicherhoehe). Kein Engine-Aufruf noetig -> der Optimierer laesst sich
# gegen eine Brute-Force-Referenz auf feinem Raster verifizieren.
synthetic_run_factory <- function(demand) {
  porosity <- c(infiltration_box = 0.95, gravel_trench = 0.3)
  function(params) {
    cap <- params$mulde_area *
      (params$mulde_height +
         porosity[[params$storage_type]] * params$storage_height)
    ratio <- demand / cap
    list(
      n_overflows = max(0, floor(ratio) - 3),
      sum_overflows = 800 * max(0, ratio - 3),
      element.WB_Evapotranspiration_ = 0.1 * params$mulde_area
    )
  }
}

test_fixed <- list(connected_area = 1000, filter_height = 300,
                   filter_hydraulicconductivity = 360,
                   bottom_hydraulicconductivity = 12)

# Brute-Force-Referenz: guenstigstes zulaessiges Design auf feinem Raster
reference_optimum <- function(run, type, x, storage_values) {
  grid <- expand.grid(
    mulde_area = seq(25, 200, by = 0.5),
    mulde_height = seq(100, 300, by = 5),
    storage_height = storage_values,
    stringsAsFactors = FALSE
  )
  grid$storage_type <- type
  porosity <- c(infiltration_box = 0.95, gravel_trench = 0.3)
  cap <- grid$mulde_area *
    (grid$mulde_height + porosity[[type]] * grid$storage_height)
  grid$n <- pmax(0, floor(environment(run)$demand / cap) - 3)
  grid <- grid[grid$n <= x, , drop = FALSE]
  if (nrow(grid) == 0) return(NULL)
  grid$filter_height <- 300
  costs <- compute_costs(grid)
  costs[which.min(costs$cost_total), , drop = FALSE]
}

test_that("Optimierer findet das Kostenminimum (Vergleich mit Brute-Force)", {
  run <- synthetic_run_factory(demand = 3.6e5)
  out <- optimise_swale_design(run, x_targets = 0:3,
                               fixed = test_fixed, verbose = FALSE)

  expect_true(all(out$status == "ok"))
  expect_false(any(out$monotonicity_warning))
  # Zulaessigkeit: Ueberlaufziel eingehalten
  expect_true(all(out$n_overflows <= out$x))

  for (i in seq_len(nrow(out))) {
    type <- out$storage_type[i]
    stor <- if (type == "infiltration_box") c(300, 600, 900, 1200)
            else seq(900, 3600, by = 25)
    ref <- reference_optimum(run, type, out$x[i], stor)
    expect_false(is.null(ref))
    # innerhalb 5 % des (quasi-kontinuierlichen) Brute-Force-Optimums
    expect_lte(out$cost_total[i], ref$cost_total * 1.05)
  }

  # Kosten-Wirksamkeits-Kurve: lockereres Ziel ist nie teurer
  for (type in unique(out$storage_type)) {
    cc <- out$cost_total[out$storage_type == type][order(out$x[out$storage_type == type])]
    expect_true(all(diff(cc) <= 1e-9))
  }

  # Laufbudget: alle 8 Zellen zusammen deutlich unter Brute-Force-Groesse
  expect_lte(attr(out, "n_runs_total"), 200)
})

test_that("Speicher-Eskalation greift, wenn die Flaeche am Anschlag klemmt", {
  run <- synthetic_run_factory(demand = 8e5)
  out <- optimise_swale_design(run, x_targets = 0,
                               fixed = test_fixed, verbose = FALSE)
  box <- out[out$storage_type == "infiltration_box", ]
  expect_identical(box$status, "ok")
  # bei Minimal-Speicher 300 ist selbst 200 m2 unzulaessig -> Eskalation
  expect_gt(box$storage_height, 300)
  expect_lte(box$n_overflows, 0)
})

test_that("unloesbar innerhalb der Bounds ist ein regulaeres Ergebnis", {
  run <- synthetic_run_factory(demand = 5e6)
  out <- optimise_swale_design(run, x_targets = 0,
                               fixed = test_fixed, verbose = FALSE)
  expect_true(all(out$status == "infeasible_within_bounds"))
  expect_true(all(is.na(out$mulde_area)))
  expect_true(all(is.na(out$cost_total)))
})

test_that("Warmstart aus Rasterergebnissen spart Laeufe", {
  run <- synthetic_run_factory(demand = 3.6e5)

  # Prior im CSV-Schema der Workflows (kf = 360, h_m = 300, Rasterschritt 25)
  prior <- expand.grid(
    mulde_area = seq(25, 200, by = 25),
    mulde_height = 300,
    storage_type = c("infiltration_box", "gravel_trench"),
    stringsAsFactors = FALSE
  )
  prior$storage_height <- ifelse(prior$storage_type == "infiltration_box",
                                 300, 900)
  prior$filter_hydraulicconductivity <- 360
  prior$n_overflows <- vapply(seq_len(nrow(prior)), function(i) {
    as.numeric(run(c(as.list(prior[i, ]), test_fixed))$n_overflows)
  }, numeric(1))

  cold <- optimise_swale_design(run, x_targets = 0:3,
                                fixed = test_fixed, verbose = FALSE)
  warm <- optimise_swale_design(run, x_targets = 0:3,
                                fixed = test_fixed, prior_results = prior,
                                verbose = FALSE)

  # identisches Ergebnis, weniger Laeufe
  expect_equal(warm$cost_total, cold$cost_total, tolerance = 0.02)
  expect_lt(attr(warm, "n_runs_total"), attr(cold, "n_runs_total"))
})

test_that("Such-MC: gejitterte Pfade treffen das deterministische Optimum", {
  run <- synthetic_run_factory(demand = 3.6e5)
  det <- optimise_swale_design(run, x_targets = 1, fixed = test_fixed,
                               verbose = FALSE)
  for (s in 1:3) {
    set.seed(s)
    jit <- optimise_swale_design(run, x_targets = 1, fixed = test_fixed,
                                 split_jitter = 0.3, verbose = FALSE)
    expect_equal(jit$storage_height, det$storage_height)
    expect_lte(max(abs(jit$mulde_area - det$mulde_area)), 2)      # area_tol
    expect_lte(max(abs(jit$mulde_height - det$mulde_height)), 10) # height_tol
    expect_lte(max(abs(jit$cost_total - det$cost_total) / det$cost_total),
               0.03)
  }
})

test_that("max_total_depth wirkt als analytische Nebenbedingung", {
  run <- synthetic_run_factory(demand = 3.6e5)
  out <- optimise_swale_design(run, x_targets = 0, fixed = test_fixed,
                               max_total_depth = 1200, verbose = FALSE)
  ok <- out[out$status == "ok", ]
  expect_true(all(ok$mulde_height + ok$filter_height + ok$storage_height
                  <= 1200 + 1e-9))
})
