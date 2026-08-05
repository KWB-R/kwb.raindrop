# Gleiches synthetisches, monotones Hydraulikmodell wie in
# test-optimise_swale_design.R: Ueberlaeufe fallen mit der
# Rueckhaltekapazitaet cap = Flaeche x (Muldentiefe + Porositaet x
# Speicherhoehe). Kein Engine-Aufruf noetig -> die simultane Suche laesst
# sich gegen eine Brute-Force-Referenz und gegen die Bisektion verifizieren.
sim_run_factory <- function(demand) {
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

sim_fixed <- list(connected_area = 1000, filter_height = 300,
                  filter_hydraulicconductivity = 360,
                  bottom_hydraulicconductivity = 12)

# Brute-Force-Referenz: guenstigstes zulaessiges Design auf feinem Raster
sim_reference_optimum <- function(run, type, x, storage_values) {
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

test_that("Simultane Suche findet das Kostenminimum (Vergleich mit Brute-Force)", {
  run <- sim_run_factory(demand = 3.6e5)
  out <- optimise_swale_design_simultaneous(run, x_targets = 0:3,
                                            fixed = sim_fixed,
                                            verbose = FALSE)

  expect_true(all(out$status == "ok"))
  expect_false(any(out$monotonicity_warning))
  # Zulaessigkeit: Ueberlaufziel eingehalten
  expect_true(all(out$n_overflows <= out$x))

  for (i in seq_len(nrow(out))) {
    type <- out$storage_type[i]
    stor <- if (type == "infiltration_box") c(300, 600, 900, 1200)
            else seq(900, 3600, by = 25)
    ref <- sim_reference_optimum(run, type, out$x[i], stor)
    expect_false(is.null(ref))
    # innerhalb 5 % des (quasi-kontinuierlichen) Brute-Force-Optimums
    expect_lte(out$cost_total[i], ref$cost_total * 1.05)
  }

  # Kosten-Wirksamkeits-Kurve: lockereres Ziel ist nie teurer
  for (type in unique(out$storage_type)) {
    cc <- out$cost_total[out$storage_type == type][order(out$x[out$storage_type == type])]
    expect_true(all(diff(cc) <= 1e-9))
  }
})

test_that("Bisektion und simultane Suche bestaetigen sich gegenseitig", {
  run <- sim_run_factory(demand = 3.6e5)
  cd <- optimise_swale_design(run, x_targets = 0:2,
                              fixed = sim_fixed, verbose = FALSE)
  nm <- optimise_swale_design_simultaneous(run, x_targets = 0:2,
                                           fixed = sim_fixed,
                                           verbose = FALSE)
  # gleiche Zellen, gleicher Status, Kosten innerhalb 5 % voneinander
  expect_identical(nm$status, cd$status)
  expect_true(all(abs(nm$cost_total - cd$cost_total) <=
                    0.05 * pmin(nm$cost_total, cd$cost_total)))
})

test_that("hoher Bedarf erzwingt implizit einen groesseren Speicher", {
  run <- sim_run_factory(demand = 8e5)
  out <- optimise_swale_design_simultaneous(run, x_targets = 0,
                                            fixed = sim_fixed,
                                            verbose = FALSE)
  box <- out[out$storage_type == "infiltration_box", ]
  expect_identical(box$status, "ok")
  # bei Minimal-Speicher 300 ist selbst 200 m2 unzulaessig
  expect_gt(box$storage_height, 300)
  expect_lte(box$n_overflows, 0)
})

test_that("unloesbar innerhalb der Bounds ist ein regulaeres Ergebnis", {
  run <- sim_run_factory(demand = 5e6)
  out <- optimise_swale_design_simultaneous(run, x_targets = 0,
                                            fixed = sim_fixed,
                                            verbose = FALSE)
  expect_true(all(out$status == "infeasible_within_bounds"))
  expect_true(all(is.na(out$mulde_area)))
  expect_true(all(is.na(out$cost_total)))
})

test_that("Warmstart aus Rasterergebnissen liefert dasselbe Optimum", {
  run <- sim_run_factory(demand = 3.6e5)

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
    as.numeric(run(c(as.list(prior[i, ]), sim_fixed))$n_overflows)
  }, numeric(1))

  cold <- optimise_swale_design_simultaneous(run, x_targets = 0:2,
                                             fixed = sim_fixed,
                                             verbose = FALSE)
  warm <- optimise_swale_design_simultaneous(run, x_targets = 0:2,
                                             fixed = sim_fixed,
                                             prior_results = prior,
                                             verbose = FALSE)

  # gleiches Optimum (innerhalb der Suchtoleranzen)
  expect_equal(warm$cost_total, cold$cost_total, tolerance = 0.05)
})

test_that("max_total_depth wirkt als analytische Nebenbedingung", {
  run <- sim_run_factory(demand = 3.6e5)
  out <- optimise_swale_design_simultaneous(run, x_targets = 0,
                                            fixed = sim_fixed,
                                            max_total_depth = 1200,
                                            verbose = FALSE)
  ok <- out[out$status == "ok", ]
  expect_true(all(ok$mulde_height + ok$filter_height + ok$storage_height
                  <= 1200 + 1e-9))
})

test_that("leerer Suchraum (Tiefe verbietet alles) liefert Zeilen statt Fehler", {
  # max_total_depth = 500 laesst mit filter_height 300 und Muldentiefe
  # >= 100 keine einzige Speicherhoehe zu -> kein Engine-Lauf, aber ein
  # regulaeres Ergebnis (kein Absturz beim Zusammenbau der Attribute)
  run <- sim_run_factory(demand = 3.6e5)
  for (f in list(optimise_swale_design, optimise_swale_design_simultaneous)) {
    out <- f(run, x_targets = 0:1, fixed = sim_fixed,
             max_total_depth = 500, verbose = FALSE)
    expect_true(all(out$status == "infeasible_within_bounds"))
    expect_identical(attr(out, "n_runs_total"), 0L)
    expect_identical(nrow(attr(out, "evaluations")), 0L)
  }
})

test_that("degenerierte Rigol-Achse (genau eine zulaessige Hoehe) ist loesbar", {
  # max_total_depth = 1300: gb[2] wird auf gb[1] = 900 gedrueckt --
  # genau ein zulaessiger Speicherwert bleibt, die Zelle ist loesbar
  run <- sim_run_factory(demand = 3.6e5)
  out <- optimise_swale_design_simultaneous(
    run, x_targets = 5, fixed = sim_fixed,
    storage_spec = default_storage_spec()["gravel_trench"],
    max_total_depth = 1300, verbose = FALSE
  )
  expect_identical(out$status, "ok")
  expect_identical(out$storage_height, 900)
  expect_identical(out$mulde_height, 100)
})

test_that("NA-Zeilen im Prior stuerzen den Warmstart nicht ab", {
  run <- sim_run_factory(demand = 3.6e5)
  prior <- data.frame(
    mulde_area = c(150, NA), mulde_height = c(300, NA),
    storage_type = c("infiltration_box", NA),
    storage_height = c(300, NA),
    filter_hydraulicconductivity = c(360, NA),
    n_overflows = c(0, NA)
  )
  out <- optimise_swale_design_simultaneous(
    run, x_targets = 0, fixed = sim_fixed,
    storage_spec = default_storage_spec()["infiltration_box"],
    prior_results = prior, verbose = FALSE
  )
  expect_identical(out$status, "ok")
})

test_that("max_evals begrenzt die Suchphase messbar", {
  run <- sim_run_factory(demand = 3.6e5)
  small <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                              fixed = sim_fixed,
                                              max_evals = 15,
                                              verbose = FALSE)
  large <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                              fixed = sim_fixed,
                                              max_evals = 200,
                                              verbose = FALSE)
  expect_lt(attr(small, "n_runs_total"), attr(large, "n_runs_total"))
  # das Optimum leidet nicht wesentlich unter dem kleinen Budget
  expect_true(all(small$cost_total <= large$cost_total * 1.10))
})

test_that("Ergebnis traegt die Attribute evaluations und n_runs_total", {
  run <- sim_run_factory(demand = 3.6e5)
  out <- optimise_swale_design_simultaneous(
    run, x_targets = 1, fixed = sim_fixed,
    storage_spec = default_storage_spec()["infiltration_box"],
    verbose = FALSE
  )
  ev <- attr(out, "evaluations")
  expect_s3_class(ev, "tbl_df")
  expect_gt(nrow(ev), 0)
  expect_true(all(c("storage_type", "mulde_area", "mulde_height",
                    "storage_height", "n_overflows") %in% names(ev)))
  expect_identical(attr(out, "n_runs_total"), nrow(ev))
})

test_that("simultane Suche folgt veraenderten Kostensaetzen, Bisektion nicht", {
  # Die Bisektions-Reihenfolge (Flaeche zuerst, Speicher nur im
  # Notfall) kodiert die Default-Kostenhierarchie; cost_rates bepreist
  # dort nur nachtraeglich. Bei sehr billigem Speichermaterial liegt
  # das Optimum bei hoher Speicherstufe + kleiner Flaeche -- eine Ecke,
  # die die Bisektion nie besucht, die simultane Suche (cost_rates in
  # der Zielfunktion) aber findet.
  run <- sim_run_factory(demand = 3.6e5)
  cheap_box <- default_cost_rates()
  cheap_box$infiltration_box_eur_per_m3 <- 5
  spec <- default_storage_spec()["infiltration_box"]

  bis <- optimise_swale_design(run, x_targets = 0, fixed = sim_fixed,
                               storage_spec = spec,
                               cost_rates = cheap_box, verbose = FALSE)
  sim <- optimise_swale_design_simultaneous(run, x_targets = 0,
                                            fixed = sim_fixed,
                                            storage_spec = spec,
                                            cost_rates = cheap_box,
                                            verbose = FALSE)
  expect_identical(bis$status, "ok")
  expect_identical(sim$status, "ok")
  expect_gt(sim$storage_height, bis$storage_height)
  expect_lt(sim$cost_total, bis$cost_total * 0.95)
})

test_that("alle Methoden funktionieren mit Ein-Typ-storage_spec", {
  run <- sim_run_factory(demand = 3.6e5)
  for (m in c("nelder_mead", "diff_evolution", "halton_search")) {
    out <- optimise_swale_design_simultaneous(
      run, x_targets = 1, fixed = sim_fixed,
      storage_spec = default_storage_spec()["infiltration_box"],
      method = m, verbose = FALSE
    )
    expect_identical(out$status, "ok")
    expect_identical(out$method, m)
  }
})

test_that("alle Suchverfahren treffen das Brute-Force-Optimum", {
  run <- sim_run_factory(demand = 3.6e5)
  # NM ist am praezisesten, DE nah dran, Halton ist die naive Baseline
  slack <- c(nelder_mead = 1.05, diff_evolution = 1.08, halton_search = 1.12)
  for (m in names(slack)) {
    out <- optimise_swale_design_simultaneous(run, x_targets = c(0, 2),
                                              fixed = sim_fixed, method = m,
                                              verbose = FALSE)
    expect_true(all(out$status == "ok"), info = m)
    expect_true(all(out$method == m), info = m)
    expect_true(all(out$n_overflows <= out$x), info = m)
    for (i in seq_len(nrow(out))) {
      type <- out$storage_type[i]
      stor <- if (type == "infiltration_box") c(300, 600, 900, 1200)
              else seq(900, 3600, by = 25)
      ref <- sim_reference_optimum(run, type, out$x[i], stor)
      expect_lte(out$cost_total[i], ref$cost_total * slack[[m]])
    }
  }
})

test_that("Differential Evolution ist deterministisch und laesst Rs RNG in Ruhe", {
  run <- sim_run_factory(demand = 3.6e5)

  set.seed(4711)
  rng_before <- .Random.seed
  de1 <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                            fixed = sim_fixed,
                                            method = "diff_evolution",
                                            verbose = FALSE)
  # .Random.seed unveraendert: der interne LCG ersetzt Rs Zufallsstrom
  expect_identical(.Random.seed, rng_before)

  de2 <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                            fixed = sim_fixed,
                                            method = "diff_evolution",
                                            verbose = FALSE)
  expect_identical(de1$cost_total, de2$cost_total)
  expect_identical(de1$mulde_area, de2$mulde_area)

  # anderer Seed = anderer Suchpfad, aber gleiches Optimum (Toleranzen)
  de3 <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                            fixed = sim_fixed,
                                            method = "diff_evolution",
                                            seed = 99, verbose = FALSE)
  expect_equal(de3$cost_total, de1$cost_total, tolerance = 0.05)
  # Kanarienvogel gegen ein ignoriertes seed-Argument: die besuchten
  # Designs (Suchpfade) muessen sich unterscheiden, auch wenn die
  # Optima uebereinstimmen duerfen
  expect_false(identical(attr(de3, "evaluations"), attr(de1, "evaluations")))
})

test_that("verschiedene Start-Konfigurationen treffen dasselbe Optimum", {
  # verschiedene Suchpfade besuchen verschiedene Gitterpunkte -- die
  # Optima muessen innerhalb der Suchtoleranzen uebereinstimmen
  # (analog zur split_jitter-Erwartung der Bisektion)
  run <- sim_run_factory(demand = 3.6e5)
  few <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                            fixed = sim_fixed, n_starts = 1,
                                            verbose = FALSE)
  many <- optimise_swale_design_simultaneous(run, x_targets = 1,
                                             fixed = sim_fixed, n_starts = 5,
                                             max_evals = 120,
                                             verbose = FALSE)
  # Sickerbox: identische Stufe; Schotterrigol (stufenlos): eine
  # Toleranzstufe (25 mm) Spielraum
  tol_hs <- ifelse(many$storage_type == "infiltration_box", 0, 25)
  expect_true(all(abs(many$storage_height - few$storage_height) <= tol_hs))
  expect_true(all(abs(many$cost_total - few$cost_total) <=
                    0.03 * few$cost_total))
})
