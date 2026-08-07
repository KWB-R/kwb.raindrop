test_that("stack_levels: Vielfache einer Modulhoehe bis zur Obergrenze", {
  expect_equal(stack_levels(360), seq(360, 2520, by = 360))
  expect_equal(stack_levels(614), c(614, 1228, 1842, 2456))
  expect_true(all(stack_levels(400) <= 2600))
})

test_that("stack_levels: Mischkombination Vollblock + max. 1 Halbblock", {
  expect_equal(
    stack_levels(c(660, 350), max_count = c(7L, 1L)),
    c(350, 660, 1010, 1320, 1670, 1980, 2330)
  )
})

test_that("Presets und Default-Spezifikation", {
  presets <- sickerbox_level_presets()
  expect_equal(presets$brute_force, c(300, 600, 900, 1200))
  expect_true(all(vapply(presets, function(p) all(diff(p) > 0), logical(1))))

  spec <- default_storage_spec()
  expect_equal(spec$infiltration_box$levels, c(300, 600, 900, 1200))
  expect_equal(spec$gravel_trench$bounds, c(900, 3600))
})
