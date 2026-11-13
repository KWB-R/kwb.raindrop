test_that("default_growth_factors() are the UrbanEVA monthly factors", {
  gf <- default_growth_factors()

  expect_length(gf, 12L)
  expect_named(gf, c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun",
                     "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"))
  expect_true(all(gf > 0))
  # The factors redistribute the ET potential seasonally without changing
  # its annual total: annual mean exactly 1.00.
  expect_equal(mean(gf), 1)
  # Seasonal shape: summer maximum, winter minimum.
  expect_equal(unname(gf[["Jun"]]), max(gf))
  expect_equal(unname(gf[["Jan"]]), min(gf))
})

test_that("make_growth_curve() expands factors on the engine time axis", {
  end_h <- 2 * 8760
  curve <- make_growth_curve("2011-01-01", end_time_hours = end_h)

  expect_s3_class(curve, "data.frame")
  expect_named(curve, c("time", "value"))
  # Starts at simulation hour 0 with the origin month's factor.
  expect_equal(curve$time[1L], 0)
  expect_equal(curve$value[1L], unname(default_growth_factors()[["Jan"]]))
  # Strictly increasing support points, terminal point pinned to the end.
  expect_true(all(diff(curve$time) > 0))
  expect_equal(curve$time[nrow(curve)], end_h)
  # Step semantics: the terminal point repeats the last month's value.
  expect_equal(curve$value[nrow(curve)], curve$value[nrow(curve) - 1L])
  # Two non-leap years from a January-1st origin: 24 month boundaries
  # inside [0, end) plus the terminal point (the 17520 h boundary is
  # dropped to keep times strictly increasing).
  expect_equal(nrow(curve), 25L)
  # Support points sit on month boundaries: February 2011 starts after
  # 31 days.
  expect_equal(curve$time[2L], 31 * 24)
  expect_equal(curve$value[2L], unname(default_growth_factors()[["Feb"]]))
})

test_that("make_growth_curve() clamps a mid-month origin to hour 0", {
  curve <- make_growth_curve("2011-01-15 12:00:00", end_time_hours = 8760)

  expect_equal(curve$time[1L], 0)
  expect_equal(curve$value[1L], unname(default_growth_factors()[["Jan"]]))
  expect_true(all(diff(curve$time) > 0))
})

test_that("make_growth_curve() validates its inputs", {
  expect_error(make_growth_curve("2011-01-01", end_time_hours = 8760,
                                 factors = rep(1, 11)))
  expect_error(make_growth_curve("2011-01-01", end_time_hours = 0))
  # Wrongly ordered names are rejected (January-first required) ...
  gf_wrong <- rev(default_growth_factors())
  expect_error(
    make_growth_curve("2011-01-01", end_time_hours = 8760,
                      factors = gf_wrong),
    "January-first"
  )
  # ... but unnamed factors are accepted.
  expect_silent(
    make_growth_curve("2011-01-01", end_time_hours = 8760,
                      factors = unname(default_growth_factors()))
  )
})

test_that("roof_type_values() returns the v2 overrides per roof type", {
  conv <- roof_type_values("conventional")
  expect_identical(
    conv,
    list(`//Berechnungsparameter/Hoernschemeyer_aktiv` = 1L)
  )
  # NULL leaves the base.h5 frame untouched: no overrides at all.
  expect_identical(
    roof_type_values("conventional", hoernschemeyer_aktiv = NULL),
    list()
  )

  ext <- roof_type_values("extensive")
  int <- roof_type_values("intensive")

  # v2 vegetation (D. Wicke, 2026-10-13; LAI swapped relative to v1).
  expect_equal(
    ext$`//Massnahmenelemente/Dach/Bodenschichtung/Schichtdicken`, 80)
  expect_equal(
    ext$`//Massnahmenelemente/Dach/Parameter_Evapotranspiration/KC_CropFactor`,
    0.94)
  expect_equal(
    ext$`//Massnahmenelemente/Dach/Parameter_Evapotranspiration/LAI_LeafAreaIndex`,
    3.9)
  expect_equal(
    int$`//Massnahmenelemente/Dach/Bodenschichtung/Schichtdicken`, 200)
  expect_equal(
    int$`//Massnahmenelemente/Dach/Parameter_Evapotranspiration/KC_CropFactor`,
    1.05)
  expect_equal(
    int$`//Massnahmenelemente/Dach/Parameter_Evapotranspiration/LAI_LeafAreaIndex`,
    5.0)

  # The two switches a roof that actually evaporates needs, plus the
  # sealed underside.
  for (roof in list(ext, int)) {
    expect_equal(
      roof$`//Massnahmenelemente/Dach/Berechnungsparameter/Infiltration_aktiv`,
      1L)
    expect_equal(
      roof$`//Massnahmenelemente/Dach/Berechnungsparameter/Evapotranspiration_aktiv`,
      1L)
    expect_equal(
      roof$`//Massnahmenelemente/Dach/Allgemein/Endversickerungsrate`, 0)
    expect_equal(
      roof$`//Massnahmenelemente/Dach/Parameter_Evapotranspiration/SL_LeafStorageCoefficient`,
      0.29)
    # Start moisture must sit above the wilting point, or transpiration
    # never starts.
    start <- roof$`//Massnahmenelemente/Dach/Bodenschichtung/Startwerte_theta_ActualSoilMoisture`
    expect_gt(start, roof$`//Bodenarten/Dach/thetaWP_MoistureAtWiltingPoint`)
  }

  # The substrate soil is deliberately identical for both green roofs.
  soil_keys <- grep("^//Bodenarten/Dach/", names(ext), value = TRUE)
  expect_length(soil_keys, 5L)
  expect_identical(ext[soil_keys], int[soil_keys])
})

test_that("roof_type_values() validates its inputs", {
  expect_error(roof_type_values("extensive", hoernschemeyer_aktiv = 2))
  expect_error(roof_type_values("extensive", hoernschemeyer_aktiv = c(1, 1)))
  expect_error(roof_type_values("green"))
})
