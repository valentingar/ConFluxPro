test_that("fg_flux works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  FLUX <-
    fg_flux(base_dat,
            gases = "CO2",
            modes = "LL",
            param = c("DS", "c_air"),
            funs = c("harm", "arith"))

  expect_equal(nrow(FLUX$FLUX), 48)
  expect_equal(round(FLUX$FLUX$flux[10], 3),0.317)

})
