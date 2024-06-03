test_that("fg_flux works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  FLUX <-
    fg_flux(base_dat)

  expect_equal(nrow(FLUX$FLUX), 48)
  expect_equal(round(FLUX$FLUX$flux[10], 3),0.317)

})
