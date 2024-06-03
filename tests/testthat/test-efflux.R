test_that("efflux works", {

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))

  EFFLUX <-
    efflux(PROFLUX)


  expect_equal(nrow(EFFLUX), 24)
  expect_equal(round(EFFLUX$efflux, 1),
               c(0.8,0.7,1.4,1.1,1.7,1.4,4.1,3.3,5.9,4.5,7.2,5.7,10.1,8.5,8.1,6.9,6.2,5.2,4.0,3.2,1.7,1.6,1.2,1.1))


})


test_that("efflux from fgres works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  FLUX <-
    fg_flux(base_dat)

  EFFLUX <-
  efflux(FLUX)

  expect_equal(nrow(EFFLUX), 24)
  expect_equal(round(EFFLUX$efflux[4],3), 0.441)


})
