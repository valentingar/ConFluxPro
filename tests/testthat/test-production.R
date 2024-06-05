test_that("production pfres works", {

  PROFLUX <- readRDS(test_path("fixtures", "base_proflux.rds"))

  PROD <- production(PROFLUX)

  expect_equal(nrow(PROD), 48)
  expect_equal(round(PROD$prod_abs[5], 3), 0.149)
  expect_equal(round(PROD$prod_rel[15], 3), 0.079)

})

test_that("production fgres works", {

  base_dat <- readRDS(test_path("fixtures", "base_dat.rds"))

  FLUX <- fg_flux(base_dat)

  PROD <- production(FLUX)

  expect_equal(nrow(PROD), 48)
  expect_equal(round(PROD$prod_abs[5], 3), 0.101)
  expect_equal(round(PROD$prod_rel[15], 3), 0.145)

})
