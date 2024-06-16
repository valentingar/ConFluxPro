test_that("fg_flux works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  FLUX <-
    fg_flux(base_dat)

  expect_equal(nrow(FLUX$FLUX), 48)
  expect_equal(round(FLUX$FLUX$flux[10], 3),0.317)

})

test_that("fg_flux multiple gases", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  sp <- sp2 <- base_dat$soilphys
  gd <- gd2 <- base_dat$gasdata
  lmap <- lmap2 <- base_dat$layers_map

  sp$gas <- "CH4"
  gd$gas <- "CH4"
  lmap$gas <- "CH4"

  base_dat <- cfp_dat(cfp_gasdata(bind_rows(gd, gd2),
                                   id_cols = c("site", "gas", "Date")),
                      cfp_soilphys(bind_rows(sp, sp2),
                                   id_cols = c("site", "gas", "Date")),
                      cfp_layers_map(bind_rows(lmap, lmap2),
                                   id_cols = c("site", "gas")))

  FLUX <-
    fg_flux(base_dat)

  expect_equal(nrow(FLUX$FLUX), 96)

})
