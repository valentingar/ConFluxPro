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

test_that("fg_flux invalid profiles", {

  base_dat <- readRDS(testthat::test_path("fixtures",
                                          "base_dat.rds"))
  #lmap <-
  #data.frame(site = rep(c("site_a", "site_b"), each = 3),
  #           upper = c(5,0,-20,7,0,-20),
  #           lower = c(0,-20,-100, 0, -20, -100)) |>
  #  cfp_layers_map(gas = "CO2", id_cols = "site")
  #base_dat$layers_map <- lmap

  base_dat$gasdata$x_ppm <- 1000
  base_dat <- as_cfp_dat(base_dat)
  FLUX <- fg_flux(base_dat)

  expect_identical(max(abs(FLUX$FLUX$flux)), 0)
  expect_equal(sum(is.na(FLUX$FLUX$r2)), 48)



  })


