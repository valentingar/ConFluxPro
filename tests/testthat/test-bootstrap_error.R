test_that("extend gasdata works", {

  base_dat2 <- base_dat <- ConFluxPro::base_dat
  base_dat2$gasdata$sd_x_ppm <- 25

  gd_ext <- create_extended_gasdata(base_dat$gasdata,
                                    gasdata_depths =
                                      depth_structure(base_dat,
                                                      structure_from = "gasdata"),
                                    sd_x_ppm = 25, 5)
  gd_ext2 <- create_extended_gasdata(base_dat$gasdata,
                                     gasdata_depths =
                                       depth_structure(base_dat,
                                                       structure_from = "gasdata"),
                                    sd_x_ppm = depth_structure(base_dat,
                                                               structure_from = "gasdata") %>%
                                      dplyr::mutate(sd_x_ppm = 0),
                                    5)
  gd_ext3 <- create_extended_gasdata(base_dat2$gasdata,
                                     gasdata_depths =
                                       depth_structure(base_dat2,
                                                       structure_from = "gasdata"),
                                     n_replicates = 5)


 expect_equal(nrow(gd_ext), 600)
 expect_equal(nrow(gd_ext2), 600)
 expect_equal(nrow(gd_ext3), 600)
 expect_equal(gd_ext2$x_ppm[gd_ext2$gd_id == 1 & gd_ext2$depth == 5][1],
              base_dat$gasdata$x_ppm[base_dat$gasdata$gd_id == 1 & base_dat$gasdata$depth == 5])

})

test_that("create_bootstrap_gasdata works", {

  base_dat <- ConFluxPro::base_dat

  gd_bs <- create_bootstrap_gasdata(base_dat$gasdata, n_samples = 5)

  expect_equal(nrow(gd_bs), 312*5)
  expect_contains(cfp_id_cols(gd_bs), c("bootstrap_id"))
})

test_that("bootstrapping works pro_flux", {
  PROFLUX <- readRDS(test_path("fixtures", "base_proflux.rds"))

  PF_BSE <- bootstrap_error(PROFLUX, n_samples = 2)

  expect_contains(names(PF_BSE$PROFLUX), c("DELTA_flux", "DELTA_prod"))
  expect_true(PF_BSE$PROFLUX$DELTA_flux[1] > 0)

})


test_that("bootstrapping provide rep_cols for gasdata",{

  gasdata <- ConFluxPro::gasdata %>%
    filter(site == "site_a") %>%
    cfp_gasdata(c("site", "Date"))
  soilphys <- ConFluxPro::soilphys %>%
    filter(site == "site_a", Date == "2021-02-01") %>%
    dplyr::select(!c("Date")) %>%
    cfp_soilphys(id_cols = "site")
  lmap <- cfp_layers_map(ConFluxPro::layers_map %>%
                           filter(site == "site_a"),
                         "site", gas = "CO2", highlim = 1000, lowlim = 0)
  PROFLUX <- pro_flux(cfp_dat(gasdata, soilphys, lmap))


  mod_BSE <- make_bootstrap_model(PROFLUX,
                                  n_samples = 2,
                                  rep_cols = "Date")
  expect_equal(n_profiles(mod_BSE), 2)


})
