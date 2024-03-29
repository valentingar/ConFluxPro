test_that("fg_flux works", {
  require(dplyr)

  soilphys <-
    ConFluxPro::soilphys %>%
    cfp_soilphys(id_cols = c("site", "Date"))

  gasdata <-
    ConFluxPro::gasdata %>%
    cfp_gasdata(id_cols = c("site", "Date"))


  lmap <- soilphys %>%
    select(upper,site) %>%
    distinct() %>%
    group_by(site) %>%
    slice_max(upper) %>%
    reframe(upper = c(upper,0),
              lower = c(0,-100)) %>%
    cfp_layers_map(gas = "CO2",
                   layer_couple = 0,
                   lowlim = 0,
                   highlim = 1000,
                   id_cols = "site")
  FLUX <-
    cfp_dat(gasdata,
            soilphys,
            lmap ) %>%
    fg_flux(gases = "CO2",
            modes = "LL",
            param = c("DS", "c_air"),
            funs = c("harm", "arith"))

  expect_equal(nrow(FLUX$FLUX), 48)
  expect_equal(round(FLUX$FLUX$flux[10], 3),0.316)

})
