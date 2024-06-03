test_that("efflux works", {

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
  EFFLUX <-
    cfp_dat(gasdata,
            soilphys,
            lmap ) %>%
    pro_flux() %>%
    efflux()

  expect_equal(nrow(EFFLUX), 24)
  expect_equal(round(EFFLUX$efflux, 1),
               c(0.8,0.7,1.4,1.1,1.7,1.4,4.1,3.3,5.9,4.5,7.2,5.7,10.1,8.5,8.1,6.9,6.2,5.2,4.0,3.2,1.7,1.6,1.2,1.1))


})
