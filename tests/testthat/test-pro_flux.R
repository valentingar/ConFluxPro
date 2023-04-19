test_that("pro_flux works", {

  library(dplyr)

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
  PROFLUX <-
    cfp_dat(gasdata,
            soilphys,
             lmap ) %>%
    pro_flux()


  expect_equal(nrow(PROFLUX$PROFLUX),216)
  expect_equal(round(PROFLUX$PROFLUX$flux[1],digits = 3),0.038)
})

