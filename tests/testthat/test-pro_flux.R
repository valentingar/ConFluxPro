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


test_that("pro_flux zero_flux works", {

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
    reframe(upper = c(upper, 0),
            lower = c(0, -100)) %>%
    cfp_layers_map(gas = "CO2",
                   layer_couple = 0,
                   lowlim = 0,
                   highlim = 1000,
                   id_cols = "site")
  PROFLUX <-
    cfp_dat(gasdata,
            soilphys,
            lmap) %>%
    pro_flux(zero_flux = FALSE,
             zero_limits = c(0,1000))


  expect_equal(nrow(PROFLUX$PROFLUX),216)
})


test_that("pro_flux does not allow negative concentrations", {
  library(dplyr)

  soilphys <-
  data.frame(
    gas = "CO2",
    date = 1,
    upper =c(10,0),
    lower = c(0,-10),
    DS = c(-2E-4,
           2E-5),
    c_air = 44) %>%
    cfp_soilphys(id_cols = "date")

  gasdata <-
    data.frame(depth = c(10,0,-5,-10),
               x_ppm = c(400,100,2000,4000),
               date = 1,
               gas = "CO2") %>%
    cfp_gasdata(id_cols = "date")


  lmap <- data.frame(date = 1,
                     upper = c(10,0),
                     lower = c(0,-10)) %>%
    cfp_layers_map(gas = "CO2",
                   layer_couple = 0,
                   lowlim = 0,
                   highlim = 1000,
                   id_cols = "date")
  PROFLUX <-
    cfp_dat(gasdata,
            soilphys,
            lmap) %>%
    pro_flux()

  PROFLUX$PROFLUX

  expect_true(all(PROFLUX$PROFLUX$conc >=0))

  })
