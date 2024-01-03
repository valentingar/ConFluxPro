test_that("topheight only", {
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
  PROFLUX <-
    cfp_dat(gasdata,
            soilphys,
            lmap ) %>%
    pro_flux()


  run_map <- run_map(PROFLUX,
                     params = list("topheight" = c(-1)),
                     method = "permutation",
                     type = c("addition")
  )

  expect_error(
  alternate(PROFLUX,
            run_map = run_map,
            f = function(x) complete_soilphys(x, overwrite = TRUE),
            return_raw = TRUE),
  NA
  )


})

test_that("layers from soilphys", {
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
  PROFLUX <-
    cfp_dat(gasdata,
            soilphys,
            lmap ) %>%
    pro_flux()


  run_map <- run_map(PROFLUX,
                     params = list("topheight" = c(-1, 1),
                                   "TPS" = c(0.8, 1.2)),
                     method = "random",
                     type = c("addition", "factor"),
                     layers_different = TRUE,
                     layers_from = "soilphys",
                     n_runs = 2
  )

  expect_no_error(
    alternate(PROFLUX,
              run_map = run_map,
              f = function(x) complete_soilphys(x,
                                                overwrite = TRUE,
                                                DSD0_formula = "a*AFPS^b"),
              return_raw = TRUE)
  )


})
