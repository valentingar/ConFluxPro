test_that("topheight only", {


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
    summarise(upper = c(upper,0),
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



test_that("no change",{


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
    summarise(upper = c(upper,0),
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
                     params = list("topheight" = c(0,1),
                                   "TPS" = c(1,2)),
                     method = "random",
                     n_runs = 1,
                     type = c("addition", "factor")
  ) %>%
    mutate(value = ifelse(type == "factor",1,0))

  PF_nochange <-
  alternate(PROFLUX,
            run_map = run_map,
            f = function(x) complete_soilphys(x, overwrite = TRUE),
            return_raw = TRUE)[[1]]

  expect_equal(efflux(PF_nochange), efflux(PROFLUX))



})
