test_that("create runmap works", {



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


  run_map <- create_runs(PROFLUX,
                         params = list("TPS" = c(1,1.2),
                                       "Temp" = c(1,1.05)),
                         method = "permutation",
                         type = "factor"
                         )
  set.seed(42)
  run_map_2 <- create_runs(PROFLUX,
                         params = list("TPS" = c(1,1.2),
                                       "Temp" = c(1,1.05)),
                         method = "random",
                         type = "factor",
                         n_runs = 4
  )


  df <- dplyr::tibble(run_id = rep(1:4, each = 2),
                   param = rep(c("TPS","Temp"),times = 4),
                   value = c(1,1,1.2,1,1,1.05,1.2,1.05),
                   type = "factor",
                   gas = "CO2"
  )

  df_2 <-dplyr::tibble(run_id = rep(1:4, each = 2),
                       param = rep(c("TPS","Temp"),times = 4),
                       value = c(1.18, 1.05, 1.06, 1.04, 1.13, 1.03, 1.15, 1.01),
                       type = "factor",
                       gas = "CO2")

  expect_equal(run_map, df)
  expect_equal(run_map_2, df_2, tolerance = 0.01)


})
