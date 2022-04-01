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


  run_map <- run_map(PROFLUX,
                     params = list("TPS" = c(1,1.2),
                                   "t" = c(1,1.05)),
                     method = "permutation",
                     type = c("factor","factor")
  )
  set.seed(42)
  run_map_2 <- run_map(PROFLUX,
                       params = list("TPS" = c(1,1.2),
                                     "t" = c(1,1.05)),
                       method = "random",
                       type = c("factor", "factor"),
                       n_runs = 4
  )


  df <- dplyr::tibble(run_id = rep(1:4, each = 2),
                      param = rep(c("TPS","t"),times = 4),
                      value = c(1,1,1.2,1,1,1.05,1.2,1.05),
                      type = "factor",
                      gas = "CO2"
  ) %>%
    new_cfp_run_map(id_cols = c("gas"),
                    params = list("TPS" = c(1,1.2),
                                  "t" = c(1,1.05)),
                    method = "permutation",
                    type = c("factor","factor"),
                    n_runs = NULL,
                    layers_different = FALSE
    )

  df_2 <-dplyr::tibble(run_id = rep(1:4, each = 2),
                       param = rep(c("TPS","t"),times = 4),
                       value = c(1.18, 1.05, 1.06, 1.04, 1.13, 1.03, 1.15, 1.01),
                       type = "factor",
                       gas = "CO2") %>%
    new_cfp_run_map(id_cols = c("gas"),
                    params = list("TPS" = c(1,1.2),
                                  "t" = c(1,1.05)),
                    method = "random",
                    type = c("factor", "factor"),
                    n_runs = 4,
                    layers_different = FALSE
    )

  expect_equal(run_map, df)
  expect_equal(run_map_2, df_2, tolerance = 0.01)

})


test_that("permutation works", {


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
                     params = list("topheight" = c(-1,0,1),
                                   "TPS" = c(1,1.2)),
                     method = "permutation",
                     type = c("addition","factor")
  )

  df <- dplyr::tibble(run_id = rep(1:6, each = 2),
                      param = rep(c("topheight","TPS"),times = 6),
                      value = c(-1,1,0,1,1,1,-1,1.2,0,1.2,1,1.2),
                      type = rep(c("addition", "factor"), times = 6),
                      gas = "CO2"
  ) %>%
    new_cfp_run_map(id_cols = "gas",
                    params = list("topheight" = c(-1,0,1),
                                  "TPS" = c(1,1.2)),
                    method = "permutation",
                    type = c("addition","factor"),
                    n_runs = NULL,
                    layers_different = FALSE)

  expect_equal(run_map, df, tolerance = 0.01)
})
