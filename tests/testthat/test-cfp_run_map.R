test_that("create runmap works", {

  library(dplyr)

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))


  run_map <- cfp_run_map(PROFLUX,
                     params = list("TPS" = c(1,1.2),
                                   "t" = c(1,1.05)),
                     method = "permutation",
                     type = c("factor","factor")
  )
  set.seed(42)
  run_map_2 <- cfp_run_map(PROFLUX,
                       params = list("TPS" = c(1,1.2),
                                     "t" = c(1,1.05)),
                       method = "random",
                       type = c("factor", "factor"),
                       n_runs = 4
  )


  df <- data.frame(run_id = rep(1:4, each = 2),
                      param = rep(c("TPS","t"),times = 4),
                      value = c(1,1,1.2,1,1,1.05,1.2,1.05),
                      type = "factor",
                      gas = "CO2",
                   param_id = rep(1:2,times = 4)
  ) %>%
    ConFluxPro:::new_cfp_run_map(id_cols = c("gas"),
                    params = list("TPS" = c(1,1.2),
                                  "t" = c(1,1.05)),
                    method = "permutation",
                    type = c("factor","factor"),
                    n_runs = 4,
                    layers_different = FALSE,
                    layers_from = "layers_map",
                    layers_altmap = NULL,
                    runmap_type = "base",
                    params_df = data.frame(param = c("TPS","t"),
                                           param_id = c(1,2))
    )

  df_2 <-data.frame(run_id = rep(1:4, each = 2),
                       param = rep(c("TPS","t"),times = 4),
                       value = c(1.18, 1.05, 1.06, 1.04, 1.13, 1.03, 1.15, 1.01),
                       type = "factor",
                       gas = "CO2",
                    param_id = rep(1:2,times = 4)) %>%
    ConFluxPro:::new_cfp_run_map(id_cols = c("gas"),
                    params = list("TPS" = c(1,1.2),
                                  "t" = c(1,1.05)),
                    method = "random",
                    type = c("factor", "factor"),
                    n_runs = 4,
                    layers_different = FALSE,
                    layers_from = "layers_map",
                    layers_altmap = NULL,
                    runmap_type = "base",
                    params_df = data.frame(param = c("TPS","t"),
                                           param_id = c(1,2))
    )

  expect_equal(run_map, df)
  expect_equal(run_map_2, df_2, tolerance = 0.01)

})


test_that("permutation works", {


  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))

  run_map <- cfp_run_map(PROFLUX,
                     params = list("topheight" = c(-1,0,1),
                                   "TPS" = c(1,1.2)),
                     method = "permutation",
                     type = c("addition","factor")
  )

  df <- data.frame(run_id = rep(1:6, times = 2),
                      param = rep(c("TPS","topheight"),each = 6),
                      value = c(1,1,1,1.2,1.2,1.2,-1,0,1,-1,0,1),
                      type = rep(c("factor", "addition"), each = 6),
                      gas = "CO2",
                   param_id = rep(1:2,each = 6)
  ) %>%
    new_cfp_run_map(id_cols = "gas",
                    params = list("topheight" = c(-1,0,1),
                                  "TPS" = c(1,1.2)),
                    method = "permutation",
                    type = c("addition", "factor"),
                    n_runs = 6,
                    layers_different = FALSE,
                    layers_from = "layers_map",
                    layers_altmap = NULL,
                    runmap_type = "base",
                    params_df = data.frame(param = c("TPS", "topheight"),
                                               param_id = c(1,2)))

  expect_equal(run_map, df, tolerance = 0.01)
})




test_that("topheight adjust", {

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))

  expect_error(
  run_map <- cfp_run_map(PROFLUX,
                     params = list("topheight" = c(-100,0,1),
                                   "TPS" = c(1,1.2)),
                     method = "permutation",
                     type = c("addition","factor")
  ))

  run_mappy <- cfp_run_map(PROFLUX,
                     params = list("topheight" = c(-4,0,1),
                                   "TPS" = c(1,1.2)),
                     method = "permutation",
                     type = c("addition","factor"),
                     topheight_adjust = TRUE
  )

  min_topheight <-
  run_mappy %>%
    dplyr::filter(param == "topheight") %>%
    dplyr::group_by(site) %>%
    dplyr::slice_min(value, with_ties = FALSE)

  expect_equal(min_topheight$value[which(min_topheight$site == c("site_a", "site_b"))],
               c(-4,0))

  expect_error(
    run_mappo <- cfp_run_map(PROFLUX,
                       params = list("topheight" = c(-4,1),
                                     "TPS" = c(1,1.2)),
                       method = "random",
                       type = c("addition","factor"),
                       n_runs = 10,
                       topheight_adjust = TRUE
    ),
    NA)


})
test_that("topheight only", {


  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))


  run_map <- cfp_run_map(PROFLUX,
                     params = list("topheight" = c(-1)),
                     method = "permutation",
                     type = c("addition")
  )

  expect_no_error(
    run_map <- cfp_run_map(PROFLUX,
                       params = list("topheight" = c(-1,2)),
                       method = "permutation",
                       type = c("addition")
    ))
  expect_no_error(
    run_map <- cfp_run_map(PROFLUX,
                       params = list("topheight" = c(-1,1)),
                       method = "random",
                       type = c("addition"),
                       n_runs = 2
    ))

})

test_that("layers_different works", {


  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))

  run_map <- cfp_run_map(PROFLUX,
                         params = list("TPS" = c(0.9, 1.1)),
                         method = "random",
                         type = c("factor"),
                         layers_different = TRUE,
                         n_runs = 1)

  expect_equal(nrow(run_map), 4)
  expect_equal(cfp_params_df(run_map),
               data.frame(pmap = c(1, 2),
                          param = c("TPS", "TPS"),
                          param_id = c(1, 2)))

})


test_that("layers_different does not work for topheight only", {

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))

  expect_error(cfp_run_map(PROFLUX,
                           params = list("topheight" = c(0.9, 1.1)),
                           method = "random",
                           type = c("factor"),
                           layers_different = TRUE,
                           n_runs = 1))

  expect_error(cfp_run_map(PROFLUX,
                           params = list("topheight" = c(0.9, 1.1)),
                           method = "permutation",
                           type = c("factor"),
                           layers_different = TRUE,
                           n_runs = 1))


})

