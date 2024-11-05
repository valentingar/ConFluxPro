test_that("sobol run_map works", {

  skip_on_cran()

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds")) |>
    filter(prof_id %in% c(7,8))


  set.seed(42)
  run_map <- cfp_run_map(PROFLUX,
                         params = list("TPS" = c(1,1.2),
                                       "t" = c(1,1.05)),
                         method = "random",
                         type = c("factor", "factor"),
                         layers_different = TRUE,
                         n_runs = 4
  )

  srm <- sobol_run_map(run_map)

  EFFLUX <-
    alternate(PROFLUX,
              function(x) {
                complete_soilphys(x,
                                  DSD0_formula = "a*AFPS^b", quiet = TRUE)
              },
              run_map = srm) |>
    cfp_altapply(efflux)

  si <-
    sobol_calc_indices(EFFLUX,
                       "efflux",
                       id_cols = "site",
                       run_map = srm)

  expect_equal(round(si$Si, 3), c(0.036,
                                  0.000,
                                  0.966,
                                  0.000,
                                  0.004,
                                  0.000,
                                  0.997,
                                  0.000))


})
