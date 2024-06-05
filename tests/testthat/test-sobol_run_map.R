test_that("sobol run_map works", {

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds")) |>
    filter(prof_id %in% c(7,8))


  run_map <- cfp_run_map(PROFLUX,
                         params = list("TPS" = c(1,1.2),
                                       "t" = c(1,1.05)),
                         method = "random",
                         type = c("factor", "factor"),
                         layers_different = TRUE,
                         n_runs = 4
  )

  srm <- sobol_run_map(run_map)

  expect_equal(nrow(srm), 160)


})
