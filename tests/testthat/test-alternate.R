test_that("topheight only", {

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))


  run_map <- cfp_run_map(PROFLUX,
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

  PROFLUX <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))


  run_map <- cfp_run_map(PROFLUX,
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
