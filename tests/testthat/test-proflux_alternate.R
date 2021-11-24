test_that("proflux_alternate works", {


  data("gasdata")
  data("soilphys")

  lmap <- soilphys %>%
    select(upper,site) %>%
    distinct() %>%
    group_by(site) %>%
    slice_max(upper) %>%
    summarise(upper = c(upper,0),
              lower = c(0,-100),
              lowlim = 0,
              highlim = 1000,
              layer_couple = 0,
              layer = c(1,1))

  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  EFFLUX <-
    PROFLUX %>%
    pf_efflux()

  error_funs <-
    list(gd = error_gasdata,
         ef = error_efflux)

  error_args <-
    list(gd = list(param_cols = c("site"),
                   normer = "sd"),
         ef = list(EFFLUX = EFFLUX,
                   param_cols = "site",
                   normer = "sd")
    )


  PF2 <-
  proflux_alternate(PROFLUX,
                    "TPS",
                    1.0,
                    params_map = lmap %>%
                      select(upper,lower,site),
                    no_confirm = TRUE,
                    DSD0_formula = "a*AFPS^b",
                    error_funs = error_funs,
                    error_args = error_args)

  error_funs2 <-
    list(MC = error_compare_models)

  error_args2 <-
    list(MC = list(PF_summary = proflux_summarise(PROFLUX),
                   param_cols = "site"))

  PF3 <-
    proflux_alternate(PROFLUX,
                      c("TPS"),
                      facs = c(1.5,2),
                      params_map = lmap %>%
                        select(upper,lower,site),
                      sensitivity_analysis = "OAT",
                      no_confirm = TRUE,
                      DSD0_formula = "a*AFPS^b",
                      error_funs = error_funs2,
                      error_args = error_args2)

  PF4 <-
    proflux_alternate(PROFLUX,
                      c("TPS"),
                      facs = c(1.5),
                      params_map = lmap %>%
                        select(upper,lower,site),
                      sensitivity_analysis = "OATL",
                      no_confirm = TRUE,
                      DSD0_formula = "a*AFPS^b",
                      error_funs = error_funs2,
                      error_args = error_args2)


  expect_equal(nrow(PF2$results),4)
  expect_equal(any(is.na(PF2$results$NRMSE)),FALSE)
  expect_equal(round(PF2$results$NRMSE,digits = 2),
               c(0.21,0.24,0,0))

  expect_equal(nrow(PF3$results),18)
  expect_equal(unique(PF3$results$run_id),c(1,2,3))

  expect_equal(unique(PF4$run_map$run_id),c(1,2,3))

})
