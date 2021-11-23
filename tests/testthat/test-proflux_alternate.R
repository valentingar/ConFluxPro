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
                    lmap %>%
                      select(upper,lower,site),
                    no_confirm = TRUE,
                    DSD0_formula = "a*AFPS^b",
                    error_funs = error_funs,
                    error_args = error_args)

  expect_equal(nrow(PF2),8)
  expect_equal(any(is.na(PF2$NRMSE)),FALSE)
  expect_equal(round(PF2$NRMSE,digits = 2),
               c(1.42,1.42,1.46,1.46,rep(0,4)))

})
