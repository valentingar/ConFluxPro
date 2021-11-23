test_that("model compare works", {


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
              layer = c(1,2))

  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  MC <- PROFLUX %>%
    error_compare_models(proflux_summarise(.),"site")


  expect_equal(MC$NRMSE,rep(0,10))

})
