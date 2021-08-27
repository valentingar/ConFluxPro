test_that("pro_flux works", {


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
              layer_couple = 0)
  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))


  expect_equal(nrow(PROFLUX),216)
  expect_equal(round(PROFLUX$flux[1],digits = 3),0.038)
})
