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
              layer_couple = 0)

  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  PF2 <-
  proflux_alternate(PROFLUX,
                    "TPS",
                    seq(0.9,1.1,0.1),
                    lmap %>% select(upper,lower,site),
                    no_confirm = TRUE,
                    DSD0_formula = "a*AFPS^b")

})
