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

test_that("pro_flux correct mapping",{

  gasdata <- data.frame(depth = c(5,0,-10,-20,-30,-100),
                        NRESULT_ppm = NA,
                        site = "A")
  soilphys <- data.frame(upper = c(-93,-83,-73,-63,-53,-48,
                                   -43,-38,-33,-28,-23,-18,
                                   -13,-8,-4,0,7),
                         lower =  c(-100,-93,-83,-73,-63,-53,-48,
                                    -43,-38,-33,-28,-23,-18,
                                    -13,-8,-4,0),
                         DS = 100,
                         rho_air = 1,
                         site = "A"
  ) %>%
    dplyr::mutate(depth = (upper+lower)/2)
  lmap <- data.frame(upper = c(-30,-8,0,7),
                     lower = c(-100,-30,-8,0),
                     layer = c("M3","M2","M1","HU"),
                     highlim = 1000,
                     lowlim = 0,
                     layer_couple = 0,
                     site = "A")
  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site"))
  expect_equal(PROFLUX$pmap,c(rep(1,10),rep(2,7),3,3,4,4))
})
