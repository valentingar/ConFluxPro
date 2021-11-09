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

test_that("pro_flux missing in soilphys",{


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

  gasdata$NRESULT_ppm[141:143] <- NA
  soilphys$DS[1] <- NA
  soilphys <- soilphys[-130,]

  PROFLUX <-
    pro_flux(gasdata,
             soilphys,
             lmap,
             c("site","Date"))

  expect_equal(nrow(PROFLUX),207)
  expect_equal(round(PROFLUX$flux[100],digits = 3),0.047)
  expect_true(is.na(PROFLUX$flux[1]))
  expect_true(is.na(PROFLUX$flux[41]))
})


test_that("DSD0 optim works", {



  data("gasdata")
  data("soilphys")

  dates <- as.Date(c("2021-01-01",
           "2021-02-01"))

  gasdata <-
  gasdata %>%
    filter(Date %in% dates)

  soilphys <-
    soilphys %>%
    filter(Date %in% dates)

  known_flux <- data.frame(
    site = rep(c("site_a","site_b"),each = 2),
    Date = rep(dates,times = 2),
    flux = c(0.76,1.39,0.57,0.89)
  )

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
              known_flux = known_flux,
              known_flux_factor = 5000,
              DSD0_optim = F,
              c("site","Date"))


   EFFLUX <- PROFLUX %>% pf_efflux()

  expect_equal(round(EFFLUX$efflux,digits = 2),
               round(known_flux$flux,digits = 2))
})
