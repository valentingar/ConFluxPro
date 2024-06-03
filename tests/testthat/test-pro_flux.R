test_that("base_proflux fixture is correct", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))
  base_proflux <- readRDS(testthat::test_path("fixtures", "base_proflux.rds"))

  expect_equal(pro_flux(base_dat), base_proflux)

})


test_that("pro_flux works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  PROFLUX <-
    pro_flux(base_dat)

  expect_equal(nrow(PROFLUX$PROFLUX),216)
  expect_equal(round(PROFLUX$PROFLUX$flux[1],digits = 3),0.039)
})


test_that("pro_flux zero_flux works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  PROFLUX <-
    pro_flux(base_dat,
             zero_flux = FALSE,
             zero_limits = c(0, 1000))


  expect_equal(nrow(PROFLUX$PROFLUX),216)
})


test_that("pro_flux does not allow negative concentrations", {

  soilphys <-
  data.frame(
    gas = "CO2",
    date = 1,
    upper =c(10,0),
    lower = c(0,-10),
    DS = c(2E-4,
           2E-5),
    c_air = 44) |>
    cfp_soilphys(id_cols = "date")

  gasdata <-
    data.frame(depth = c(10,0,-5,-10),
               x_ppm = c(400,100,2000,4000),
               date = 1,
               gas = "CO2") |>
    cfp_gasdata(id_cols = "date")


  lmap <- data.frame(date = 1,
                     upper = c(10,0),
                     lower = c(0,-10)) |>
    cfp_layers_map(gas = "CO2",
                   layer_couple = 0,
                   lowlim = 0,
                   highlim = 1000,
                   id_cols = "date")

  my_dat <-
    cfp_dat(gasdata,
            soilphys,
            lmap)

  my_dat$gasdata$x_ppm[1] <- -my_dat$gasdata$x_ppm[1]

  PROFLUX <- pro_flux(my_dat)

  PROFLUX$PROFLUX

  expect_true(all(PROFLUX$PROFLUX$conc >=0 | is.na(PROFLUX$PROFLUX$conc)))

  })
