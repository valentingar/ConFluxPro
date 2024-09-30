test_that("base_dat fixture is correct", {


  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  expect_equal(testhelp_make_dat(), base_dat)


})

test_that("create cfp_dat object",{

  library(dplyr)

  lmap <- data.frame(site = rep(c("site_a","site_b"),
                              each = 2),
                   upper = rep(c(10,0),
                               times = 2),
                   lower = rep(c(0,-100),
                               times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                      gas = "CO2")

  lmap2 <- data.frame(site = rep(c("site_a","site_b"),
                                each = 2),
                     upper = rep(c(11,0),
                                 times = 2),
                     lower = rep(c(0,-100),
                                 times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                   gas = "CO2")

  soilphys <-
  data.frame(site = rep(c("site_a","site_b"),
                        each = 3),
             upper = rep(c(10,0,-20),
                         times = 2),
             lower = rep(c(0,-20,-100),
                         times = 2),
             c_air = 1,
             DS = 1,
             gas = "CO2") %>%
    cfp_soilphys(id_cols = "site")

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               x_ppm = rep(400,10),
               gas = "CO2") %>%
    cfp_gasdata(id_cols = "site")

  cfp_input <- cfp_dat(gasdata,
                       soilphys,
                       lmap)

  expect_equal(names(cfp_input), c("profiles", "gasdata", "soilphys", "layers_map"))
  expect_equal(nrow(cfp_input$soilphys),8)
  expect_error(cfp_dat(gasdata,
                       soilphys,
                       lmap2))


})

test_that("split soilphys",{

  library(dplyr)

  lmap <- data.frame(site = rep(c("site_a","site_b"),
                                each = 2),
                     upper = rep(c(10,0),
                                 times = 2),
                     lower = rep(c(0,-100),
                                 times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                   gas = "CO2")

  soilphys <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 3),
               upper = rep(c(10,0,-20),
                           times = 2),
               lower = rep(c(0,-20,-100),
                           times = 2),
               c_air = 1,
               DS = 1,
               gas = "CO2") %>%
    cfp_soilphys(id_cols = "site")

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               x_ppm = rep(400,10),
               gas = "CO2") %>%
    cfp_gasdata(id_cols = "site")

  cfp_input <- cfp_dat(gasdata,
                       soilphys,
                       lmap)

  gd_new <- cfp_input$gasdata %>%
    dplyr::add_row(site = rep(c("site_a","site_b"), each = 4),
                   depth = rep(c(-40,-45,-50,-55),times = 2),
                   gas = "CO2",
                   gd_id = rep(1:2,each = 4))

  sp_split <- split_soilphys(cfp_input$soilphys,
                             gd_new,
                             cfp_input$layers_map)

  expect_equal(sort(sp_split$upper[sp_split$site == "site_a"]),
               c(-55,-50,-45,-40,-30,-20,0,10))



})

test_that("missing combinations work",{

  library(dplyr)

  lmap <- data.frame(site = rep(c("site_a","site_b"),
                                each = 2),
                     upper = rep(c(10,0),
                                 times = 2),
                     lower = rep(c(0,-100),
                                 times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                   gas = "CO2")

  soilphys <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 3),
               upper = rep(c(10,0,-20),
                           times = 2),
               lower = rep(c(0,-20,-100),
                           times = 2),
               c_air = 1,
               DS = 1,
               gas = "CO2") %>%
    dplyr::cross_join(data.frame(date = c(1, 2, 3))) %>%
    cfp_soilphys(id_cols = c("site", "date"))

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               x_ppm = rep(400,10),
               gas = "CO2") %>%
    dplyr::cross_join(data.frame(date = c(1, 2, 3, 4))) %>%
    cfp_gasdata(id_cols = c("site", "date"))

  my_dat <-
    cfp_dat(gasdata,
            soilphys,
            lmap)

  expect_equal(n_profiles(my_dat), 6)

})



test_that("incomplete profiles are checked",{

  library(dplyr)

  lmap <- data.frame(site = rep(c("site_a","site_b"),
                                each = 2),
                     upper = rep(c(10,0),
                                 times = 2),
                     lower = rep(c(0,-100),
                                 times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                   gas = "CO2")

  soilphys <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 3),
               upper = rep(c(10,0,-20),
                           times = 2),
               lower = rep(c(0,-20,-100),
                           times = 2),
               c_air = 1,
               DS = 1,
               gas = "CO2") %>%
    dplyr::cross_join(data.frame(date = c(1, 2, 3))) %>%
    cfp_soilphys(id_cols = c("site", "date"))

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               x_ppm = rep(400,10),
               gas = "CO2") %>%
    dplyr::cross_join(data.frame(date = c(1, 2, 3))) %>%
    dplyr::slice(c(1:5, 7:10, 12:16, 18:27, 29:dplyr::n())) %>%
    cfp_gasdata(id_cols = c("site", "date"))

  my_dat <-
    cfp_dat(gasdata,
            soilphys,
            lmap)

  expect_equal(n_profiles(my_dat), 4)

})


test_that("filter cfp_dat objects works", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  base_dat_site_a <- filter(base_dat, site == "site_a")
  base_dat_prof1 <- filter(base_dat, prof_id == 1)

  expect_equal(unique(base_dat_site_a$profiles$site), "site_a")
  expect_equal(unique(base_dat_prof1$profiles$prof_id), 1)
  expect_equal(n_profiles(base_dat_site_a), 12)

  expect_equal(nrow(base_dat_prof1$soilphys), 9)

})



test_that("cfp_dat fails when no valid profiles", {

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  gd <- base_dat$gasdata

  base_dat$gasdata$x_ppm[base_dat$gasdata$depth > 0] <- NA

  expect_error(
  cfp_dat(base_dat$soilphys,
          base_dat$gasdata,
          base_dat$layers_map)
  )

})

test_that("cfp_dat fails when id cols of one dataset are present in another.",{

  base_dat <- readRDS(testthat::test_path("fixtures", "base_dat.rds"))

  soilphys <- cfp_soilphys(base_dat)
  gasdata <- cfp_gasdata(base_dat)
  layers_map <- cfp_layers_map(base_dat)

  attr(gasdata, "id_cols") <- c("gas", "Date")

  expect_error(
    cfp_dat(gasdata, soilphys, layers_map),
    regexp = "id_col of one dataset cannot be a non-id_col in another!"
    )
})
