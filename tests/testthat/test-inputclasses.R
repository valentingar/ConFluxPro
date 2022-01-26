test_that("create cfp_gasdata object", {


  df <- data.frame(site = rep(c("site_a","site_b"),
                              each = 4),
                   depth = rep(c(10,0,-20,-100),
                               times = 2),
                   NRESULT_ppm = rep(400,8),
                   gas = "CO2")



 x <- cfp_gasdata(df,
                  id_cols = "site")

 expect_message(cfp_gasdata(df,id_cols = "site"))
 expect_true(inherits(x, "cfp_gasdata"))
 expect_equal(cfp_id_cols(x),c("site","gas"))

 expect_error(cfp_gasdata(df))

})

test_that("create cfp_soilphys object",{

  df <- data.frame(site = rep(c("site_a","site_b"),
                              each = 3),
                   upper = rep(c(10,0,-20),
                               times = 2),
                   lower = rep(c(0,-20,-100),
                               times = 2),
                   rho_air = 1,
                   DS = 1,
                   gas = "CO2")

  x <- cfp_soilphys(df,
                    id_cols = "site")

  expect_message(cfp_soilphys(df,
                              id_cols = "site"))
  expect_error(cfp_soilphys(df))
  expect_true(inherits(x, "cfp_soilphys"))
  expect_equal(cfp_id_cols(x),c("site","gas"))

  # should result in error because of upper / lower consistency
  expect_error(cfp_soilphys(df,id_cols = c()))




})

test_that("create cfp_layers_map object",{

  df <- data.frame(site = rep(c("site_a","site_b"),
                              each = 2),
                   upper = rep(c(10,0),
                               times = 2),
                   lower = rep(c(0,-100),
                               times = 2)
                   )

  x <- cfp_layers_map(df,
                    id_cols = "site",
                    gas = "CO2")

  expect_message(cfp_layers_map(df,
                                id_cols = "site",
                                gas = "CO2"))
  expect_error(cfp_layers_map(df,
                              id_cols = "site"))

  expect_true(inherits(x, "cfp_layers_map"))
  expect_equal(cfp_id_cols(x),c("site","gas"))

  # should result in error because of upper / lower consistency
  expect_error(cfp_layers_map(df,id_cols = c()))

})



test_that("create cfp_layers_map object",{

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
  data.frame(plot = rep(c("site_a","site_b"),
                        each = 3),
             upper = rep(c(10,0,-20),
                         times = 2),
             lower = rep(c(0,-20,-100),
                         times = 2),
             rho_air = 1,
             DS = 1,
             gas = "CO2") %>%
    cfp_soilphys(id_cols = "plot")

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               NRESULT_ppm = rep(400,10),
               gas = "CO2") %>%
    cfp_gasdata(id_cols = "site")

  cfp_input <- cfp_dat(gasdata,
                       soilphys,
                       lmap)


})
