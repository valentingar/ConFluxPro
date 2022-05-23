test_that("create cfp_gasdata object", {


  df <- data.frame(site = rep(c("site_a","site_b"),
                              each = 4),
                   depth = rep(c(10,0,-20,-100),
                               times = 2),
                   x_ppm = rep(400,8),
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
                   c_air = 1,
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
