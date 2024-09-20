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

test_that("cfp_gasdatasoilphys no negatives allowed", {
  library(dplyr)

  gasdata <- ConFluxPro::gasdata %>%
    mutate(x_ppm = -x_ppm)

  expect_error(gasdata %>%
                 cfp_gasdata(id_cols = c("site", "Date"))
  )
})
