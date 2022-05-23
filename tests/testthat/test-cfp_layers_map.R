
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
