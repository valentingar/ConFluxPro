test_that("combining cfp_dat works", {
  mod1 <- filter(base_dat, site == "site_a")
  mod2 <- filter(base_dat, site == "site_b")

  y <- combine_models(list(mod1, mod2))

  expect_equal(n_profiles(base_dat), n_profiles(y))
  expect_equal(cfp_id_cols(y),
               c(cfp_id_cols(base_dat), "cmb_id"))


})
