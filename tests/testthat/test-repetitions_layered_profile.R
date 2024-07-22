test_that("check_matching repetitions works", {
  df <- data.frame(site = "site_a",
                   rep_id = c(1,1,2,2),
                   upper = c(5,0,5,0),
                   lower = c(0,-5,0,-5)
                   ) %>%
    cfp_layered_profile(id_cols = c("site", "rep_id"))

  df2 <- data.frame(site = "site_a",
                   rep_id = c(1,1,2,2),
                   upper = c(5,0,2,0),
                   lower = c(0,-5,0,-5)
  ) %>%
    cfp_layered_profile(id_cols = c("site", "rep_id"))


  df3 <- data.frame(site = "site_a",
                    rep_id = c(1,1,2),
                    upper = c(5,0,5),
                    lower = c(0,-5,0)
  ) %>%
    cfp_layered_profile(id_cols = c("site", "rep_id"))

  expect_true(check_matching_repetitions(df, "rep_id"))
  expect_false(check_matching_repetitions(df, "blub"))
  expect_false(check_matching_repetitions(df2, "rep_id"))
  expect_false(check_matching_repetitions(df3, "rep_id"))

})
