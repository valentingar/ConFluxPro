test_that("subsetting works", {
  df <- data.frame(site = c("a", "a", "b", "b"),
                   blub = c(1,2,3,4),
                   blob = c(5,6,7,8))

  df_prof <- cfp_profile(df, id_cols = "site")
  df2 <- cfp_profile(df[df$site == "a",],
                     id_cols = "site")
  df3 <- cfp_profile(df[, c(1,3)],
                     id_cols = "site")
  df4 <- cfp_profile(df[, c(2,3)])

  expect_equal(df_prof[df_prof$site == "a",],
               df2)
  expect_equal(df_prof[, c(1,3)],
               df3)
  expect_equal(df_prof[,c(2,3)],
               df4)


})
