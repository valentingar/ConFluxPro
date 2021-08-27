test_that("efflux_extrap works", {

  df <- data.frame(depth = rep(c(2.5,-10,-60),times = 2),
                   flux = c(6,4,1,5,3,0.5),
                   layer = rep(c("HU","M1","M2"),times = 2),
                   topheight = 5,
                   Date = c(1,2),
                   gas = "CO2")

  EF <-
  efflux_extrap(df,
                "CO2",
                method = "lm",
                modename = "LM",
                id_cols = "Date")

  expect_equal(round(EF$efflux,digits = 6), c(5.219048,5.130952))
})
