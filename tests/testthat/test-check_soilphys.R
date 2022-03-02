test_that("check_soilphys_half_ready", {

  df <- data.frame(upper = numeric(),
                   lower = numeric(),
                   SWC = numeric(),
                   TPS = numeric(),
                   t = numeric(),
                   p = numeric()
                   )

expect_equal(check_soilphys(df,id_cols = c())$result,T)
})

test_that("check_soilphys_not_ready", {

  df <- data.frame(upper = numeric(),
                   lower = numeric(),
                   SWC = numeric(),
                   TPS = numeric(),
                   t = numeric()
  )

  expect_equal(check_soilphys(df,id_cols = c())$result,F)
})

test_that("check_soilphys_supects", {

  df <- data.frame(upper = c(10,0),
                   lower = c(0,-10),
                   depth = c(5,-5),
                   SWC = c(NA,1),
                   TPS = c(NA,1),
                   t = c(NA,1)
  )
  expect_equal(check_soilphys(df,id_cols = c())$suspects,tibble(upper = c(10,10,10),
                                                                param = c("TPS","SWC","t"),
                                                                value = c(TRUE,TRUE,TRUE))
               )

  df <- data.frame(upper = c(NA,NA),
                   lower = c(0,-10),
                   depth = c(5,-5),
                   SWC = c(NA,1),
                   TPS = c(NA,1),
                   t = c(NA,1)
  )
  expect_equal(check_soilphys(df,id_cols = c())$suspects,tibble(param = c("upper"),
                                                                value = c(TRUE))
  )

})





