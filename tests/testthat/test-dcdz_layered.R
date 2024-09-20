test_that("dcdz_layered works", {

  df <- data.frame(depth = c(10,0,-100),
                   x_ppm = c(400,800,5000),
                   gd_id = 1)
  lmap <- data.frame(upper = c(10,0),
                     lower = c(0,-100),
                     layer = c("HU","MIN"))
  LL <-
  dcdz_layered(df,
               lmap,
               mode = "LL")

  LS <-
    dcdz_layered(df,
                 lmap,
                 mode = "LS")

  EF <-
    dcdz_layered(df %>%
                   rbind(data.frame(depth = c(-50),
                                    x_ppm = c(3000),
                                    gd_id = 1)),
                 lmap,
                 mode = "EF")

  expect_equal(nrow(EF),2)
  expect_equal(nrow(LS),2)
  expect_equal(nrow(LL),2)
  expect_equal(LL$dcdz_ppm,c(-4000,-4200))
  expect_equal(LL$dcdz_ppm,c(-4000,-4200))
  expect_equal(round(EF$dcdz_ppm,2),c(-4199.15,-4024.07))
})
