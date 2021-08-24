test_that("dcdz_layered works", {

  df <- data.frame(depth = c(10,0,-100),
                   NRESULT_ppm = c(400,800,5000))
  lmap <- data.frame(upper = c(10,0),
                     lower = c(0,-100),
                     layer = c("HU","MIN"))
  LL <-
  dcdz_layered(df,
               lmap,
               mode = "LL",
               depth_steps = c(0) #interface depths
  )

  LS <-
    dcdz_layered(df,
                 lmap,
                 mode = "LS",
                 depth_steps = c(0) #interface depths
    )

  EF <-
    dcdz_layered(df,
                 lmap,
                 mode = "EF",
                 depth_steps = c(0) #interface depths
    )

  expect_equal(nrow(EF),2)
  expect_equal(nrow(LS),2)
  expect_equal(nrow(LL),2)
  expect_equal(LL$dcdz_ppm,c(-4000,-4200))
  expect_equal(LL$dcdz_ppm,c(-4000,-4200))
})
