test_that("correct interpolation",{
  df1 <- data.frame(depth = c(10,0,-50),
                    value1 = c(1,5,30),
                    site = "A")

  df2 <- data.frame(upper = c(9,0,-10),
                    lower = c(0,-10,-30),
                    value2 = c(0.9,0.7,0.5),
                    site = "A")

  lmap <- data.frame(depth = c(5,0,-5,-10,-20,-30),
                     site = "A")

  df_res <-data.frame(site = "A",
                      upper = c(5,0,-5,-10,-20),
                      lower = c(0,-5,-10,-20,-30),
                      depth = c(2.5,-2.5,-7.5,-15,-25),
                      value2 = c(0.9,0.7,0.7,0.5,0.5),
                      value1 = c(4,6.25,8.75,12.5,17.5))
  df_res <-df_res[order(df_res$upper),]
  row.names(df_res) <- (1:5)

  expect_equal(
  discretize_depth(df1,
                   param = "value1",
                   depth_target = lmap,
                   method = "linear",
                   id_cols = "site"),
  df_res[,c(1,6,4,2,3)]

  )
  expect_equal(
    discretize_depth(df2,
                     param = "value2",
                     depth_target = lmap,
                     method = "boundary",
                     id_cols = "site"),
    df_res[,c(1,5,4,2,3)]


  )
})

test_that("can interpolate multiple profiles",{
  df1 <- data.frame(depth = c(10,0,-50,
                              10,0,-30),
                    value1 = c(1,5,30,
                               1,60,120),
                    site = c("A","A","A",
                             "B","B","B")
  )

  df2 <- data.frame(upper = c(9,0,-10,
                              5,0,-20),
                    lower = c(0,-10,-30,
                              0,-20,-50),
                    value2 = c(0.9,0.7,0.5,
                               10,12,11),
                    site = c("A","A","A",
                             "B","B","B")
  )

  lmap <- data.frame(depth = c(5,0,-5,-10,-20,-30,
                               2,0,-6,-8,-20,-24),
                     site = c("A","A","A","A","A","A",
                              "B","B","B","B","B","B"))

  df_res <-data.frame(site = c("A","A","A","A","A",
                               "B","B","B","B","B"),
                      upper = c(5,0,-5,-10,-20,
                                2,0,-6,-8,-20),
                      lower = c(0,-5,-10,-20,-30,
                                0,-6,-8,-20,-24),
                      depth = c(2.5,-2.5,-7.5,-15,-25,
                                1,-3,-7,-14,-22),
                      value2 = c(0.9,0.7,0.7,0.5,0.5,
                                 10,12,12,12,11),
                      value1 = c(4,6.25,8.75,12.5,17.5,
                                 54.1,66,74,88,104)
                      )

  df_res <-df_res[order(df_res$site,df_res$upper),]
  row.names(df_res) <- (1:10)



  expect_equal(
    discretize_depth(df1,
                     param = "value1",
                     depth_target = lmap,
                     method = "linear",
                     id_cols = "site"),
    df_res[,c(1,6,4,2,3)]

  )
  expect_equal(
    discretize_depth(df2,
                     param = "value2",
                     depth_target = lmap,
                     method = "boundary",
                     id_cols = "site"),
    df_res[,c(1,5,4,2,3)]
  )

})

test_that("boundary creates correct NAs",{
          df <- data.frame(upper = c(9,0,-10),
                           lower = c(0,-10,-30),
                           value2 = c(0.9,0.7,0.5),
                           site = "A")

          lmap <- data.frame(depth = c(12,10,0,-5,-8,-20,-30,-100),
                             site = "A")

          expect_equal(discretize_depth(df,
                                        param = "value2",
                                        depth_target = lmap,
                                        method = "boundary",
                                        id_cols = "site")$value2,
                       c(NA,0.5,NA,0.7,0.7,NA,NA)
                       )


          })


test_that("depth_target can be a vector",{
  df <- discretize_depth(
    data.frame(upper = c(10,0),
               lower = c(0,-100),
               value = c("A","B"),
               site = "A") ,
    depth_target = c(10,0,-10,-50),
    method = "boundary",
    param = "value",
    id_cols = "site")

  expect_equal(nrow(df),3)
})


test_that("id_cols can be left blank",{
  df <- discretize_depth(
    data.frame(upper = c(10,0),
               lower = c(0,-100),
               value = c("A","B")) ,
    depth_target = c(10,0,-10,-50),
    method = "boundary",
    param = "value")

  expect_equal(nrow(df),3)
})


test_that("method boundary_average",{
  df <- data.frame(upper = c(10,0),
                   lower = c(0,-10),
                   value = c(1,2))
  dt <- c(10,5,-5,-10)

  df_test <-
  discretize_depth(df,
                   "value",
                   "boundary",
                   dt,
                   boundary_nearest = T,
                   boundary_average = "arith")

  df_res <-
    data.frame(value = c(2,1.5,1),
               depth = c(-7.5,0,7.5),
               upper = c(-5,5,10),
               lower = c(-10,-5,5)
    )
  expect_equal(df_test,df_res)
})


test_that("method nearest",{
  df <- data.frame(depth = c(-10,-8,0,10),
                   value = c(1,4,1,2))
  dt <- c(10,7,-5,-10)

  df_test <-
    discretize_depth(df,
                     "value",
                     "nearest",
                     dt,
                     int_depth = 0.5)

  df_test2 <-
    discretize_depth(df,
                     "value",
                     "nearest",
                     dt,
                     int_depth = 0)

  df_test3 <-
    discretize_depth(df,
                     "value",
                     "nearest",
                     dt,
                     int_depth = 1)

  df_res <-
    data.frame(value = c(4,1,2),
               depth = c(-7.5,1,8.5),
               upper = c(-5,7,10),
               lower = c(-10,-5,7)
    )
  expect_equal(df_test,df_res)
  expect_equal(df_test2$value,c(1,4,2))
  expect_equal(df_test3$value,c(4,2,2))
})

test_that("method harmonic",{
  df <- data.frame(depth = c(-10,0,10),
                   value = c(1,1,2))
  dt <- c(10,5,-5,-10)

  df_test <-
    discretize_depth(df,
                     "value",
                     "harmonic",
                     dt,
                     int_depth = 0.5)

  df_res <-
    data.frame(value = c(1,1,1.6),
               depth = c(-7.5,0,7.5),
               upper = c(-5,5,10),
               lower = c(-10,-5,5)
    )
  expect_equal(df_test,df_res)
})


test_that("method linear spline",{
  df <- data.frame(depth = c(-10,0,10),
                   value = c(1,1,2))
  dt <- c(10,5,-5,-10)

  df_test <-
    discretize_depth(df,
                     "value",
                     "linspline",
                     dt)

  df_res <-
    data.frame(value = c(0.958,1.333,1.708),
               depth = c(-7.5,0,7.5),
               upper = c(-5,5,10),
               lower = c(-10,-5,5)
    )
  expect_equal(round(df_test,3),df_res)
})


