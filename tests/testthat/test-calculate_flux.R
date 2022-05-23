test_that("calculate flux works", {
  df <- calculate_flux(data.frame(depth = c(10,0,-100),
                                  x_ppm = c(400,700,5000),
                                  gas = "CO2"),
                       data.frame(upper = c(10,0,-30),
                                  lower = c(0,-30,-100),
                                  depth = c(5,-15,-65),
                                  DS = c(5E-5,5E-5,5E-5),
                                  c_air = c(40,40,40),
                                  gas = "CO2"),
                       layers_map = data.frame(
                         upper = c(10,0),
                                               lower = c(0,-100),
                                               layer = c("HU","MIN")),
                       param = c("DS","c_air"),
                       funs = c("harm","arith"),
                       gases = "CO2",
                       modes = c("LS","LL"))

  expect_equal(df$flux,c(6,8.6,6,8.6))

})
