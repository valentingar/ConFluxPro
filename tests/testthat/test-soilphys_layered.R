test_that("soilphys_layered works", {
  df <-
    data.frame(upper = c(10,0,-30),
               lower = c(0,-30,-100),
               depth = c(5,-15,-65),
               DS = c(5E-5,5E-5,0E-5),
               rho_air = c(40,40,40),
               gas = "CO2")

  layers_map <- data.frame(
    upper = c(10,0),
    lower = c(0,-100),
    layer = c("HU","MIN"))

  df_res <-
  soilphys_layered(df,
                   layers_map,
                   param = c("DS","rho_air"),
                   funs = c("harm","arith"),
                   id_cols = "gas")

  expect_equal(df_res$DS,c(5E-5,0))


})
