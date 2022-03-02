test_that("create cfp_dat object",{

  lmap <- data.frame(site = rep(c("site_a","site_b"),
                              each = 2),
                   upper = rep(c(10,0),
                               times = 2),
                   lower = rep(c(0,-100),
                               times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                      gas = "CO2")

  lmap2 <- data.frame(site = rep(c("site_a","site_b"),
                                each = 2),
                     upper = rep(c(11,0),
                                 times = 2),
                     lower = rep(c(0,-100),
                                 times = 2)
  ) %>%
    cfp_layers_map(id_cols = "site",
                   gas = "CO2")

  soilphys <-
  data.frame(site = rep(c("site_a","site_b"),
                        each = 3),
             upper = rep(c(10,0,-20),
                         times = 2),
             lower = rep(c(0,-20,-100),
                         times = 2),
             c_air = 1,
             DS = 1,
             gas = "CO2") %>%
    cfp_soilphys(id_cols = "site")

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               x_ppm = rep(400,10),
               gas = "CO2") %>%
    cfp_gasdata(id_cols = "site")

  cfp_input <- cfp_dat(gasdata,
                       soilphys,
                       lmap)

  expect_equal(names(cfp_input), c("profiles", "gasdata", "soilphys", "layers_map"))
  expect_equal(nrow(cfp_input$soilphys),8)
  expect_error(cfp_dat(gasdata,
                       soilphys,
                       lmap2))


})
