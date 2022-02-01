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

  soilphys <-
  data.frame(plot = rep(c("site_a","site_b"),
                        each = 3),
             upper = rep(c(10,0,-20),
                         times = 2),
             lower = rep(c(0,-20,-100),
                         times = 2),
             rho_air = 1,
             DS = 1,
             gas = "CO2") %>%
    cfp_soilphys(id_cols = "plot")

  gasdata <-
    data.frame(site = rep(c("site_a","site_b"),
                          each = 5),
               depth = rep(c(10,0,-20,-30,-100),
                           times = 2),
               NRESULT_ppm = rep(400,10),
               gas = "CO2") %>%
    cfp_gasdata(id_cols = "site")

  cfp_input <- cfp_dat(gasdata,
                       soilphys,
                       lmap)


})
