#' Helper functions for test suite.

testhelp_make_proflux <- function(){

  soilphys <-
    cfp_soilphys(ConFluxPro::soilphys,
                 id_cols = c("site", "Date"))

  gasdata <-
    cfp_gasdata(ConFluxPro::gasdata,
                id_cols = c("site", "Date"))


  lmap <-
    cfp_layers_map(ConFluxPro::layers_map,
                   gas = "CO2",
                   lowlim = 0,
                   highlim = 1000,
                   id_cols = "site")

  cfp_dat(gasdata,
            soilphys,
            lmap) |>
    pro_flux()


}
