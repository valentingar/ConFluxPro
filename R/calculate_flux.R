#' @title calculate_flux
#'
#' @description This function takes the soilphys-dataframe as well as the gasdata-dataframe
#' and calculates fluxes accordingly. Fluxes are calculated for each layer defined in
#' layers map and are given in mumol/m^2/s.
#'
#'
#' @param gasdata (dataframe) The gasdata dataframe
#' @param soilphys (dataframe) The soilphys dataframe after discretisation and completion.
#' @param layers_map (dataframe) containing the following parameters: "layer"=name of the layer;
#' "upper"=upper limit of layer in cm; "lower" = lower limit of the layer in cm;
#' @param modes (character) A character vector specifying mode(s) for dcdz calculation. Can be "LL","LS","EF".
#' @param param (character) A vector containing the the parameters of soilphys, for which means should be calculated,
#' must contain "rho_air" and "DS", more parameters help interpretation
#' @param funs (character) A vector defining the type of mean to be used. One of "arith" or "harm"
#'
#'
#'
#' @return FLUX
#' @examples
#' calculate_flux(gasdata,
#'                soilphys_complete,
#'                layers_map = layers_map ,
#'                gases = c("CO2","CH4","O2"),
#'                modes =c("LL","LS","EF"),
#'                param = c("DSD0","DS","SWC","Temp","p"),
#'                funs = c("harm","harm","arith","arith","arith"))
#'
#' @import dplyr
#'
#' @seealso soilphys_layered
#' @seealso dcdz_layered
#'
#' @export
calculate_flux <- function(gasdata,
                           soilphys,
                           layers_map,
                           gases,
                           modes,
                           param,
                           funs){
if(!"DS" %in% param){
  stop("cannot calculate flux: 'DS' is missing in param!")
}

g_plots <- unique(gasdata$Plot)
s_plots <- unique(soilphys$Plot)
if ("Plot" %in% names(layers_map)){
l_plots <- unique(layers_map$Plot)

if(!all(g_plots %in% l_plots)){
  no_plots <- g_plots[!g_plots %in% l_plots]
  #print(no_plots)
  warning(paste("The following Plots are not represented in layers_map, skipping: "),paste(no_plots,collapse = " ,"))
  gasdata <- gasdata %>% dplyr::filter(!Plot %in% no_plots)
}



}
else{
  layers_map <- lapply(g_plots,function(Plot){
    df <- layers_map %>% dplyr::mutate(Plot == !!Plot)
  }) %>% bind_rows()

}

#subset gasdata to relevant gases
gasdata <- gasdata %>% dplyr::filter(gas %in% gases)

#for progress tracking
n_gradients <- length(with(gasdata,unique(paste(Plot,Date,gas))))
n_soilphys <- length(with(soilphys,unique(paste(Plot,Date,gas))))

print("starting gradient")
FLUX <- gasdata %>% dplyr::group_by(Plot,Date,gas) %>%
  dplyr::mutate(n_gr = dplyr::cur_group_id(),n_tot=n_gradients) %>%
    dplyr::group_modify(~{
      if (.x$n_gr[1] %in% floor(seq(1,n_gradients,length.out = 11))){
        print(paste0(round(.x$n_gr[1] /n_gradients*100)," %"))
      }
      FLUX <- lapply(modes, function(mode){
        #print(.y$Plot)
        #print(mode)
        #print(.y$Date)
        #print(.y$gas)
      dcdz_layered(.x,layers_map[layers_map$Plot == .y$Plot[1],],mode)}) %>%
        dplyr::bind_rows()
      return(FLUX)
    })

print("gradient complete")
print("starting soilphys")
relevant_subset <- with(gasdata, paste(Plot,gas,Date))
soilphys_layers <-soilphys_layered(soilphys %>% dplyr::filter(paste(Plot,gas,Date) %in% relevant_subset),
                            layers_map,
                            param,
                            funs)
print("soilphys complete")
FLUX <- FLUX %>%
  dplyr::left_join(soilphys_layers) %>%
  dplyr::mutate(rho_air = p*100 / (8.314 * (273.15+Temp))) %>%
  dplyr::mutate(flux = -DS*rho_air*dcdz_ppm) %>%
  dplyr::mutate(depth = upper-lower)
print("flux calculation complete")
return(FLUX)
}
