#' @title calculate_flux
#'
#' @description This function takes the soilphys-Dataframe as well as the gasdata-dataframe and calculates fluxes accordingly.
#'
#'
#' @param gasdata (dataframe) The gasdata dataframe
#' @param soilphys (dataframe) The soilphys dataframe after discretisation and completion.
#' @param layers_map (dataframe) containing the following parameters: "layer"=name of the layer;
#' "upper"=upper limit of layer in cm; "lower" = lower limit of the layer in cm;
#' @param modes (character) A character vector specifying mode(s) for dcdz calculation. Can be "LL","LS","EF".
#' @param param (character) A vector containing the the parameters of soilphys, for which means should be calculated
#' @param funs (character) A vector defining the type of mean to be used. One of "arith" or "harm"
#'
#'
#'
#' @return FLUX
#' @examples
#'
#' @import dplyr
#'
#' @export
calculate_flux <- function(gasdata,
                           soilphys,
                           layers_map,
                           modes,
                           param,
                           funs){
if(!"DS" %in% param){
  stop("cannot calculate flux: 'DS' is missing in param!")
}

FLUX <- gasdata %>% dplyr::group_by(Plot,Date,gas) %>%
    dplyr::group_modify(~{
      FLUX <- lapply(modes, function(mode){
      dcdz_layered(.x,layers_map,mode)}) %>%
        dplyr::bind_rows()
      return(FLUX)
    })

soilphys_layers <-soilphys_layered(soilphys,
                            layers_map,
                            param,
                            funs)
FLUX <- FLUX %>%
  dplyr::left_join(soilphys_layers) %>%
  dplyr::mutate(flux = -DS*dcdz)
return(FLUX)
}
