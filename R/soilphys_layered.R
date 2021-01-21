#' @title soilphys_layered
#'
#' @description This function calculates (harmonic-) means of the soilphys dataframe per layer
#' for the flux calculation.
#'
#' @param soilphys (dataframe) the soilphys dataframe
#' @param layers_map (dataframe) containing the following parameters: \n
#' "Plot"\n
#' "layer"=name of the layer\n
#' "upper"=upper limit of layer in cm \n
#' "lower" = lower limit of the layer in cm \n
#'  Must be the same
#'  dataframe as the input of the same name in dcdz_layered().
#' @param param (character vector) A vector containing the names of the variables in soilphys to be carried over
#' @param funs (character vector) A vector defining the type of mean to be used. One of "arith" or "harm"
#'
#' @return
#'
#' @examples
#' @family soilphys
#'
#' @import dplyr
#' @export

soilphys_layered<-function(soilphys,
                           layers_map,
                           param=c("TPS","SWC","AFPS","T","p","DSD0","D0","DS"),
                           funs=c("arith","arith","arith","arith","arith","harm","harm","harm")){

  set_layer <-function(Plot,depth){
    unlist(lapply(1:length(Plot),function(i){
    l<-layers_map$layer[layers_map$Plot == Plot[i] & layers_map$upper > depth[i] & layers_map$lower < depth[i]]
    return(l)}))
  }

  param_arith <- param[funs == "arith"]
  param_harm <- param[funs == "harm"]

  if(any(param_harm %in% param_arith)){
    warning("Some parameters appear to be calculated with multiple methods!")
  }

  soilphys<-soilphys %>%
    dplyr::mutate(layer = set_layer(Plot,depth) ) %>%
    dplyr::group_by(Plot,Date,gas,layer) %>%
    dplyr::mutate(height = abs(upper-lower))

  soilphys_arith <- soilphys %>%
    dplyr::summarise(across(.cols = contains( !!param_arith),.fns=~weighted.mean(.x,w=height,na.rm=T)))

  soilphys_harm <-soilphys %>%
    dplyr::summarise(across(.cols = contains( !!param_harm),.fns=~harm(.x,w=height,na.rm=T)))

  soilphys <- dplyr::left_join(soilphys_harm,soilphys_arith,by = c("Plot","Date","layer","gas"),suffix = c("_harm","_arith"))
  soilphys <- dplyr::left_join(soilphys,layers_map)
  return(soilphys)
}
