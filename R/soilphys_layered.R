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
                           funs=c("arith","arith","arith","arith","arith","harm","harm","harm"),
                           id_cols){

  id_cols_s <- c(id_cols,"layer")

  param_arith <- param[funs == "arith"]
  param_harm <- param[funs == "harm"]

  if(any(param_harm %in% param_arith)){
    warning("Some parameters appear to be calculated with multiple methods!")
  }

  soilphys<-soilphys %>%
    set_layer(layers_map = layers_map,id_cols = id_cols)%>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({id_cols_s}))) %>%
    dplyr::mutate(height = abs(upper-lower))

  soilphys_arith <- soilphys %>%
    dplyr::summarise(dplyr::across(.cols = any_of( !!param_arith),.fns=~weighted.mean(.x,w=height,na.rm=T)))

  soilphys_harm <-soilphys %>%
    dplyr::summarise(dplyr::across(.cols = any_of( !!param_harm),.fns=~harm(.x,w=height,na.rm=T)))

  soilphys <- dplyr::left_join(soilphys_harm,soilphys_arith,by = id_cols_s,suffix = c("_harm","_arith"))
  soilphys <- dplyr::left_join(soilphys,layers_map)
  return(soilphys)
}



## helpers -------------

set_layer <- function(df,layers_map,id_cols){
  layers_map$j_help <- 1
  id_lmap <- c(id_cols[id_cols %in% names(layers_map)],"j_help")

  df %>%
    mutate(j_help = 1) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_lmap))) %>%
    dplyr::group_modify(~{
      l_tmp <- .y %>% dplyr::left_join(layers_map,by = id_lmap)
      .x %>%
        dplyr::rowwise() %>%
        dplyr::mutate(layer = l_tmp$layer[l_tmp$upper > depth & l_tmp$lower < depth] )
    })
}




