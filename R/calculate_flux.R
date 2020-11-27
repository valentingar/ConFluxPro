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
      dcdz_layered(.x,layers_map,mode)}) %>%
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
  dplyr::mutate(n_air = p*100 / (8.314 * (273.15+Temp))) %>%
  dplyr::mutate(flux = -DS*n_air*dcdz)
print("flux calculation complete")
return(FLUX)
}
