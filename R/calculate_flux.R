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
#' @param id_cols (character) A vector defining the id-columns uniquely identifying each profile.
#' Usually this includes Date but might also include any variations considered.
#'
#' @family FLUX
#'
#' @return FLUX
#' @examples
#' calculate_flux(gasdata,
#'                soilphys_complete,
#'                layers_map = layers_map ,
#'                gases = c("CO2","CH4","O2"),
#'                modes =c("LL","LS","EF"),
#'                param = c("DSD0","DS","SWC","Temp","p"),
#'                funs = c("harm","harm","arith","arith","arith"),
#'                id_cols = c(Date))
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
                           funs,
                           id_cols){
if(!"DS" %in% param){
  stop("cannot calculate flux: 'DS' is missing in param!")
}

  if(!"rho_air" %in% param){
    stop("cannot calculate flux: 'rho_air' is missing in param!")
  }


if(!length(which(id_cols %in% names(gasdata)))== length(id_cols)){
  warning("not all id_cols are present in gasdata.")
}
  if(!length(which(id_cols %in% names(soilphys)))== length(id_cols)){
    warning("not all id_cols are present in soilphys")
  }

  if(!"gas" %in% id_cols){
    id_cols <- c(id_cols,"gas")
  }
  if(!"Plot" %in% id_cols){
    id_cols <- c(id_cols,"Plot")
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



}else{
  layers_map <- lapply(g_plots,function(Plot){
    df <- layers_map %>% dplyr::filter(Plot == !!Plot)
  }) %>% bind_rows()

}

#subset gasdata to relevant gases
gasdata <- gasdata %>% dplyr::filter(gas %in% gases)

#removing points without data
gasdata <- gasdata %>% dplyr::filter(!is.na(NRESULT_ppm),!is.na(depth))

if(nrow(gasdata) < 2){
  stop("gasdata is empty for given gases - check your input and data!")
}

#some prep
layers_map <- layers_map %>%
  dplyr::arrange(dplyr::desc(upper))

depth_steps <- layers_map %>% #depths between the layers from top to bottom
  group_by(Plot) %>%
  slice(n = -1) %>%
  mutate(depth_steps = upper) %>%
  select(Plot,depth_steps)



#turns Inf-values to NA
gasdata$NRESULT_ppm[is.infinite(gasdata$NRESULT_ppm)==T] <- NA

#removes all NAs from gasdata
gasdata <- gasdata %>%
  dplyr::filter(is.na(NRESULT_ppm)==F,is.na(depth) == F)




#for progress tracking
n_gradients <- gasdata %>% dplyr::ungroup() %>%
  dplyr::select(dplyr::any_of({{id_cols}}))  %>%
  dplyr::distinct() %>%
  nrow()
n_soilphys <- soilphys%>% dplyr::ungroup() %>%
  dplyr::select(dplyr::any_of({{id_cols}}))  %>%
  dplyr::distinct() %>%
  nrow()

printers <-floor(seq(1,n_gradients,length.out = 11))

id_cols <- c(id_cols,"mode")

print("starting gradient")
FLUX <- lapply(modes,function(mode){
  return(gasdata %>% mutate(mode = !!mode))
}) %>%
  bind_rows() %>%
  ungroup()%>%
  dplyr::group_by(dplyr::across(dplyr::any_of({{id_cols}}))) %>%
  dplyr::mutate(n_gr = dplyr::cur_group_id(),n_tot=n_gradients) %>%
    dplyr::group_modify(~{
      if (.x$n_gr[1] %in% printers){
        print(paste0(round(.x$n_gr[1] /n_gradients*100)," %"))
      }
      FLUX <-
      dcdz_layered(.x,
                   layers_map[layers_map$Plot == .y$Plot[1],],
                   .y$mode[1],
                   depth_steps$depth_steps[depth_steps$Plot == .y$Plot[1]])
      return(FLUX)
    })

id_cols <-id_cols[!id_cols == "mode"]
print("gradient complete")
print("starting soilphys")


soilphys_layers <-soilphys_layered(gasdata %>% #decreasing size of soilphys to relevant subset
                                     dplyr::ungroup() %>%
                                     dplyr::select(dplyr::any_of({id_cols})) %>%
                                     dplyr::distinct() %>%
                                     dplyr::left_join(soilphys),
                                   layers_map,
                                   param,
                                   funs,
                                   id_cols)
print("soilphys complete")
FLUX <- FLUX %>%
  dplyr::left_join(soilphys_layers) %>%
  dplyr::mutate(flux = -DS*rho_air*dcdz_ppm) %>%
  dplyr::mutate(depth = mean(c(upper,lower)))
print("flux calculation complete")
return(FLUX)
}
