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
                           id_cols = NULL){
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

# finding combinations of id_cols that are not present
# in both layers_map and gasdata
if (any(id_cols %in% names(layers_map)) == T ){
id_nomatch <-
layers_map %>%
  dplyr::select(dplyr::any_of(id_cols)) %>%
  dplyr::distinct() %>%
  dplyr::anti_join(gasdata %>%
                     dplyr::select(dplyr::any_of(id_cols)) %>%
                     dplyr::distinct())


# warning for skipped id_cols and subsetting of gasdata
if(nrow(id_nomatch) > 0){
  warning(paste("The following values of id_cols are not represented
                in layers_map or gasdata, skipping: "))
  print(id_nomatch)
  gasdata <- gasdata %>%
    dplyr::anti_join(id_nomatch)
}}

#subset gasdata to relevant gases
gasdata <- gasdata %>%
  dplyr::filter(gas %in% gases)

#removing points without data
gasdata <- gasdata %>%
  dplyr::filter(!is.na(NRESULT_ppm),
                !is.na(depth))

if(nrow(gasdata) < 2){
  stop("gasdata is empty for given gases - check your input and data!")
}

#some prep
layers_map <- layers_map %>%
  dplyr::arrange(dplyr::desc(upper))

depth_steps <- layers_map %>% #depths between the layers from top to bottom
  dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
  dplyr::slice(n = -1) %>%
  dplyr::mutate(depth_steps = upper) %>%
  dplyr::select(dplyr::any_of(c(id_cols,"depth_steps")))



#turns Inf-values to NA
gasdata$NRESULT_ppm[is.infinite(gasdata$NRESULT_ppm)==T] <- NA

#removes all NAs from gasdata
gasdata <- gasdata %>%
  dplyr::filter(is.na(NRESULT_ppm)==F,is.na(depth) == F)


#if there is only one group in layers_map, this ensures correct joins
# when calculating the fluxes
layers_map$j_help <- 1
depth_steps$j_help <- 1


#for progress tracking
n_gradients <- gasdata %>%
  dplyr::ungroup() %>%
  dplyr::select(dplyr::any_of({{id_cols}}))  %>%
  dplyr::distinct() %>%
  nrow()
n_soilphys <- soilphys%>%
  dplyr::ungroup() %>%
  dplyr::select(dplyr::any_of({{id_cols}}))  %>%
  dplyr::distinct() %>%
  nrow()

printers <-floor(seq(1,n_gradients,length.out = 11))

id_cols <- c(id_cols,"mode")
id_lmap <- c(id_cols[id_cols %in% names(layers_map)],"j_help")

print("starting gradient")
FLUX <- lapply(modes,function(mode){
  return(gasdata %>% mutate(mode = !!mode))
}) %>%
  dplyr::bind_rows() %>%
  dplyr::ungroup()%>%
  dplyr::mutate(j_help = 1) %>%
  dplyr::group_by(dplyr::across(dplyr::any_of({c(id_cols,"j_help")}))) %>%
  dplyr::mutate(n_gr = dplyr::cur_group_id(),
                n_tot=n_gradients) %>%
    dplyr::group_modify(~{
      if (.x$n_gr[1] %in% printers){
        print(paste0(round(.x$n_gr[1] /n_gradients*100)," %"))
      }
      FLUX <-
      dcdz_layered(.x,
                   .y %>% dplyr::left_join(layers_map,by = id_lmap),
                   .y$mode[1],
                   .y %>% dplyr::left_join(depth_steps,by = id_lmap) %>% dplyr::pull(depth_steps))
      FLUX <- FLUX %>% dplyr::select(!dplyr::any_of(c("j_help","gas","mode")))
    })

id_cols <-id_cols[!id_cols == "mode"]
print("gradient complete")
print("starting soilphys")

if(length(id_cols[id_cols %in% names(soilphys)])>0){
soilphys <-
gasdata %>% #decreasing size of soilphys to relevant subset
  dplyr::ungroup() %>%
  dplyr::select(dplyr::any_of({id_cols})) %>%
  dplyr::distinct() %>%
  dplyr::left_join(soilphys)}

soilphys_layers <-soilphys_layered(soilphys,
                                   layers_map,
                                   param,
                                   funs,
                                   id_cols)
print("soilphys complete")
FLUX <- FLUX %>%
  dplyr::left_join(soilphys_layers) %>%
  dplyr::mutate(flux = -DS*rho_air*dcdz_ppm) %>%
  dplyr::mutate(depth = (upper+lower)/2) %>%
  dplyr::mutate(flux_sd = abs(flux*abs(dcdz_sd/dcdz_ppm)))
print("flux calculation complete")
return(FLUX)
}
