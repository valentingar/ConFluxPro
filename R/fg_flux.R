#' @title fg_flux
#'
#' @description This function takes a valid input dataset in cfp_dat
#' and calculates fluxes accordingly. Fluxes are calculated for each layer defined
#' in layers map and are given in mumol/m^2/s.
#'
#'
#' @inheritParams pro_flux
#'
#' @inheritParams cfp_dat
#'
#' @inheritParams cfp_gasdata
#'
#' @param ... Additional arguments passed to fg_flux.cfp_fgmod. Can be of the following:
#'
#' @param gases (character) A character vector defining the gases for which
#' gluxes shall be calulated.
#' @param modes (character) A character vector specifying mode(s) for dcdz
#'   calculation. Can be "LL","LS","EF".
#' @param param (character) A vector containing the the parameters of soilphys,
#'   for which means should be calculated, must contain "c_air" and "DS", more
#'   parameters help interpretation
#' @param funs (character) A vector defining the type of mean to be used. One of
#'   "arith" or "harm"
#' @rdname fg_flux
#' @export fg_flux

fg_flux <- function(x, ...){
  UseMethod("fg_flux")
}

#'

#' @exportS3Method
fg_flux.cfp_dat <- function(x, ...){

  x <- cfp_fgmod(x,...)
  .Class <- "cfp_fgmod"
  NextMethod()
}

#' @exportS3Method
fg_flux.cfp_fgres <- function(x, ...){
  x <- as_cfp_fgmod(x)
  NextMethod()
}

#' @exportS3Method
fg_flux.cfp_fgmod <- function(x, ...){

  y <-
  calculate_flux(x$gasdata,
                 x$soilphys,
                 x$layers_map,
                 cfp_gases(x),
                 cfp_modes(x),
                 cfp_param(x),
                 cfp_funs(x),
                 cfp_id_cols(x))
  y <- y %>%
    dplyr::left_join(x$profiles) %>%
    dplyr::select(prof_id,
           upper,
           lower,
           depth,
           layer,
           mode,
           gas,
           flux,
           flux_sd,
           dcdz_ppm,
           dcdz_sd,
           dc_ppm,
           c_air,
           DS,
           r2)


  cfp_fgres(x,y)
}




#' @rdname fg_flux
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

  if(!"c_air" %in% param){
    stop("cannot calculate flux: 'c_air' is missing in param!")
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
    id_cols_lmap <- id_cols[id_cols %in% names(layers_map)]

    id_nomatch <-
      layers_map %>%
      dplyr::select(dplyr::any_of(id_cols)) %>%
      dplyr::distinct() %>%
      dplyr::anti_join(gasdata %>%
                         dplyr::select(dplyr::any_of(id_cols)) %>%
                         dplyr::distinct(),
                       by = id_cols_lmap)


    # warning for skipped id_cols and subsetting of gasdata
    if(nrow(id_nomatch) > 0){
      message(paste("The following values of id_cols are not represented
                in layers_map or gasdata, skipping: "))
      cat(id_nomatch)

      merger <- names(gasdata)[names(gasdata) %in% names(id_nomatch)]

      gasdata <- gasdata %>%
        dplyr::anti_join(id_nomatch, by = merger)
    }}

  #subset gasdata to relevant gases
  gasdata <- gasdata %>%
    dplyr::filter(gas %in% gases)

  #removing points without data
  gasdata <- gasdata %>%
    dplyr::filter(!is.na(x_ppm),
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
  gasdata$x_ppm[is.infinite(gasdata$x_ppm)==T] <- NA

  #removes all NAs from gasdata
  gasdata <- gasdata %>%
    dplyr::filter(is.na(x_ppm)==F,is.na(depth) == F)


  #if there is only one group in layers_map, this ensures correct joins
  # when calculating the fluxes
  layers_map$j_help <- 1
  depth_steps$j_help <- 1


  #for progress tracking
  n_gradients <- gasdata %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(id_cols))  %>%
    dplyr::distinct() %>%
    nrow()
  n_soilphys <- soilphys%>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(id_cols))  %>%
    dplyr::distinct() %>%
    nrow()

  id_cols <- c(id_cols, "mode")
  id_lmap <- c(id_cols[id_cols %in% names(layers_map)],"j_help")

  FLUX <- lapply(modes,function(mode){
    return(gasdata %>% dplyr::mutate(mode = !!mode))
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::ungroup()%>%
    dplyr::mutate(j_help = 1) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({c(id_cols,"j_help")}))) %>%
    dplyr::mutate(n_gr = dplyr::cur_group_id(),
                  n_tot=n_gradients) %>%
    dplyr::group_modify(~{
      FLUX <-
        dcdz_layered(.x,
                     .y %>% dplyr::left_join(layers_map, by = id_lmap),
                     .y$mode[1],
                     .y %>% dplyr::left_join(depth_steps, by = id_lmap) %>%
                       dplyr::pull(depth_steps))
      FLUX <- FLUX %>%
        dplyr::select(!dplyr::any_of(c("j_help","gas","mode",id_cols)))
    })

  id_cols <-id_cols[!id_cols == "mode"]

  if(length(id_cols[id_cols %in% names(soilphys)])>0){

    merger <- names(soilphys)[names(soilphys) %in% names(gasdata)]
    merger <- merger[merger %in% id_cols]

    soilphys <-
      gasdata %>% #decreasing size of soilphys to relevant subset
      dplyr::ungroup() %>%
      dplyr::select(dplyr::any_of(id_cols)) %>%
      dplyr::distinct() %>%
      dplyr::left_join(soilphys, by = merger)
    }

  soilphys_layers <-soilphys_layered(soilphys,
                                     layers_map,
                                     param,
                                     funs,
                                     id_cols)

  merger <- names(FLUX)[names(FLUX) %in% names(soilphys_layers)]

  FLUX <- FLUX %>%
    dplyr::left_join(soilphys_layers, by = merger) %>%
    dplyr::mutate(flux = -DS*c_air*dcdz_ppm) %>%
    dplyr::mutate(depth = (upper+lower)/2) %>%
    dplyr::mutate(flux_sd = abs(flux*abs(dcdz_sd/dcdz_ppm))) %>%
    dplyr::ungroup() %>%
    dplyr::select(!j_help)
}
