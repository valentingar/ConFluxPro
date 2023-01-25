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

  # first separate groups
  x_split <- split_by_group(x)

  p <- progressr::progressor(steps = nrow(x$profiles) * length(cfp_modes(x)))


  y <- furrr::future_map(x_split,
                         calculate_flux,
                         p = p
  )


  #combine FLUX result
  y <- dplyr::bind_rows(y)

  y <- y %>%
    dplyr::left_join(x$profiles, by = names(y)[names(y) %in% names(x$profiles)]) %>%
    dplyr::select(prof_id,
           upper,
           lower,
           depth,
           layer,
           gas,
           mode,
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
calculate_flux <- function(x, p){

  gasdata <- x$gasdata
  soilphys <- x$soilphys
  layers_map <- x$layers_map
  gases <- cfp_gases(x)
  modes <- cfp_modes(x)
  param <- cfp_param(x)
  funs <- cfp_funs(x)
  id_cols <- cfp_id_cols(x)

  if(!"DS" %in% param){
    stop("cannot calculate flux: 'DS' is missing in param!")
  }

  if(!"c_air" %in% param){
    stop("cannot calculate flux: 'c_air' is missing in param!")
  }

  #some prep
  layers_map <- layers_map %>%
    dplyr::arrange(dplyr::desc(upper))

  #turns Inf-values to NA
  gasdata$x_ppm[is.infinite(gasdata$x_ppm)==T] <- NA

  #removes all NAs from gasdata
  gasdata <- gasdata %>%
    dplyr::filter(is.na(x_ppm) == F, is.na(depth) == F)

  id_cols <- c(id_cols, "mode")
  id_lmap <- id_cols[id_cols %in% names(layers_map)]

  FLUX <-
  mapply(gas = gases,
         mode = modes,
         FUN = function(gas, mode){
           gasdata <- gasdata[gasdata$gas == gas, ]

           gasdata_split <-
             split(gasdata, gasdata[, names(gasdata) %in% id_cols])

           FLUX <-
             lapply(
               gasdata_split,
               FUN = function(x, p, ...){
                 p(message = "dcdz calculation ...")
                 dcdz_layered(x, ...)
               },
               mode = mode,
               layers_map = layers_map,
               p = p
             ) %>%
             dplyr::bind_rows() %>%
             dplyr::mutate(mode = !!mode,
                           gas = !!gas)

         },
         SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(x$profiles[, c("gd_id", "prof_id")], by = "gd_id")

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

  soilphys_layers <- soilphys_layers %>%
    dplyr::left_join(x$profiles,
                     by = names(soilphys_layers)[names(soilphys_layers) %in% names(x$profiles)]) %>%
    dplyr::ungroup() %>%
    dplyr::select(tidyr::any_of(c("prof_id", "upper", "lower" , param)))

    FLUX <- FLUX %>%
    dplyr::left_join(soilphys_layers, by = c("prof_id", "upper", "lower")) %>%
    dplyr::mutate(flux = -DS*c_air*dcdz_ppm) %>%
    dplyr::mutate(depth = (upper+lower)/2) %>%
    dplyr::mutate(flux_sd = abs(flux*abs(dcdz_sd/dcdz_ppm))) %>%
    dplyr::ungroup()
}
