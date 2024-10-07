#' @title Combine Models
#'
#' @description
#' A short description...
#'
#'
#'
#'
#'
#'
#'
#'
#' @export

combine_models <- function(x){
  UseMethod("combine_models")
}



#' @rdname combine_models
#' @exportS3Method
combine_models.cfp_altres <- function(x){

  NextMethod()
}


#' @rdname combine_models
#' @exportS3Method
combine_models.list <- function(x){

  stopifnot("Not a list of cfp_dat objects" = all(sapply(x, inherits, what = "cfp_dat")))

  stopifnot("All elements must have the same class" = all(sapply(lapply(x, class), identical, y = class(x[[1]]))))


  # F_list <- NULL
  # if( "PROFLUX" %in% names(x[[1]])){
  #   F_list <- lapply(x, function(x) x$PROFLUX) %>%
  #     dplyr::bind_rows(.id = "cmb_id") %>%
  #     cfp_layered_profile(id_cols = c(cfp_id_cols(x$PROFLUX), "cmb_id"))
  # } else if ("FLUX" %in% names(x[[1]])){
  #   F_list <- lapply(x, function(x) x$FLUX)%>%
  #     dplyr::bind_rows(.id = "cmb_id") %>%
  #     cfp_layered_profile(id_cols = c(cfp_id_cols(x$FLUX), "cmb_id"))
  # }


  combine_models_by_reference(x[[1]], x)
}


### helpers ------

combine_models_by_reference <- function(x_ref, x){
  UseMethod("combine_models_by_reference")
}

combine_models_by_reference.cfp_pfres <- function(x_ref, x){


  y <- NextMethod()

  y <- cfp_pfres(
    y,
    lapply(x, function(x) x$PROFLUX %>%
             dplyr::left_join(x$profiles, by = c("prof_id", "sp_id"))) %>%
         dplyr::bind_rows(.id = "cmb_id") %>%
         dplyr::select(dplyr::any_of(c(
           cfp_id_cols(y),
           "upper", "lower",
           "flux", "F0", "prod", "conc", "RMSE", "DELTA_flux", "DELTA_prod")))  %>%
      dplyr::right_join(y$profiles, by = c(cfp_id_cols(y))) %>%
      dplyr::right_join(y$soilphys %>%
                          dplyr::select(dplyr::all_of(c("sp_id", "upper", "lower", "pmap", "step_id"))),
                                        by = c("sp_id", "upper", "lower")) %>%
      dplyr::select(!any_of(c(cfp_id_cols(y), "gd_id", "group_id"))) %>%
         cfp_layered_profile(id_cols = "prof_id")
  )
  y
}

combine_models_by_reference.cfp_pfmod <- function(x_ref, x){


  new_cfp_pfmod(NextMethod(),
                zero_flux = cfp_zero_flux(x_ref),
                zero_limits = cfp_zero_limits(x_ref),
                DSD0_optim = cfp_DSD0_optim(x_ref),
                evenness_factor = cfp_evenness_factor(x_ref),
                known_flux_factor = cfp_known_flux_factor(x_ref))
}


combine_models_by_reference.cfp_fgres <- function(x_ref, x){
  NextMethod()
}

combine_models_by_reference.cfp_fgmod <- function(x_ref, x){
  NextMethod()
}


combine_models_by_reference.cfp_dat <- function(x_ref, x){
  lmap_list <- lapply(x, cfp_layers_map)
  lmap_first <- lmap_list[[1]]
  sp_list <- lapply(x, cfp_soilphys)
  sp_first <- sp_list[[1]]
  gd_list <- lapply(x, cfp_gasdata)
  gd_first <- gd_list[[1]]


  if (all(sapply(lmap_list, identical, y = lmap_first))){
    lmap_cmb <- lmap_first
  } else {
    lmap_cmb <- dplyr::bind_rows(lapply(x, cfp_layers_map), .id = "cmb_id") %>%
      cfp_layers_map(id_cols = c(unique(sapply(lmap_list, cfp_id_cols)), "cmb_id"))
  }
  if (all(sapply(sp_list, identical, y = sp_first))){
    soilphys_cmb <- sp_first
  } else {
    soilphys_cmb <- dplyr::bind_rows(sp_list, .id = "cmb_id") %>%
      cfp_soilphys(id_cols = c(unique(unlist(lapply(sp_list, cfp_id_cols))), "cmb_id"))
  }
  if (all(sapply(gd_list, identical, y = gd_first))){
    gasdata_cmb <- gd_first
  } else {
    gasdata_cmb <- dplyr::bind_rows(gd_list, .id = "cmb_id")%>%
      cfp_gasdata(id_cols = c(unique(unlist(lapply(gd_list, cfp_id_cols))), "cmb_id"))
  }

  y <- cfp_dat(gasdata_cmb, soilphys_cmb, lmap_cmb)

}




reconstruct_class <- function(dat_ref, dat_new, ...){
  UseMethod("reconstruct_class")
}

reconstruct_class.cfp_pfres <- function(dat_ref, dat_new, y = NULL){

  dat_new <- cfp_pfres(NextMethod(), y)

}


reconstruct_class.cfp_pfmod <- function(dat_ref, dat_new){

  new_cfp_pfmod(dat_new,
                zero_flux = cfp_zero_flux(dat_ref),
                zero_limits = cfp_zero_limits(dat_ref),
                DSD0_optim = cfp_DSD0_optim(dat_ref),
                evenness_factor = cfp_evenness_factor(dat_ref),
                known_flux_factor = cfp_known_flux_factor(dat_ref))

}

