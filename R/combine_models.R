#' @title Combine models
#'
#' @description
#' Combinea list of multiple models or [cfp_dat()] objects into a single object.
#'
#' @param x A list of models, must inherit from [cfp_dat()]
#'
#' @examples
#' mod1 <- filter(base_dat, site == "site_a")
#' mod2 <- filter(base_dat, site == "site_b")
#' combine_models(list(mod1, mod2))
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

  stopifnot("Not a list of cfp_dat objects" =
              all(sapply(x, inherits, what = "cfp_dat")))

  stopifnot("All elements must have the same class" =
              all(sapply(lapply(x, class), identical, y = class(x[[1]]))))

  combine_models_by_reference(x[[1]], x)
}


### helpers ------
#' @rdname combine_models
#' @param x_ref Reference element of x that controls the return class and
#' attributes.
#' @export
combine_models_by_reference <- function(x_ref, x){
  UseMethod("combine_models_by_reference")
}

#' @exportS3Method
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
           "flux", "F0", "prod", "conc",
           "RMSE", "DELTA_flux", "DELTA_prod"))) %>%
      dplyr::right_join(y$profiles, by = c(cfp_id_cols(y))) %>%
      dplyr::right_join(
        y$soilphys %>%
          dplyr::select(dplyr::all_of(c("sp_id", "upper", "lower",
                                        "pmap", "step_id"))),
                                        by = c("sp_id", "upper", "lower")) %>%
      dplyr::select(!dplyr::any_of(c(cfp_id_cols(y), "gd_id", "group_id"))) %>%
         cfp_layered_profile(id_cols = "prof_id")
  )
  y
}

#' @exportS3Method
combine_models_by_reference.cfp_pfmod <- function(x_ref, x){


  new_cfp_pfmod(NextMethod(),
                zero_flux = cfp_zero_flux(x_ref),
                zero_limits = cfp_zero_limits(x_ref),
                DSD0_optim = cfp_DSD0_optim(x_ref),
                evenness_factor = cfp_evenness_factor(x_ref),
                known_flux_factor = cfp_known_flux_factor(x_ref))
}

#' @exportS3Method
combine_models_by_reference.cfp_fgres <- function(x_ref, x){

  y <- NextMethod()

  y <- cfp_fgres(
    y,
    lapply(x, function(x) x$FLUX %>%
             dplyr::left_join(x$profiles, by = c("prof_id", "gas"))) %>%
      dplyr::bind_rows(.id = "cmb_id") %>%
      dplyr::select(!dplyr::any_of("prof_id")) %>%
      dplyr::right_join(y$profiles %>%
                          dplyr::select(
                            dplyr::all_of(c(cfp_id_cols(y),
                                            "prof_id"))),
                        by = cfp_id_cols(y))%>%
      cfp_layered_profile(id_cols = "prof_id")
  )
  y
}

#' @exportS3Method
combine_models_by_reference.cfp_fgmod <- function(x_ref, x){
  cfp_fgmod(NextMethod(),
            gases = cfp_gases(x_ref),
            modes = cfp_modes(x_ref),
            param = cfp_param(x_ref),
            funs = cfp_funs(x_ref))
}

#' @exportS3Method
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
    lmap_cmb <- dplyr::bind_rows(
      lapply(x, cfp_layers_map), .id = "cmb_id") %>%
      cfp_layers_map(
        id_cols = c(unique(unlist(lapply(lmap_list, cfp_id_cols))), "cmb_id"))
  }
  if (all(sapply(sp_list, identical, y = sp_first))){
    soilphys_cmb <- sp_first
  } else {
    soilphys_cmb <- dplyr::bind_rows(sp_list, .id = "cmb_id") %>%
      cfp_soilphys(
        id_cols = c(unique(unlist(lapply(sp_list, cfp_id_cols))), "cmb_id"))
  }
  if (all(sapply(gd_list, identical, y = gd_first))){
    gasdata_cmb <- gd_first
  } else {
    gasdata_cmb <- dplyr::bind_rows(gd_list, .id = "cmb_id")%>%
      cfp_gasdata(
        id_cols = c(unique(unlist(lapply(gd_list, cfp_id_cols))), "cmb_id"))
  }

  y <- cfp_dat(gasdata_cmb, soilphys_cmb, lmap_cmb)

}


