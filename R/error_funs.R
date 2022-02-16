#' @title error_funs
#'
#' @description A set of functions that can be called on an
#' cfp_pfres object (the result of a call to pro_flux) to assess
#' the quality of the model.
#'
#' @param x A cfp_pfres object, that is returned by a call
#' to pro_flux()
#'
#' @param param_cols The columns that, together, define different parameters (e.g. different gases)
#' for which NRMSEs should be calculated separately (e.g. "gas"). Defaults to
#' the id_cols of layers_map. If no such destinction is wished, set to \code{character()}
#'
#' @param EFFLUX A data.frame with (at most) one value of efflux per profile of x. Must
#' contain any id_cols of x needed.
#'
#' @param ... Further arguments passed to \code{efflux()} in case of cfp_fgres.
#'
#' @inheritParams rmse
#'
#' @details For error_concentration, the way the error parameter is calculated for cfp_fgres and cfp_pfres objects
#' is entirely different and should not be used in comparison between the two. NRMSE of
#' cfp_pfres objects are calculated as the mean of depth-wise NRMSEs of modelled versus
#' input gas concentrations. 'NRMSE's of cfp_fgres objects simply calculate the mean of
#' (dcdz_sd / dcdz_ppm) per group described in param_cols.
#'
#' @aliases error_concentration error_efflux error_funs
#'
#' @rdname error_funs
#' @export

error_concentration <- function(
  x,
  param_cols = NULL,
  normer
){
  UseMethod("error_concentration")
}

#' @exportS3Method
error_concentration.cfp_pfres <- function(
    x,
    param_cols = NULL,
    normer
  ) {

  if (is.null(param_cols)){
    #using layers map id_cols as default
    param_cols <- cfp_id_cols(x$layers_map)
  }

    gasdata <-
      x$gasdata %>%
      dplyr::select(gd_id,
                    NRESULT_ppm,
                    depth) %>%
      dplyr::rename(upper = depth,
                    conc_ref = NRESULT_ppm)
    soilphys <-
      x$soilphys %>%
      dplyr::select(sp_id,
                    upper,
                    step_id,
                    rho_air)

    x$profiles %>%
      dplyr::left_join(x$PROFLUX, by = c("prof_id", "sp_id")) %>%
      dplyr::left_join(soilphys %>%
                         dplyr::select(sp_id,
                                       step_id,
                                       rho_air),
                       by = c("sp_id", "step_id")) %>%
      dplyr::select(dplyr::any_of(
        {c(param_cols,
           "prof_id",
           "gd_id",
           "upper",
           "conc",
           "rho_air")}
      )) %>%
      dplyr::mutate(conc_ppm = conc/rho_air) %>%
      dplyr::left_join(gasdata, by = c("gd_id", "upper")) %>%

      # grouping by upper first
      # because different depths have different
      # value ranges (lower are higher).
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({c(param_cols,"upper")}))) %>%
      dplyr::summarise(NRMSE = nrmse(conc_ppm,
                                     conc_ref,
                                     norm = !!normer)) %>%

      # then by only the param_cols provided
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({param_cols}))) %>%

      # then calculation of mean of all depth-NRMSEs
      dplyr::summarise(NRMSE = mean(NRMSE,
                                    na.rm = T))
}

#' @exportS3Method
error_concentration.cfp_fgres <- function(
  x,
  param_cols = NULL,
  normer = NULL){

  if (is.null(param_cols)){
    #using layers map id_cols as default
    param_cols <- cfp_id_cols(x$layers_map)
  }

  merger <- names(x$FLUX)[names(x$FLUX) %in% names(x$profiles)]

  x$FLUX %>%
    dplyr::left_join(x$profiles, by = merger) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(param_cols))) %>%
    dplyr::summarise(NRMSE = mean(dcdz_sd / abs(dcdz_ppm), na.rm = TRUE))
}


#' @rdname error_funs
#' @export
error_efflux <-function(x,
         param_cols,
         EFFLUX,
         normer,
         ...){
  UseMethod("error_efflux")
}

#' @exportS3Method
  error_efflux.default <- function(x,
                           param_cols,
                           EFFLUX,
                           normer,
                           ...){

    id_cols <- cfp_id_cols(x)

    EFFLUX <-
      EFFLUX %>%
      data.frame() %>%
      dplyr::select(
        dplyr::any_of({c(id_cols,"efflux")})) %>%
      dplyr::rename(efflux_ref = efflux)

    merger <- id_cols[id_cols %in% names(EFFLUX)]

    x %>%
      efflux(...) %>%
      dplyr::left_join(EFFLUX, by = merger) %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({param_cols}))) %>%
      dplyr::summarise(NRMSE = nrmse(efflux,
                                     efflux_ref,
                                     norm = !!normer))

  }

