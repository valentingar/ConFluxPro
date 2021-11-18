#' @title proflux_error
#'
#' @description A set of functions that can be called on an
#' PFres object (the result of a call to pro_flux) to assess
#' the quality of the model.
#'
#' @param PROFLUX A PFres object, that is returned by a call
#' to pro_flux()
#'
#' @param param_cols The columns that, together, define different parameters (e.g. different gases)
#' for which NRMSEs should be calculated separately (e.g. "gas").
#'
#' @inheritParams rmse
#'
#' @name proflux_error
NULL
#'
#' @rdname proflux_error

  error_gasdata <- function(
    PROFLUX,
    param_cols,
    normer
  ) {
    id_cols <- PF_id_cols(PROFLUX)
    gasdata <- PF_gasdata(PROFLUX)

    gasdata <- gasdata %>%
      dplyr::select(dplyr::any_of(
        {c(id_cols,
           "NRESULT_ppm",
           "depth")})) %>%
      dplyr::rename(upper = depth,
                    conc_ref = NRESULT_ppm)
    PROFLUX %>%
      dplyr::select(dplyr::any_of(
        {c(id_cols,
           "upper",
           "conc",
           "rho_air")}
      )) %>%
      dplyr::mutate(conc_ppm = conc/rho_air) %>%
      dplyr::left_join(gasdata) %>%

      # grouping by upper first
      # because different depths have different
      # value ranges (lower are higher).
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({c(param_cols,"upper")}))) %>%
      dplyr::summarise(NRMSE = nrmse(conc/rho_air,
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


#' @rdname proflux_error

  error_efflux <- function(PROFLUX,
                           param_cols,
                           EFFLUX,
                           normer){

    id_cols <- PF_id_cols(PROFLUX)

    EFFLUX <-
      EFFLUX %>%
      data.frame() %>%
      dplyr::select(
        dplyr::any_of({c(id_cols,"efflux")})) %>%
      dplyr::rename(efflux_ref = efflux)

    PROFLUX %>%
      pf_efflux() %>%
      dplyr::left_join(EFFLUX) %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({param_cols}))) %>%
      dplyr::summarise(NRMSE = nrmse(efflux,
                                     efflux_ref,
                                     norm = !!normer))

  }




