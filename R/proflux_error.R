#' @title proflux_error
#'
#' @description A set of functions that can be called on an
#' cfp_pfres object (the result of a call to pro_flux) to assess
#' the quality of the model.
#'
#' @param x A cfp_pfres object, that is returned by a call
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
    x,
    param_cols,
    normer
  ) {

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
      dplyr::left_join(x$PROFLUX, by = "prof_id") %>%
      dplyr::left_join(soilphys, by = c("sp_id", "step_id")) %>%
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


#' @rdname proflux_error

  error_efflux <- function(x,
                           param_cols,
                           EFFLUX,
                           normer){

    id_cols <- cfp_id_cols(x)

    EFFLUX <-
      EFFLUX %>%
      data.frame() %>%
      dplyr::select(
        dplyr::any_of({c(id_cols,"efflux")})) %>%
      dplyr::rename(efflux_ref = efflux)

    merger <- id_cols[id_cols %in% names(EFFLUX)]

    x %>%
      efflux() %>%
      dplyr::left_join(EFFLUX, by = merger) %>%
      dplyr::group_by(
        dplyr::across(
          dplyr::any_of({param_cols}))) %>%
      dplyr::summarise(NRMSE = nrmse(efflux,
                                     efflux_ref,
                                     norm = !!normer))

  }

