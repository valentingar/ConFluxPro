#' @title deepflux
#'
#' @description Extract the incoming and outgoing flux from below the deepest layer
#' of a \code{pro_flux()} model.
#'
#' @param x A valid \code{cfp_pfres()} object.
#'
#' @param ... Further parameters passed on to \code{efflux()} in case of \code{cfp_fgres}.
#'
#' @details F0 represents the flux below the lowest layer defined in the \code{cfp_pfres()} model
#'
#' @returns data.frame with F0 (\eqn{µmol / m^2 / s})
#'
#' @export

deepflux <- function(x, ...){
  UseMethod("deepflux")
}

#' @exportS3Method
deepflux.cfp_pfres <- function(x, ...){
  PROD <- x$PROFLUX
  id_cols <- cfp_id_cols(x)

  PROD %>%
    dplyr::select(prof_id, F0) %>%
    dplyr::distinct() %>%
    dplyr::left_join(x$profiles %>%
                       dplyr::select(dplyr::any_of(c(id_cols, "prof_id"))))

}
