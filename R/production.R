#' @title production
#'
#' @description Easily extract the production of \code{cfp_pfres()} and
#' \code{cfp_fgres()} models per layer defined in \code{layers_map()} and calculate
#' the relative contribution per layer.
#'
#' @param x A valid \code{cfp_pfres()} or \code{cfp_fgres()} object.
#'
#' @param ... Further parameters passed on to \code{efflux()} in case of \code{cfp_fgres}.
#'
#' @details For a \code{pro_flux()} model, the extraction is straightforward
#' and simply the product of the optimised production rate (per volume) multiplied
#' by the height of the layer.
#'
#' For \code{fg_flux()}, the assumption is made that the production of the layer
#' \eqn{i} is the difference of the flux in the layer above \eqn{F_{i+1}} and the layer below
#' \eqn{F_{i-1}}. The flux below the lowest layer is assumed to be zero and the flux above the
#' topmost layer is the efflux. This approach has some uncertainties and it should be
#' evaluated if it applies to your model.
#'
#' @returns data.frame with prod_abs (\eqn{µmol / m^2 / s}), efflux (\eqn{µmol / m^2 / s}) and
#' \eqn{prod_rel = efflux / prod_abs}.
#'
#' @export

production <- function(x, ...){
  UseMethod("production")
}

#' @exportS3Method
production.cfp_fgres <- function(x, ...){
  EFFLUX <- efflux(x, ...)
  FLUX <- x$FLUX
  id_cols <- cfp_id_cols(x)
  merger <- names(EFFLUX)[names(EFFLUX) %in% names(FLUX)]

  FLUX %>%
    dplyr::left_join(EFFLUX, by = merger) %>%
    dplyr::arrange(upper) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(flux_lag = dplyr::lag(flux),
                  flux_lead = dplyr::lead(flux)) %>%
    dplyr::mutate(flux_lag = ifelse(is.na(flux_lag), 0, flux_lag),
                  flux_lead = ifelse(is.na(flux_lead), efflux, flux_lead)) %>%
    dplyr::mutate(prod_abs = flux_lead - flux_lag ) %>%
    dplyr::mutate(prod_rel = prod_abs / efflux) %>%
    dplyr::select(dplyr::any_of(c(id_cols,
                                  "upper",
                                  "lower",
                                  "depth",
                                  "layer",
                                  "prod_abs",
                                  "prod_rel",
                                  "efflux")))
}

#' @exportS3Method
production.cfp_pfres <- function(x, ...){
  EFFLUX <- efflux(x)
  PROD <- x$PROFLUX
  id_cols <- cfp_id_cols(x)
  merger <- names(x$layers_map)[names(x$layers_map) %in% names(EFFLUX)]
  merger <- c(merger, "pmap")


  PROD %>%
    dplyr::mutate(height = (upper - lower) / 100) %>%
    dplyr::mutate(flux = prod * height) %>%
    dplyr::group_by(prof_id, pmap) %>%
    dplyr::summarise(prod_abs = sum(flux)) %>%
    dplyr::left_join(EFFLUX, by = "prof_id") %>%
    dplyr::mutate(prod_rel = prod_abs / efflux) %>%
     dplyr::left_join(x$layers_map, by = merger) %>%
    dplyr::select(dplyr::any_of(c(id_cols,
                                  "upper",
                                  "lower",
                                  "depth",
                                  "layer",
                                  "prod_abs",
                                  "prod_rel",
                                  "efflux")))


}
