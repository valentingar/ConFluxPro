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
#' If there are error estimates available from a call to [bootstrap_error()],
#' the errors are propagated as follows:
#' \deqn{\Delta prod_{rel} = |\Delta efflux \cdot
#' \frac{prod_{abs}}{efflux^2}| +
#' |\Delta prod_{abs}\cdot\frac{1}{efflux}|}
#'
#' @returns data.frame with \code{prod_abs} (\eqn{µmol / m^2 / s}),
#' \code{efflux} (\eqn{µmol / m^2 / s}) and \code{prod_rel} where
#' \eqn{prod_{rel} = efflux / prod_{abs}}.
#'
#' @importFrom rlang .data
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
    dplyr::arrange("upper") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("mode",id_cols)))) %>%
    dplyr::mutate(flux_lag = dplyr::lag(.data$flux),
                  flux_lead = dplyr::lead(.data$flux)) %>%
    dplyr::mutate(flux_lag = ifelse(is.na(.data$flux_lag), 0, .data$flux_lag),
                  flux_lead = ifelse(is.na(.data$flux_lead), .data$efflux, .data$flux_lead)) %>%
    dplyr::mutate(prod_abs = .data$flux_lead - .data$flux_lag ) %>%
    dplyr::mutate(prod_rel = .data$prod_abs / .data$efflux) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c(id_cols,
                                  "mode",
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

  bootstrap_exists <- "DELTA_prod" %in% names(PROD)

  if(!bootstrap_exists){
    PROD$DELTA_F0 <- NA
    PROD$DELTA_prod <- NA
    EFFLUX$DELTA_efflux <- NA
  }

  PROD %>%
    dplyr::mutate(height = (upper - lower) / 100) %>%
    dplyr::mutate(prod_abs = prod * height) %>%
    dplyr::mutate(DELTA_prod_abs = DELTA_prod * height) %>%
    dplyr::group_by(prof_id, pmap) %>%
    dplyr::summarise(prod_abs = sum(prod_abs),
                     DELTA_prod_abs = sum(DELTA_prod_abs),
                     F0 = F0[1],
                     DELTA_F0 = DELTA_F0[1]) %>%
    dplyr::left_join(EFFLUX, by = "prof_id") %>%
    dplyr::mutate(prod_rel = prod_abs / efflux,
                  DELTA_prod_rel = abs(DELTA_prod_abs / efflux) +
                    abs(DELTA_efflux * prod_abs / efflux^2)) %>%
     dplyr::left_join(x$layers_map, by = merger) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(c(id_cols,
                                  "upper",
                                  "lower",
                                  "depth",
                                  "layer",
                                  "prod_abs",
                                  "prod_rel",
                                  "efflux",
                                  "F0",
                                  "DELTA_F0",
                                  "DELTA_prod_abs",
                                  "DELTA_prod_rel"))) %>%
    cfp_layered_profile(id_cols = id_cols)


}
