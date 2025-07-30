#' @title coercion
#'
#' @name coercion
#' @description These functions help coerce different object types in
#' ConFluxPro.
#'
#' @param x An object which should be coerced
#'
#' @returns Either a [cfp_fgmod] or [cfp_pfmod] model frame
#'
#' @examples
#' PROFLUX <- pro_flux(base_dat)
#' as_cfp_pfmod(PROFLUX)
#'
#' FLUX <- fg_flux(base_dat)
#' as_cfp_fgmod(FLUX)
#'
#'
#' @rdname coercion
NULL
