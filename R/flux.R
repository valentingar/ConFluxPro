#' @title Re-run model
#'
#' @description A function to either run \code{fg_flux()} or \code{pro_flux()}
#'   models from valid \code{cfp_fgmod} or \code{cfp_pfmod} objects.
#'
#' @param x A valid \code{cfp_fgmod} or \code{cfp_pfmod} object.
#'
#' @returns Either a \link{cfp_pfres} or \link{cfp_fgres} model result.
#'
#' @examples
#' FLUX <- ConFluxPro::base_dat |> fg_flux()
#' FLUX2 <- flux(FLUX)
#'
#' all.equal(FLUX, FLUX2)
#'
#' @export

flux <- function(x){
  UseMethod("flux")
}

#' @exportS3Method
flux.cfp_pfmod <- function(x){
  pro_flux(x)
}
#' @exportS3Method
flux.cfp_fgmod <- function(x){
  fg_flux(x)
}
