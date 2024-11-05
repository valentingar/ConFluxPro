#' @title Re-run model
#'
#' @description A function to either run \code{fg_flux()} or \code{pro_flux()}
#'   models from valid \code{cfp_fgmod} or \code{cfp_pfmod} objects.
#'
#' @param x A valid \code{cfp_fgmod} or \code{cfp_pfmod} object.
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
