#' @title prod_mod
#'
#' @description These functions calculates flux or concentration profile of a
#'   gas based on fixed production and known diffusion coefficient Ds. The basis
#'   is a step-wise calculation beginning from the bottom-most layer with
#'   incoming flux F0 and known concentration C0 at the lower end of the step.
#'
#'
#'   The following input parameters must be vectors sorted from the lowest depth
#'   of the profile to the highest. Note that the assigned units are examples.
#'   However, units should be concise and match over all parameters. The
#'   functions are designed to be used in optimization and are hence as fast as
#'   possible.
#'
#' @param height the height of each step in m
#' @param prod the production assigned to this step production in
#'   \eqn{\mu~mol/s/m^3}
#' @param DS the diffusion coefficient DS in \eqn{m^2/s}
#'
#' @param F0 (numeric) Incoming flux to lowest step in \eqn{\mu mol/m^2/s}
#' @param C0 (numeric) Concentration at the lower end of the lowest step in
#'   \eqn{\mu mol/m^3}.
#'
#' @returns \code{conc} in \eqn{\mu mol/m^3} or
#' \code{flux} in \eqn{\mu mol/m^2/s} at top of each step.
#'
#' @family proflux
#' @keywords internal
#' @aliases prod_mod prod_mod_flux prod_mod_conc
#'
#' @examples
#' prod_mod_conc(prod = c(0.01,0.02,0.65,0.5,0.4),
#'               height = c(0.5,0.2,0.1,0.03,0.02),
#'               DS = c(rep(2.5E-7,3),rep(1E-6,2)),
#'               F0=0,
#'               C0=48000E-6)
#' @export

prod_mod_conc <- function(prod,
                          height,
                          DS,
                          F0,
                          C0){
  flux <- cumsum(((prod) * height))+F0
  conc <- cumsum(-((prod)/(2*DS)) * height^2  -
                   c(F0,flux[-length(flux)])/DS *height)+C0
  return(conc)
}


#' @rdname prod_mod_conc
#' @export
prod_mod_x <- function(prod,
                       height,
                       DS,
                       c_air,
                       F0,
                       x0){
  flux <- cumsum(((prod) * height))+F0
  conc <- cumsum(-((prod)/(2 * DS * c_air)) * height^2
                 - c(F0,flux[-length(flux)])/(DS * c_air) * height) + x0
  return(conc)
}


#' @describeIn prod_mod_conc flux output only
#'
#' @examples
#' prod_mod_flux(prod = c(0.01,0.02,0.65,0.5,0.4),
#'               height = c(0.5,0.2,0.1,0.03,0.02),
#'               F0=0
#'               )
#' @export
prod_mod_flux <- function(prod, height, F0){
  flux <- cumsum(((prod) * height))+F0
  return(flux)
}

