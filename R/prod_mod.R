#' @title prod_mod
#'
#' @description These functions calculates flux or concentration profile of a gas based on
#' fixed production and known diffsuion coefficient Ds. The basis is a stepwise
#' calculation beginning from the bottom-most layer with incoming flux F0 and
#' known concentration C0 at the lower end of the step. \n
#' The following input parameters must be vectors sorted from the lowest depth of the profile to the highest.
#' Note that the assigned units are examples. However, units should be concise and match over all parameters. \n
#' The functions are designed to be used in optimization and are hence as fast as possible.
#'
#' @param height: the height of each step in m
#' @param prod: the production assinged to this step production in mumol/s/m^3
#' @param DS: the diffusion coefficient DS in m^2/s
#'
#' @param F0 (numeric) Incoming flux to lowest step in mumol/s/m^2
#' @param C0 (numeric) Concentration at the lower end of the lowest step in mumol/m^3.
#'
#' @return concentration at top of each step in mumol/m^3
#' @return flux at top of each step in mumol/m^2/s
#'
#' @family proflux
#'
#' @examples
#' prod_mod_conc(prod = c(0.01,0.02,0.65,0.5,0.4),
#'               height = c(0.5,0.2,0.1,0.03,0.02),
#'               DS = c(rep(2.5E-7,3),rep(1E-6,2)),
#'               F0=0,
#'               C0=48000E-6)
#' [1] 43000 37400 20800 18355 16495
#'
#' prod_mod_flux(prod = c(0.01,0.02,0.65,0.5,0.4),
#'               height = c(0.5,0.2,0.1,0.03,0.02),
#'               F0=0
#'               )
#'[1] 0.005 0.009 0.074 0.089 0.097

#'

prod_mod_conc <- function(prod,height,DS,F0,C0){
  flux <- cumsum(((prod) * height))+F0
  conc <- cumsum(-((prod)/(2*DS)) *height^2  - c(0,flux[-length(flux)])/DS *height)+C0
  return(conc)
}

prod_mod_flux <- function(prod,height,F0){
  flux <- cumsum(((prod) * height))+F0
  return(flux)
}

