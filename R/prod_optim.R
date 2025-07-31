#' @title prod_optim
#'
#' @description This is the optimizer-function that is
#' minimized for the inverse, production based model.
#' It takes as input a vector of the influx, as well as the
#' values of the production to be optimized.
#'
#' This function is embedded in [pro_flux()] and is not intended to be
#' used manually.
#'
#' @param X (numeric vector) specifying the production rates to be optimized
#' @param height (numeric vector) giving the height of each step
#' @param DS (numeric vector) giving the DS of each step
#' @param D0 RESERVED FOR FUTURE EXPANSION
# #' (numeric vector) giving the D0 of each step
#' @param pmap (integer vector) assigning a production from X to each step
#' @param cmap (integer vector) assigning the modeled concentrations to the
#' observed concentrations as there can be multiple observations per depth
#' @param conc (numeric) the observed concentrations (in the same unit as
#' the modelled concentrations).
#' @param dstor RESERVED FOR FUTURE EXPANSION
# (numeric) storage changes per step (same unit as the productions given in X).
#' @param C0 (numeric) The concentration at the
#' bottom of the lowermost step.
#' @param zero_flux (logical) Applies the zero-flux boundary
#' condition(`TRUE`)? If `FALSE`, the first value in `X`.
#' represents the incoming flux to the lowest layer.
#' @param F0 (numeric) flux into lowest layer.
#' @param known_flux RESERVED FOR FUTURE EXPANSION
# #' (numeric) known surface flux to be matched
#' @param known_flux_factor RESERVED FOR FUTURE EXPANSION
# (numeric) a factor defining how much the known flux
# should weigh in the RMSE calculation
#' @param DSD0_optim RESERVED FOR FUTURE EXPANSION
# (logical) should \code{DSD0} be optimized as well?
#' @param layer_couple (numeric vector) A vector defining the weights that bind
#' the different layers together. If all is zero, no penalisation for stark
#' differences between the optimized production rates of adjacent layers takes
#' place
#' @param wmap (numeric) A vector defining the weights of the different
#' concentration measurements in the RMSE calculation.
#' @param evenness_factor (numeric) Defines strong should stark differences
#' between the production rates and very small production rates be penalized.
#'
#' @returns A modified RMSE root mean square error of the modeled and measured
#' concentration.
#' @examples
#' \donttest{
#' prod_optim(c(1,1,1),
#'  c(0.1,0.1,0.1),
#'  DS = D0_massman("CO2", 10, 1013),
#'  C0 = c(42*2000/1000/1000),
#'  conc = c(400, 1000, 2000)*42/1000/1000,
#'  wmap = 1,
#'  layer_couple = 1,
#'  evenness_factor = 1)
#'  }
#'

#' @family proflux
#'
#' @keywords internal
#'
#' @export
#'


prod_optim<- function(X,
                      height,
                      DS,
                      D0 = NA,
                      x0,
                      c_air,
                      pmap,
                      cmap,
                      x_ppm,
                      dstor = 0,
                      zero_flux = TRUE,
                      F0 = 0,
                      layer_couple,
                      wmap,
                      evenness_factor){

  # zero-flux boundary condition, TRUE: there is no flux below lowest layer
  if (!zero_flux){
    F0 <- X[1]
    X <- X[-1]
  }

  #assign production values to steps (pmap provided in function call)
  prod <- X[pmap]

  #calculate concentration using the values provided
  x_ppm_mod <- prod_mod_conc(
    prod,
    height,
    DS,
    c_air,
    F0,
    x0)

  #assign modeled concentrations to match observations
  x_ppm_mod <- x_ppm_mod[cmap]

  #calculate RMSE
  k <- (x_ppm-x_ppm_mod)^2
  k <- k*wmap #weigh the observations that depend on higher degrees of
  # freedom more
  #k <- k[is.finite(k)]
  RMSE <- sqrt(sum(k)/length(k))/(sum(x_ppm)/length(x_ppm))

  #penalty for too different production rates
  prod_penal <- ((sum(abs((X[-1]-X[-length(X)])*layer_couple))/(length(X))) /
                   (abs(sum(prod*height))+0.000001) )
  RMSE <- RMSE + prod_penal

  #penalty to prevent zero_fluxes
  pmax <- max(X, na.rm = TRUE)
  evenness_penal <- evenness_factor*sum(pmax^2 / (abs(prod/height) + 0.00001))

  RMSE <- RMSE + evenness_penal

  return(RMSE)
}
