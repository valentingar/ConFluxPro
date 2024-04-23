#' @title prod_optim
#'
#' @description This is the optimizer-function that is minimized for the inverse,
#' production based model. It takes as input a vector of the influx, as well as the
#' values of the production to be optimized.
#'
#' This function is embedded in pro_flux and is not intended to be used manually.
#'
#' @param X (numeric vector) specifying the productions to be optimized
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
# #' (numeric) storage changes per step (same unit as the productions given in X).
#' @param C0 (numeric) The concentration at the
#' bottom of the lowermost step.
#' @param zero_flux (logical) Applies the zero-flux boundary
#' condition(TRUE)? If FALSE, the first value in X
#' represents the incoming flux to the lowest layer.
#' @param F0 (numeric) flux into lowest layer.
#' @param known_flux RESERVED FOR FUTURE EXPANSION
# #' (numeric) known surface flux to be matched
#' @param known_flux_factor (numeric) a factor defining how much the known flux
#' should weigh in the RMSE calculation
#' @param DSD0_optim (logical) should \code{DSD0} be optimised as well?
#' @param layer_couple (numeric vector) A vector defining the weights that bind
#' the different layers together. If all is zero, no penalisation for stark differences
#' between the optimised production rates of adjecent layers takes place
#' @param wmap (numeric) A vector defining the weights of the different concentration
#' measurements in the RMSE calculation
#' @param evenness_factor (numeric) Defines strong should stark differences between
#' the production rates and very small production rates be penalised.
#'
#'
#' @return RMSE real mean square error of the modeled and measured concentration.

#' @family proflux
#'
#' @export
#'


prod_optim<- function(X,
                      height,
                      DS,
                      D0 = NA,
                      C0,
                      pmap,
                      cmap,
                      conc,
                      dstor = 0,
                      zero_flux = TRUE,
                      F0 = 0,
                      known_flux = NA,
                      known_flux_factor = 0,
                      DSD0_optim = FALSE,
                      layer_couple,
                      wmap,
                      evenness_factor){

  #zero-flux boundary condition - if it is TRUE then there is no flux below lowest layer
  if (!zero_flux){
    F0 <- X[1]
    X <- X[-1]
  }
  # if(DSD0_optim){
  #   DSD0_fit <- X[((length(X) / 2) + 1):length(X)]
  #   X <- X[1:length(X) / 2]
  #   DS <-  DSD0_fit[pmap]*D0
  #
  # }

  #assign production values to steps (pmap provided in function call)
  prod <- X[pmap]
  #add storage term to production
  # prod <- prod+dstor

  #calculate concentration using the values provided
  conc_mod <- prod_mod_conc(prod,height,DS,F0,C0)

  #assign moddeled concentrations to match observations
  conc_mod <- conc_mod[cmap]

  #calculate RMSE
  k <- (conc-conc_mod)^2
  k <- k*wmap #weigh the observations that depend on higher degrees of freedom more
  #k <- k[is.finite(k)]
  RMSE <- sqrt(sum(k)/length(k))/(sum(conc)/length(conc))

  #penalty for too different production rates
  prod_penal <- ((sum(abs((X[-1]-X[-length(X)])*layer_couple))/(length(X))) / (abs(sum(prod*height))+0.000001) )
  #if (is.finite(prod_penal)){
    RMSE <- RMSE + prod_penal
  #}

  #penalty to prevent zero_fluxes
  pmax <- max(X, na.rm = TRUE)
  evenness_penal <- evenness_factor*sum(pmax^2 / (abs(prod/height) + 0.00001))

  RMSE <- RMSE + evenness_penal

  #penalty for not meeting known_flux
  # if (!is.na(known_flux)){
  #   RMSE <- RMSE + sum(abs(known_flux - (sum(height*prod)+F0)))*known_flux_factor
  # }

  return(RMSE)
}
