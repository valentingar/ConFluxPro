#' @title prod_optim
#'
#' @description This is the optimizer-function that is minimized for the inverse,
#' production based model. It takes as input a vector of the influx, as well as the
#' values of the production to be optimized.
#'
#' This function is embedded in pro_flux and is not intended to be used manually.
#'
#' @param X (numeric vector) specifying the productions to be optimized
#' @param pmap (integer vector) assigning a production from X to each step
#' @param cmap (integer vector) assigning the modeled concentrations to the observed concentrations
#' as there can be multiple observations per depth
#' @param conc (numeric) the observed concentrations (in the same unit as the modelled concentrations).
#' @param dstor (numeric) storage changes per step (same unit as the productions given in X).
#' @param C0 (numeric) The concentration at the bottom of the lowermost step.
#' @param zero_flux (logical) Applies the zero-flux boundary condition(T)? If FALSE, the first value in X
#' represents the incoming flux to the lowest layer.
#' @param F0 (numeric) flux into lowest layer.
#'
#' @param RMSE real mean square error of the modeled and measured concentration.
#'
#' @examples
#'
#' @family proflux
#'
#' @export
#'


prod_optim<- function(X,
                      height,
                      DS,
                      C0,
                      pmap,
                      cmap,
                      conc,
                      dstor,
                      zero_flux=T,
                      F0 = 0,
                      known_flux = NA,
                      known_flux_factor = 0,
                      Ds_optim = F,
                      layer_couple,
                      wmap){

  #zero-flux boundary condition - if it is TRUE then there is no flux below lowest layer
  if (!zero_flux){
    F0 <- X[1]
    X <- X[-1]
  }
  if(Ds_optim){
    Ds_fit <- X[((length(X) / 2) + 1):length(X)]
    X <- X[1:length(X) / 2]
    DS <-  Ds_fit[pmap]

  }

  #assign production values to steps (pmap provided in function call)
  prod <- X[pmap]
  #print(prod)
  #add storage term to production
  prod <- prod+dstor

  #print(prod)
  #calculate concentration using the values provided
  conc_mod <- prod_mod_conc(prod,height,DS,F0,C0)

  #print(conc_mod)
  #assign moddeled concentrations to match observations
  conc_mod <- conc_mod[cmap]

  #print(conc_mod)
  #calculate RMSE
  k <- (conc-conc_mod)^2
  k <- k*wmap #weigh the observations that depend on higher degrees of freedom more
  #k <- k[is.finite(k)]
  RMSE <- sqrt(sum(k)/length(k))/(sum(conc)/length(conc))

  #penalty for too different production rates
  prod_penal <- ((sum(abs((X[-1]-X[-length(X)])*layer_couple))/(length(X))))
  #if (is.finite(prod_penal)){
    RMSE <- RMSE + prod_penal
  #}

  #penalty for not meeting known_flux
  if (is.finite(known_flux)){
    RMSE <- RMSE + sum(abs(known_flux - (sum(height*prod)+F0)))*known_flux_factor
  }

  return(RMSE)
}
