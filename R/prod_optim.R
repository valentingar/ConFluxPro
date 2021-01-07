#' @title prod_optim
#'
#' @description This is the optimiser-function that is minimised for the inverse,
#' production based model. It takes as input a vector of the influx, as well as the
#' values of the production to be optimised.
#'
#' @param X (numeric vector) specifying the productions to be optimised
#' @param pmap (integer vector) assigning a production from X to each step
#' @param cmap (integer vector) assigning the modelled concentrations to the observed concentrations
#' as there can be multiple observations per depth
#' @param conc (numeric) the observed concentrations
#' @param dstor (numeric) storage changes per step
#' @param C0 (numeric) The concentration at the bottom of the lowermost step
#' @param zero_flux (logical) Applies the zero-flux boundary condition(T)? If FALSE, the first value in X
#' represents the incoming flux to the lowest layer.
#'
#' @examples
#'
#' @export
#'


prod_optim<- function(X,
                      pmap,
                      cmap,
                      conc,
                      dstor,
                      C0,
                      zero_flux=T){

  #zero-flux boundary condition - if it is TRUE then there is no flux below lowest layer
  if (zero_flux == F){
    F0 <- X[1]
    X <- X[-1]
  }

  #assign production values to steps (pmap provided in function call)
  prod <- X[pmap]

  #add storage term to production
  prod <- prod+dstor

  #print(prod)
  #calculate concentration using the values provided
  conc_mod<-prod_mod_conc(prod,height,DS,F0,C0)

  #print(conc_mod)
  #assign moddeled concentrations to match observations
  conc_mod <- conc_mod[cmap]

  #print(conc_mod)
  #calculate RMSE
  RMSE <- sqrt(mean((conc-conc_mod)^2,na.rm = T))

  #penalty for too different production rates
  RMSE <- RMSE + mean(abs(diff(X)))*10


  #print(RMSE)
  return(RMSE)
}
