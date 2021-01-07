#' @title prod_mod
#'
#' @description Function calculates flux and concentration of a gas based on
#' fixed production and known diffsuion coefficient DS. The basis is a stepwise
#' calculation beginning from the bottom-most layer with incoming flux F0 and
#' known Concentration C0 at the lower end of the step.
#'
#' @param df (dataframe) The input dataframe, sorted lowest depth to highest,
#' containg the following columns: \n
#' height: the height of each step in m
#' prod: the production assinged to this step
#' DS: the diffusion coefficient DS
#' @param F0 (numeric) Incoming flux to lowest step in mumol/s/m^2
#' @param C0 (numeric) Concentration at the lower end of the lowest step.
#' @examples prod_mod(df,F0=0,C0=1200)
#'

#' @export

prod_mod <- function(df,F0,C0){
  df$flux <- cumsum(with(df,flux_prod(prod, height,0)))+F0
  df$conc <- cumsum(with(df,conc_prod(prod, DS,lag(flux,1,0), 0,height)))+C0
  return(df)
}
prod_mod2 <- function(df,F0,C0){
  df$flux <- cumsum(with(df,flux_prod(prod, height,0)))+F0
  conc <- cumsum(with(df,conc_prod(prod, DS,lag(flux,1,0), 0,height)))+C0
  return(conc)
}
prod_mod3 <- function(df,F0,C0){
  flux <- cumsum(with(df,flux_prod(prod, height,0)))+F0
  conc <- cumsum(with(df,conc_prod(prod, DS,lag(flux,1,0), 0,height)))+C0
  return(conc)
}
prod_mod4 <- function(prod,height,DS,F0,C0){
  flux <- cumsum(flux_prod(prod, height,0))+F0
  conc <- cumsum(conc_prod(prod, DS,lag(flux,1,0), 0,height))+C0
  return(conc)
}
prod_mod5 <- function(prod,height,DS,F0,C0){
  flux <- cumsum(flux_prod(prod, height,0))+F0
  conc <- cumsum(conc_prod(prod, DS,c(0,flux[-length(flux)]), 0,height))+C0
  return(conc)
}
prod_mod6 <- function(prod,height,DS,F0,C0){
  flux <- cumsum(((prod) * height))+F0
  conc <- cumsum(-((prod)/(2*DS)) *height^2  - c(0,flux[-length(flux)])/DS *height)+C0
  return(conc)
}
prod_mod_conc <- function(prod,height,DS,F0,C0){
  flux <- cumsum(((prod) * height))+F0
  conc <- cumsum(-((prod)/(2*DS)) *height^2  - c(0,flux[-length(flux)])/DS *height)+C0
  return(conc)
}
prod_mod_flux <- function(prod,height,F0){
  flux <- cumsum(((prod) * height))+F0
  return(flux)
}
#' @export
conc_prod<-function(prod,DS,F0,C0,dz){
  conc <- -((prod)/(2*DS)) *dz^2  - F0/DS *dz + C0
}
#' @export

flux_prod <- function(prod,dz,F0){
  flux <- ((prod) * dz + F0 )
}
