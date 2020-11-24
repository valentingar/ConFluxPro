#' @title D0_Massman
#'
#' @description This function calculates the Diffusion coefficients of different gases in air at set
#' temperature and pressure. The functions are taken from Massman 1998 "A REVIEW OF THE MOLECULAR DIFFUSIVITIES
#' H2O, CO2, CH4, CO, O3, SO2, NH4, N2O, NO, AND NO2 IN AIR, O2 AND N2 NEAR STP".
#'
#'
#'
#' @param gas (character) One of "CO2","CH4","N2O","O2","N2"
#' @param t (numeric) temperature in °C
#' @param p (numeric) pressure in hpa
#'
#' @return D0 in µmol/m ??
#'
#' @export

D0_massman <- function(gas, Temp, p){
  valid_gases <- c("CO2","CH4","N2O","O2","N2")
  if((gas %in% valid_gases) == F){
    stop(paste0(c("wrong gas: >",gas,"<! choose one of",valid_gases),collapse = " "))
  }

  a <- c(1.369*1e-5,1.952*1e-5,1.436*1e-5,1.820*1e-5,1.820*1e-5)
  b <- c(1.9206,1.81,1.81,1.81,1.81)

  D0 <- a[match(gas,valid_gases)]*((Temp+273.15)/273.15)^b[match(gas,valid_gases)]*(1013/p)
  return(D0)
}
