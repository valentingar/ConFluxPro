#' @title Calculate D0
#'
#' @description This function calculates the free-air diffusion coefficients
#' of different gases for a given temperature and pressure.
#'
#'
#'
#' @param gas (character) One of "CO2","CH4","N2O","O2","N2"
#' @param t (numeric) temperature in °C
#' @param p (numeric) pressure in hpa
#'
#' @return D0 in m^2/s
#' @references Massman, W. J. A review of the molecular diffusivities of H2O,
#'   CO2, CH4, CO, O3, SO2, NH3, N2O, NO, and NO2 in air, O2 and N2 near STP.
#'   Atmospheric Environment 1998, 32(6), 1111–1127
#' @export

D0_massman <- function(gas, t, p){
  valid_gases <- c("CO2","CH4","N2O","O2","N2")
  if(all(gas %in% valid_gases) == F){
    stop(paste0(c("wrong gas: >",gas,"<! choose one of",valid_gases),collapse = " "))
  }

  a <- c(1.381*1e-5,1.952*1e-5,1.436*1e-5,1.820*1e-5,1.820*1e-5)
  b <- c(1.81,1.81,1.81,1.81,1.81)
  D0 <- unlist(lapply(1:length(t),function(i){
    D0 <- a[match(gas[i],valid_gases)]*((t[i]+273.15)/273.15)^b[match(gas[i],valid_gases)]*(1013/p[i])
  return(D0)
    }))
  return(D0)
}
