#' @title complete_soilphys
#'
#' @description This function completes the soilphys dataset by calculating different parameters if necessary, as long as
#' all required parameters are available. Diffusion coefficients, as well as the air density are calculated if missing.
#'
#' @param soilphys (dataframe) the soilphys dataframe
#' @param DSD0_formula (character) A character vector defining the way DSD0 should be calculated.
#' Must refer to existing columns in soilphys. See examples below.
#' @param gases (character) A character vector defining the gases for which to calculate D0 and DS.
#' @param overwrite (logical) If true, already existing columns are overwritten.
#'
#' @examples soilphys <- complete_soilphys(soilphys,DSD0 = "a*AFPS^b")
#'
#' @import dplyr
#' @import rlang
#'
#' @seealso D0_massman
#'
#' @family soilphys
#' @export
#'

complete_soilphys <- function(soilphys,
                              DSD0_formula="a*AFPS^b",
                              gases,
                              overwrite = F){
df_names <- names(soilphys)
if (all(c("depth","upper","lower","Date","Plot","TPS","SWC","Temp","p") %in% df_names)==F){
  stop("there are essential parameters missing. please run check_soilphys()")
}

AFPS_flag <- !("AFPS" %in% df_names)
DSD0_flag <- !("DSD0" %in% df_names)
DS_flag <- !("DS" %in% df_names)
D0_flag <- !("D0" %in% df_names)
rho_air_flag <- !("rho_air" %in% df_names)

if (AFPS_flag == T | overwrite == T ){
  soilphys <- soilphys %>%
    dplyr::select(!dplyr::any_of("AFPS")) %>%
    dplyr::mutate(AFPS = TPS-SWC)
}
if (DSD0_flag == T | overwrite == T){

  soilphys <- soilphys %>%
    dplyr::select(!dplyr::any_of("DSD0")) %>%
    dplyr:: mutate(DSD0 = !!(rlang::parse_expr(DSD0_formula)))
}
if (D0_flag == T | overwrite == T){
  print("starting D0 calculation. This may take a few seconds.")
  soilphys <- lapply(gases, function(gas){
    return(soilphys %>% mutate(gas = !!gas))
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::select(!dplyr::any_of("D0")) %>%
    dplyr::mutate(D0 = D0_massman(gas,Temp,p))

}
if(DS_flag == T | overwrite == T){
  soilphys <-soilphys %>%
    dplyr::select(!dplyr::any_of("DS")) %>%
    dplyr::mutate(DS = DSD0*D0)
}
if(rho_air_flag == T | overwrite == T){
  soilphys <-soilphys %>%
    dplyr::select(!dplyr::any_of("rho_air")) %>%
    dplyr::mutate(rho_air = p*100 / (8.314 * (273.15+Temp)))
}
print("The following columns were added:")
print(paste0(c("AFPS","DSD0","D0","DS","rho_air")[c(AFPS_flag,DSD0_flag,D0_flag,DS_flag,rho_air_flag)==T],collapse = " "))
if(overwrite == T){
  print("The following columns were overwritten:")
  print(paste0(c("AFPS","DSD0","D0","DS","rho_air")[c(AFPS_flag,DSD0_flag,D0_flag,DS_flag,rho_air_flag)==F],collapse = " "))

}
return(soilphys)
}

