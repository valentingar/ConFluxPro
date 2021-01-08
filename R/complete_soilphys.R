#' @title complete_soilphys
#'
#' @description This function completes the soilphys dataset by calculating different parameters if necessary, as long as
#' all required parameters are available.
#'
#' @param soilphys (dataframe) the soilphys dataframe
#' @param DSD0_formula (character) A character vector defining the way DSD0 should be calculated. Must refer to existing variables in soilphys. See examples below
#' @param gases (character) A character vector defining the gases for which to calculate D0 and DS.
#' @param overwrite (logical) If true, existing columns are overwritten.
#'
#' @examples soilphys <- complete_soilphys(soilphys,DSD0 = "a*AFPS^b")
#'
#' @import dplyr
#' @import rlang

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
  soilphys <- soilphys %>%
    dplyr::select(!dplyr::any_of("D0")) %>%
    dplyr::group_by(Plot,Date,depth) %>%
    dplyr::group_modify(~{
      D0 <- unlist(lapply(gases,function(g){
        D0 <- D0_massman(g,.x$Temp,.x$p)
        return(D0)
      }))
      .x <- cbind(.x,gas = gases,D0 = D0)
      return(.x)
    })

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

