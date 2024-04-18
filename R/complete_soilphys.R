#' @title complete_soilphys
#'
#' @description This function completes the soilphys dataset by calculating
#'   different parameters if necessary, as long as all required parameters are
#'   available. Diffusion coefficients, as well as the air density are
#'   calculated if missing.
#'
#' @param soilphys (dataframe) the soilphys dataframe
#' @param DSD0_formula (character) A character vector defining the way DSD0
#'   should be calculated. Must refer to existing columns in soilphys. See
#'   examples below.
#' @param gases (character) A character vector defining the gases for which to
#'   calculate D0 and DS.
#' @param overwrite (logical) If true, already existing columns are overwritten.
#'
#' @examples soilphys <- complete_soilphys(soilphys,DSD0 = "a*AFPS^b")
#'
#'
#' @seealso D0_massman
#'
#' @family soilphys
#' @export
#'

complete_soilphys <- function(soilphys,
                              DSD0_formula = NULL,
                              gases,
                              overwrite = TRUE){
df_names <- names(soilphys)
if (all(c("depth","upper","lower","TPS","SWC","t","p") %in% df_names)==F){
  stop("there are essential parameters missing. please run check_soilphys()")
}

stopifnot("Must provide a valid DSD0_formula!" = (!is.null(DSD0_formula)))

gas_present_flag <- "gas" %in% names(soilphys)
AFPS_flag <- !("AFPS" %in% df_names)
DSD0_flag <- !("DSD0" %in% df_names)
DS_flag <- !("DS" %in% df_names)
D0_flag <- !("D0" %in% df_names)
c_air_flag <- !("c_air" %in% df_names)

if (AFPS_flag == TRUE | overwrite == TRUE ){
  soilphys$AFPS <- soilphys$TPS - soilphys$SWC
}
if (DSD0_flag == TRUE | overwrite == TRUE){

  soilphys <- soilphys %>%
    dplyr::select(!dplyr::any_of("DSD0")) %>%
    dplyr::mutate(DSD0 = !!(rlang::parse_expr(DSD0_formula)))

  if (any_negative_values(soilphys$DSD0)){
    message("Negative DSD0 calculated, setting NA!")
    soilphys$DSD0[soilphys$DSD0 < 0] <- NA
  }
}
if (D0_flag == TRUE | overwrite == TRUE){

  # if gases is already present, no need to apply new ones!
  if(gas_present_flag){
    message("gas-column found in soilphys, ignoring 'gases' argument.")
  } else {

    #otherwise: for each row, add new gases
    soilphys <- soilphys %>%
      dplyr::cross_join(data.frame(gas = gases))
  }

  # then calculate D0 based on that.
  soilphys$D0 <- D0_massman(soilphys$gas,
                             soilphys$t,
                             soilphys$p)

  if (any_negative_values(soilphys$D0)){
    message("Negative D0 calculated, setting NA!")
    soilphys$D0[soilphys$D0 < 0] <- NA
  }

}

if(DS_flag == TRUE | overwrite == TRUE){
  soilphys$DS <- soilphys$DSD0 * soilphys$D0

  if (any_negative_values(soilphys$DS)){
    message("Negative DS calculated, setting NA!")
    soilphys$DS[soilphys$DS < 0] <- NA
  }

}

if(c_air_flag == TRUE | overwrite == TRUE){
  soilphys$c_air <-  (soilphys$p*100 / (8.314 * (273.15 + soilphys$t)))

  if (any_negative_values(soilphys$c_air)){
    message("Negative c_air calculated, setting NA!")
    soilphys$c_air[soilphys$c_air < 0] <- NA
  }
}

message("The following columns were added:")
message(paste0(c("AFPS",
                 "DSD0",
                 "D0",
                 "DS",
                 "c_air")[c(AFPS_flag,
                            DSD0_flag,
                            D0_flag,
                            DS_flag,
                            c_air_flag) == TRUE],
               collapse = " "))
if(overwrite == TRUE){
  message("The following columns were overwritten:")
  message(paste0(c("AFPS",
                   "DSD0",
                   "D0",
                   "DS",
                   "c_air")[c(AFPS_flag,
                              DSD0_flag,
                              D0_flag,
                              DS_flag,
                              c_air_flag) == FALSE],
                 collapse = " "))

}
soilphys
}

