#' @title balance_correction
#'
#' @description A function to correct the measured values for non-complete gas exchange.
#' Per sample, a total balance (b_tot) is obtained and, if necessary, corrected for missing gases.
#' Theoretically, b_tot is between (0;1), however values over 1 can result from calibration errors.
#' Values over 1 are treated the same and are corrected.
#' Then each NRESULT_ppm is corrected: NRESULT_ppm / b_tot
#' This function should be applied before the series_cleaner()-function.
#'
#' @param df (dataframe) The gasdata-dataframe. The dataframe will be altered in the process of the
#' function, so that it has to be overwritten (see examples below).
#'
#' @param limits (vector, numeric) A vector of two that contain the upper and lower limits for b_tot, above and below which NRESULT_ppm are set NA. Defaults to c(0.6,1.05).
#'
#' @param gases (vector, character) A character vector of the gases that should be used in the balance approach. Default are the four most abundant atmospheric gases.
#' Spelling must match the spelling of the gas-column in gasdata exactly. Defaults to c("N2","O2","Ar","CO2").
#'
#' @param gases_ob (vector, character) A vector of obligatory gases that must be present for correct balance calculation.
#' NRESULT_ppm of samples missing any of these gases will be flagged in b_flag. Defaults to c("N2","O2").
#'
#' @param gases_std (vector, numeric) A numeric vector of standard values to be used for missing gases as fraction of total volume.
#' Values are then recalculated to account for assumed balance based on present gases. Order must match input of gases. Defaults to c(0.78084,0.20946,0.009340,0.0407))
#'
#'
#' @return gasdata (dataframe) With added columns
#' @return b_tot = balance
#' @return b_flag  TRUE for values missing gases in gases_ob
#'
#' @examples
#' gasdata <- balance_correction(gasdata,
#'                               limits = c(0.6,1.05),
#'                               gases = c("N2","O2","Ar"),
#'                               gases_ob = c("N2","O2"),
#'                               gases_std = c(0.78,0.2,0.0093)
#'                               )
#'
#'
#' @import dplyr
#'
#' @export

balance_correction <- function(df,
                               limits = c(0.6,1.05),
                               gases  = c("N2","O2","Ar","CO2"),
                               gases_ob = c("N2","O2"),
                               gases_std = c(0.78084,0.20946,0.009340,0.0407)
                               ){
#Adding commas to necessary gases, to prevent finding N2 in N2O
gases_ob <- unlist(lapply(gases_ob,function(g) paste0(c("",g,""),collapse = ",")))

# Function to correct ges with standard values of missing gases.
ges_corr <- function(ges,missing_gas){
  ges_corrected <- unlist(lapply(1:length(ges),function(i){
    gas_corr<-gases_std[gases %in% unlist(strsplit(missing_gas[i],split = ","))]
    if (length(gas_corr>0)){
      ges_corrected <- ges[i]/(1-sum(gas_corr))
    } else {
      ges_corrected<-ges[i]
    }
    return(ges_corrected)
  }))
  return(ges_corrected)
}


df <- df %>%
  dplyr::filter(gas %in% !!gases) %>% #Only gases declared in function are used
  dplyr::group_by(SAMPLE_NO) %>%
  dplyr::arrange(gas) %>%
  dplyr::summarise(ges = sum(NRESULT_ppm/10^6,na.rm = T), #ges
            n_ges = length(na.omit(NRESULT_ppm)), #number of counted gases
            missing_gas = paste(c("",gases[-match(gas[is.na(NRESULT_ppm)==F],!!gases)],""),collapse = ",")) %>% #Character string with missing gases, comma separated to discern between N2 and N2O, O2 and CO2
  dplyr::mutate(ges_flag = grepl(paste0(gases_ob,collapse="|"),missing_gas)) %>% #TRUE if any of gases_ob are missing in "ges"
  dplyr::mutate(ges = ges_corr(ges,missing_gas)) %>% # correcting ges with standard values for missing gases
  dplyr::mutate(ges_flag = ifelse(ges < min(limits) | ges > max(limits),T,ges_flag)) %>% #ges_flag also true if ges exceeds limits
  dplyr::right_join(df) %>% #joining with gasdata
  dplyr::mutate(NRESULT_ppm = NRESULT_ppm / ges)  #CORRECTION OF THE MOLE RATIOS
 return(df)
}
