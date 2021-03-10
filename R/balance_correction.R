#' @title balance_correction
#'
#' @description A function to correct the measured values for non-complete gas exchange.
#' Per sample, a total balance (b_tot) is obtained by adding the measurements of all gases and, if necessary,
#' corrected for missing gases. \n
#' Theoretically, bal is between (0;1), however values over 1 can result from calibration errors.
#' Values over 1 are treated the same and are corrected.
#' Then, each NRESULT_ppm is corrected: NRESULT_ppm / b_tot
#'
#' @param df (dataframe) The gasdata-dataframe. The dataframe will be altered in the process of the
#' function, so that it has to be overwritten (see examples below).
#'
#' @param limits (vector, numeric) A vector of two that contains the upper and lower limits for b_tot, above and below which NRESULT_ppm are flagged and set NA, if set_na is TRUE.
#'
#' @param gases (vector, character) A character vector of the gases that should be used in the balance approach.
#' Default are the four most abundant atmospheric gases (N2, O2, Ar, CO2).
#' Spelling must match the spelling of the gas-column in gasdata exactly.
#'
#' @param gases_std (vector, numeric) A numeric vector of standard values to be used for
#' missing gases as fraction of total volume. Values of b_tot recalculated to account for
#' the missing gases based on the gases present and these standard values. This is achieved by assuming a balance of
#' the gases present first and recalculating gases_st accordingly. \n
#' Order must match input of gases. Defaults to c(0.78084,0.20946,0.009340,0.0407))
#'
#' @param gases_ob (vector, character) A vector of obligatory gases that must be present for correct
#' balance calculation. NRESULT_ppm of samples missing any of these gases will be flagged in bal_flag
#' and not corrected or set NA. Defaults to c("N2","O2").
#'
#' @param set_na (logical) Should flagged values be set to NA (bal_flag == T)? Default is F.
#'
#' @return gasdata (dataframe) With added columns
#' @return bal = balance
#' @return bal_flag  TRUE if value was not corrected (or set NA).
#' @return NRESULT_ppm, corrected for balance
#'
#' @family gasdata
#'
#' @examples
#' gasdata <- balance_correction(gasdata,
#'                               limits = c(0.6,1.05),
#'                               gases = c("N2","O2","Ar"),
#'                               gases_std = c(0.78,0.2,0.0093),
#'                               gases_ob = c("N2","O2"),
#'                               set_na = T
#'                               )
#'
#'
#' @import dplyr
#'
#' @export

balance_correction <- function(df,
                               limits = c(-999,+999),
                               gases  = c("N2","O2","Ar","CO2"),
                               gases_std = c(0.78084,0.20946,0.009340,0.0407),
                               gases_ob = c("N2","O2"),
                               set_na = F
                               ){

  #stop-points for wrong input
if (!length(limits) ==2){
  stop("Please set upper and lower limits or leave out.")
}
if(!length(gases) == length(gases_std)){
  stop("lengths of 'gases' and 'gases_std' do not match!")
}
if (is.logical(set_na)==F){
  stop("set_na must be logical.")
}
if (!all(gases_ob %in% gases)){
  stop("'gases_ob' contains entries not present in 'gases'")
}
gases_present <- gases %in% unique(df$gas)
if (!all(gases_present)){
  stop(paste0("'gases' contains entries not present in the dataframe:",paste0(gases[gases_present == F],collapse = ", "),collapse=" "))
}



#Adding commas to necessary gases, to prevent finding N2 in N2O
gases_ob <- unlist(lapply(gases_ob,function(g) paste0(c("",g,""),collapse = ",")))

# Function to correct bal with standard values of missing gases.
bal_corr <- function(bal,missing_gas){
  bal_corrected <- unlist(lapply(1:length(bal),function(i){
    gas_corr<-gases_std[gases %in% unlist(strsplit(missing_gas[i],split = ","))]
    if (length(gas_corr>0)){
      bal_corrected <- bal[i]/(1-sum(gas_corr))
    } else {
      bal_corrected<-bal[i]
    }
    return(bal_corrected)
  }))
  return(bal_corrected)
}

df <- df %>%
  dplyr::filter(gas %in% !!gases) %>% #Only gases declared in function are used
  dplyr::group_by(SAMPLE_NO) %>%
  dplyr::arrange(gas) %>%
  dplyr::summarise(bal = sum(NRESULT_ppm/10^6,na.rm = T), #bal
            n_bal = length(na.omit(NRESULT_ppm)), #number of counted gases
            missing_gas = paste(c("",gases[-match(gas[is.na(NRESULT_ppm)==F],!!gases)],""),collapse = ",")) %>% #Character string with missing gases, comma separated to discern between N2 and N2O, O2 and CO2
  dplyr::mutate(bal_flag = grepl(paste0(gases_ob,collapse="|"),missing_gas)) %>% #TRUE if any of gases_ob are missing in "bal"
  dplyr::mutate(bal = bal_corr(bal,missing_gas)) %>% # correcting bal with standard values for missing gases
  dplyr::mutate(bal_flag = ifelse(bal < min(limits) | bal > max(limits),T,bal_flag)) %>% #bal_flag also true if bal exceeds limits
  dplyr::mutate(bal_flag = ifelse(bal == 0,T,bal_flag)) %>%
  dplyr::right_join(df) %>% #joining with gasdata
  dplyr::mutate(bal_flag = ifelse(is.na(bal_flag),T,bal_flag)) #if after the join a sample_no was not met in gasdata, bal_flag is na and thus is set T


#CORRECTION OF THE MOLE RATIOS for bal_flag = T
if(set_na == T){
  #if set_na == T, bal_flag == T NRESULT_ppm is set NA
  df <- df %>% dplyr::mutate(NRESULT_ppm = ifelse(bal_flag == F, NRESULT_ppm / bal,NA))
} else {
  #otherwise NRESULT_ppm is not changed
  df <- df %>% dplyr::mutate(NRESULT_ppm = ifelse(bal_flag == F, NRESULT_ppm / bal,NRESULT_ppm))
}

if(any(df$bal_flag)){
  warning(paste0("Some of the entries have been flagged (bal_flag) and ",ifelse(set_na,"been set to NA!","were not corrected (old values remain)!")," Please check these values manually",collapse = ""))
}


 return(df)
}
