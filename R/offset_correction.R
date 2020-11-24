#' @title offset_correction
#'
#' @description A function for the correction of offsets in the whole dataset.
#' The basic assumption is that atmospheric values of these gases are fairly constant over long periods of time.
#' If there are offsets in the atmospheric concentrations, then these are probably introduced via calibration differences
#' or similar factors whithin the chemical analysis.
#'
#' The time series is smoothed by subdividing the whole dataset per gas into homogeneous subsections (section).
#' Mean values of the atmospheric concentrations are then calculated, or a linear model is fit against time.
#' A correction factor is then calculated by dividing a target atmospheric value (gases_std) by the mean or the values predicted by the linear model.
#' NRESULT_ppm values are then multiplied with the factor.
#'
#'
#' @param df (dataframe) The gasdata-dataframe.
#'  @param corr_map (dataframe) A dataframe of four columns ("gas","SAMPLE_NO","section","mode") mapping each pair of gas (gas) and sample-id (SAMPLE_NO) of the gasdata dataframe to a subsection-number (section).
#' Finally, mode determines, whether the correction is performed using a linear regression, to account for drifts in time ("lin") or using the mean ("const").
#' This dataframe can be created using the helper-function offset_subsetting(), or using the included shiny-app offset_app().
#' @param gases (vector, character) A vector containing the names of the gases to be corrected. Spelling must match gasdata$gas.
#' @param gases_std (vector, numeric) A numeric vector with standard concentrations of the gases in the atmosphere. Unit is fraction of Volume (e.g. N2 = 0.78). Must match the order of gases.
#' @param depth_cal (character) A character (-vector) containing the value(s) of "depth_cat" of the gasdata dataframe to be used for the calculation. Should be the air concentration.
#'
#' @return df (dataframe)
#'
#' @examples
#'
#' @import dplyr
#'
#' @export


offset_correction <- function(df,
                              corr_map,
                              gases,
                              gases_std,
                              depth_cal){

un_mod <- unique(na.omit(corr_map$mode))
un_mod <- un_mod[!un_mod %in% c("lin","const")]
if (length(un_mod)>0){
  stop(paste("wrong mode in corr_map:",un_mod ))
}

corr_map <- corr_map %>%
  dplyr::mutate(SAMPLE_NO = as.character(SAMPLE_NO))

if(any(is.na(corr_map$mode))){
  warning("NAs in corr_map$mode. Not changing these samples.")
}


gas_map <- data.frame(gas = gases, corr_std = gases_std)

df<-df %>%
  dplyr::filter(depth_cat %in% !!depth_cal) %>%
  dplyr::left_join(corr_map) %>%
  dplyr::filter(is.na(section)==F) %>%
  dplyr::filter(is.na(NRESULT_ppm)==F) %>%
  dplyr::group_by(gas,section) %>%
  dplyr::group_modify(~{
  if(is.na(.x$mode[1])){
    med = 1
    grad = 0
  }else  if(.x$mode[1] == "const"){
      med = median(.x$NRESULT_ppm,na.rm=T)
      grad = 0
  } else if (.x$mode[1]=="lin"){
    if (.x %>% dplyr::filter(!is.na(Date),!is.na(NRESULT_ppm)) %>% nrow() <2){
      stop(paste("section",.y$section[1],"does not have enough non-NA cases (>=2)"))
    }
    mod <- lm(NRESULT_ppm ~ Date,data = .x)
    med <- coef(mod)[1]
    grad <- coef(mod)[2]
  }
    return(.x %>% dplyr::mutate(corr_med = med,corr_grad = grad))
  }) %>%
  dplyr::left_join(gas_map) %>%
  dplyr::mutate(corr_fac = corr_std*10^6 /(corr_med + corr_grad * as.numeric( Date))) %>%
  dplyr::select(contains(c("Date","Plot","gas","corr_fac"))) %>%
  dplyr::right_join(df) %>%
  dplyr::mutate(corr_fac = ifelse(is.na(corr_fac),1,corr_fac)) %>%
  dplyr::mutate(NRESULT_ppm = NRESULT_ppm * corr_fac)


return(df)
}
