#' @title offset_correction
#'
#' @description A function for the correction of offsets in the whole
#'   gas-dataset. The basic assumption is that atmospheric values of these gases
#'   are fairly constant over long periods of time. If there are offsets in the
#'   atmospheric concentrations, then these are probably introduced via
#'   calibration differences or similar factors within the gas analysis. The
#'   time series is smoothed by subdividing the whole dataset per gas into
#'   homogeneous subsections (section). Mean values of the atmospheric
#'   concentrations are then calculated, or a linear model is fit against time.
#'   A correction factor is then calculated by dividing a target atmospheric
#'   value (gases_std) by the mean or the values predicted by the linear model.
#'   NRESULT_ppm values are then multiplied by that factor.
#'
#'
#' @param df (dataframe) The gasdata-dataframe.
#' @param corr_map (dataframe) A dataframe of four columns
#'   ("gas","SAMPLE_NO","section","mode") mapping each pair of \code{gases} and
#'   sample-id \code{SAMPLE_NO} of the \code{gasdata} data.frame to a subsection-number
#'   (section). The column "mode" determines, whether the correction is
#'   performed using a linear regression, to account for drifts in time ("lin")
#'   or using the mean ("const"). This dataframe can be created using the
#'   helper-function offset_subsetting()
#'
#' @param gases (vector, character) A vector containing the names of the gases
#'   to be corrected. Spelling must match gasdata$gas.
#' @param gases_std (vector, numeric) A numeric vector with standard
#'   concentrations of the gases in the atmosphere. Unit is fraction of Volume
#'   (e.g. N2 = 0.78). Must match the order of "gases".
#' @param depth_cal (character) A character (-vector) containing the value(s) of
#'   "depth_cat" of the gasdata dataframe to be used for the calculation of the
#'   correction factor. Should be the depth_cat of the air concentration.
#'
#' @return df (dataframe)
#'
#' @examples {
#' data("gasdata")
#'
#' library(dplyr)
#'
#' gasdata <- gasdata %>%
#'   mutate(depth_cat = ifelse(depth>0,"HU","MIN"),
#'          SAMPLE_NO = row_number())
#'
#' cmap <-
#'   offset_subsetting(gasdata,
#'                     gas = "CO2",
#'                     depth_cal = "HU",
#'                     start = "2021-01-01",
#'                     end = "2022-01-01",
#'                     mode = "const")
#'
#' offset_correction(gasdata,
#'                   corr_map = cmap,
#'                   gases = "CO2",
#'                   gases_std = 400,
#'                   depth_cal = "HU")
#'
#' }
#'
#' @import dplyr
#' @importFrom lubridate %within%
#'
#' @family gasdata
#' @export


offset_correction <- function(df,
                              corr_map,
                              gases,
                              gases_std,
                              depth_cal){

un_mod <- unique(stats::na.omit(corr_map$mode))
un_mod <- un_mod[!un_mod %in% c("lin","const")]
if (length(un_mod)>0){
  stop(paste("wrong mode in corr_map:",un_mod ))
}


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
