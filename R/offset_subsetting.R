#' @title offset_subsetting
#'
#' @description This function helps to create the corr_map dataframe for the offset correction.
#' Based on the Date input, it creates a new section variable and maps it to all SAMPLE_NO.
#'
#'
#'
#' @param df (dataframe) The gasdata-dataframe.
#'
#' @param gas (character) A character defining the gas to be corrected.
#'
#' @param depth_cal (character (-vector)) A character (-vector) of the same
#'   depth categories (depth_cat) used in offset_correction(), i.e. the values
#'   of "depth_cat" that correspond to atmospheric concentration.
#'
#' @param start (vector, Date) A vector of type Date that specifies the starting
#'   date of each section (the starting date is included in the section!).
#'
#' @param end (vector, Date) A vector of type Date that specifies the end date
#'   of each section (the end date is included in the section!). The start date
#'   of element i should be the end date of (i-1) +1 day.
#'
#' @param mode (vector, character) A vector of type character that specifies the
#'   correction method to be used in each section. Valid methods are "const",
#'   here the median value is being used as a constant factor, and "lin", where
#'   a linear model is fit against time, to account for an temporal drift.
#'
#' @return df (dataframe) The corr_map dataframe used in the offset_correction() function
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
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @family gasdata
#'
#'
#' @seealso offset_correction
#' @seealso offset_override
#'
#' @export


offset_subsetting <- function(df,
                              gas,
                              depth_cal,
                              start,
                              end,
                              mode) {

  date_map <- data.frame(start_date = start,
                         end_date = end) %>%
    dplyr::mutate(int = lubridate::interval(start, end)) %>%
    dplyr::mutate(section = seq(mode))

  mode_selector <- function(section) {
    mod <- mode[section]
    return(mod)
  }

  corr_map <- df %>% dplyr::filter(gas == !!gas) %>%
    dplyr::select(Date, SAMPLE_NO) %>%
    dplyr::mutate(section = unlist(lapply(Date, function(d) {
      res <- which(d %within% date_map$int)
      if (length(res) == 0) {
        res <- NA
      }
      return(res)
    }))) %>%
    dplyr::mutate(mode = mode_selector(section))
  return(corr_map)
}
