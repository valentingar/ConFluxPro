#' @title offset_override
#'
#' @description This function helps overriding the Date-based subsetting of
#'   offset_subsetting(). It modifies an existing corr_map by editing the
#'   section of a list of SAMPLE_NO. Check the section numbering of corr_map
#'   first. If a new section is manually introduced this way, a mode will have
#'   to be added manually.
#'
#'
#' @param corr_map (dataframe) The corr_map dataframe created by
#'   offset_subsetting
#'
#' @param SAMPLE_NO (character) A vector of the SAMPLE_NO to change section
#'
#' @param section (vector, numeric) A vector in the same order as SAMPLE_No
#'   specifying the target section.
#'
#' @return corr_map (dataframe) The corr_map dataframe used in the
#'   offset_correction function with overridden SAMPLE_NO.
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
#' new_section <-
#'   cmap$SAMPLE_NO[cmap$Date == "2021-01-01"]
#'
#' cmap <-
#'   offset_override(cmap,new_section,2)
#'
#' cmap$mode[cmap$section==2] <- "const"
#'
#' offset_correction(gasdata,
#'                   corr_map = cmap,
#'                   gases = "CO2",
#'                   gases_std = 400,
#'                   depth_cal = "HU")
#'
#' }
#'
#' @family gasdata
#'
#' @import dplyr
#'
#' @seealso offset_subsetting
#' @seealso offset_correction
#' @export


offset_override <- function(corr_map,
                            SAMPLE_NO,
                            section) {
  if (!length(SAMPLE_NO) == length(section)) {
    if (length(section) > 1) {
      stop("section has wrong dimension. Must be same length as SAMPLE_NO or 1")
    } else {
      section <- rep(section, length(SAMPLE_NO))
    }
  }


  mode_map <- corr_map %>%
    dplyr::select(section, mode) %>%
    dplyr::distinct()

  corr_map$section <- unlist(lapply(1:nrow(corr_map), function(i) {
    s <- corr_map$SAMPLE_NO[i]
    sec <- section[match(s, SAMPLE_NO)]
    if (is.na(sec) == T) {
      sec = corr_map$section[i]
    }
    return(sec)
  }))

  corr_map <- corr_map %>% select(-contains("mode")) %>%
    left_join(mode_map)
  return(corr_map)
}
