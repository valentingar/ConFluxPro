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
#' @examples offset_override(corr_map = corr_map,
#' SAMPLE_NO = c(3023,5612,405,"blackforest27"),
#' section = c(6,2,9,3))
#'
#' @family gasdata
#' @import dplyr
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
