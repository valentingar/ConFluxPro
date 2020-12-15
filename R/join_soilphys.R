#' @title join_soilphys
#'
#' @description A helper function to create the soilphys-dataframe from source-files. Calls discretize_depth with specified parameters.
#'
#' @param param_map (dataframe) containing the following columns: \n
#'  param = the name of the parameter to be obtained \n
#'  name = the column name of that parameter in the source data-frame (optional). \n
#'  source = the source dataframe (must mach name given in further arguments) \n
#'  method = a valid interpolation method of discretize_depth \n
#' @param depth_target (named list, numeric) A named list, that contains the target depths of each Plot  \n
#' (Including upper and lower boundary and each intersection in between.).
#' @param id_cols (named list, character) A named list, specifying the id columns uniquely identifying a profile to be interpolated
#' for each source dataframe.
#' @param ... source dataframes, must be named. Names must match the ones given in param map. \n
#' See example below for details
#'
#' @return
#' @examples
#'
#' @import dplyr
#'
#' @export

join_soilphys <- function(...){

  #create list of source dataframes
  data_sources <- list(...)

  #Stop if duplicates in data_sources
  if(!length(unique(names(data_sources)))==length(unique(data_sources))){
    stop("Data sources are not uniquely named! Check spelling.")
  }

  #All unique sources given in param_map
  param_sources <- unique(param_map$source)

  #Which sources of param_map match sources in data_sources?
  sources_present <- param_sources %in% names(data_sources)

  #stop if not all sources are present.
  if(!all(sources_present)){
    stop(paste0("The following sources cannot be found, but are given in param_map.
                Please add the following dataframes to the function call: ",paste0(param_sources[!sources_present],collapse = " "),collapse = " "))
  }

}
