#' @title Check for complete and correct soil physical parameters
#'
#' @description This function analyses the soilphys dataframe before the flux
#'   calculation. It presents a warning, if there are variables missing and also
#'   looks for suspicious patterns that suggest an error in the interpolation
#'   made by discretize_depth. Mainly checks if certain columns are present
#'   and if they are missing, if they can be calculated from the data present.
#'   Looks for the following columns by default:
#'   "upper","lower","TPS","SWC","AFPS","t","p","DSD0","D0","DS"
#'
#'
#' @param df (dataframe) the soilphys dataframe
#'
#' @param extra_vars (character vector) column names of additional variables to
#' be checked.
#'
#' @param id_cols (character vector) the columns that, together, identify a
#' site uniquely (e.g. site, repetition)
#'
#' @returns data frame of 'suspicious' parameter/depth combinations, where all
#'   values are NA.
#'
#' @examples
#' check_soilphys(ConFluxPro::soilphys, id_cols = c("site", "Date"))
#'
#'
#' @family soilphys
#'
# @import tidyr
#' @importFrom rlang .data
#'
#' @export

check_soilphys <-function(df,
                          extra_vars=c(),
                          id_cols){

  df_names <- names(df)

  #defining a vector of obligatory parameter names
  param_names <-  c("upper","lower","TPS","SWC",
                    "AFPS","t","p","DSD0","D0","DS",extra_vars,id_cols)

  #missing parameters in df
  param_missing <- param_names[!param_names %in% df_names]

  #checking for derived parameters and their obligatory predecessors
  to_fix <- c()
  not_to_fix <- c()
  if (("AFPS" %in% param_missing)){
    if ((!"TPS" %in% param_missing) & (!"SWC" %in% param_missing)){
    to_fix<-c(to_fix,"AFPS")
  }else {
    not_to_fix <- c(not_to_fix,"AFPS")
  }
  }
  if ("DSD0" %in% param_missing){
    if("AFPS" %in% not_to_fix){
      not_to_fix <- c(not_to_fix,"DSD0","D0","DS")
    } else {
      to_fix <- c(to_fix,"DSD0")
    }
  }
  if ("D0" %in% param_missing){
    if("p" %in% param_missing | "t" %in% param_missing){
      not_to_fix <- c(not_to_fix,"D0","DS")
    } else{
      to_fix <- c(to_fix,"D0")
    }
  }
  if("DS" %in% param_missing){
    if("DSD0" %in% not_to_fix | "D0" %in% not_to_fix){
      not_to_fix <- c(not_to_fix,"DS")
    }
    else {
      to_fix <- c(to_fix,"DS")
    }
  }

  params_grouping <- param_names[!param_names == "upper"]

  #checking for suspicious NAs
  susp <-df %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(id_cols, "upper")))) %>%
    dplyr::mutate(dplyr::across(
      dplyr::any_of(params_grouping),
      is.na),
      .groups = "keep") %>%
    dplyr::select(dplyr::any_of(c(params_grouping, id_cols, "upper"))) %>%
    dplyr::summarise(dplyr::across(
      dplyr::any_of(params_grouping),
      all
      )
      )

  #finding column names of suspects
  class_susp <- vapply(susp, class, FUN.VALUE = character(1))
  is_susp <- class_susp == "logical"
  pars_susp <- names(susp)[is_susp]

  if(length(pars_susp)>0){
  susp <-
    susp %>%
    tidyr::pivot_longer(cols = {pars_susp},
                        names_to = "param",
                        values_to = "value") %>%
    dplyr::filter(.data$value)

  if(anyNA(df$upper)){
    susp <- susp %>%
      dplyr::add_row(param = "upper",
                     value = TRUE)
  }

  } else {
  susp <- NA
}




  green<-function(txt){
    return(paste0("\033[0;",32, "m",txt,"\033[0m"))
  }
  red<-function(txt){
    return(paste0("\033[0;",31, "m",txt,"\033[0m"))
  }
  yellow<-function(txt){
    return(paste0("\033[0;",33, "m",txt,"\033[0m"))
  }



  sp_ready <- ifelse(length(param_missing) == 0, TRUE, FALSE)
  sp_fixable <- ifelse(length(param_missing) == length(to_fix), TRUE, FALSE)


  out_message <-
    paste0(
    "--------------------------------------------------------",
    "\n",
    "your soilphys-dataframe is", "\n",
    ifelse(sp_ready == TRUE, green("ready"), red("!!not ready!!")),
    "\n")
  if (sp_ready == FALSE) {
    out_message <- paste0(
      out_message,
      "the dataframe ",
      ifelse(sp_fixable == TRUE, green("can"), red("cannot")) ,
      " be fixed by complete_soilphys()",
      "\n"
    )
  }
  if (sp_fixable == FALSE) {
    out_message <-
      paste0(
        out_message,
        "please provide the following parameters to the dataframe first:",
        "\n",
        paste0(param_missing[!param_missing %in% c(to_fix, not_to_fix)],
               collapse = " , "),
        "\n"
      )
  } else {
    out_message <-
      paste0(
        out_message,
        "the following parameters are still missing: ",
        "\n",
        paste0(param_missing, collapse = " , "),
        "\n",
        "please note that for DSD0 calculation,
        there may be individual prerequesits of the
        fitting parameters you applied. (If so: provide extra_vars =
        c('my variable'))",
        "\n"
      )
  }
  if (is.data.frame(susp) && nrow(susp)>0) {
    out_message <-
      paste0(
        out_message,
        "\n",
        yellow(
          paste0(
            "the following parameters have depths with all NA",
            "\n",
            "please check if they were discretized correctly",
            "\n"
          )
        ))
  }
  out_message <-
    paste0(
      out_message,
    "--------------------------------------------------------",
    "\n"
  )
  message(out_message)
list(result = sp_fixable,
     suspects = susp)
}




