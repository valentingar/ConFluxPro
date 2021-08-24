#' @title check_soilphys
#'
#' @description This function analyses the soilphys dataframe before the flux
#'   calculation. It presents a warning, if there are variables missing and also
#'   looks for suspicious patterns that suggest an error in the interpolation
#'   made by discretize_depth. Mainly checks if ceratain columns are present
#'   and if they are missing, if they can be calcluated from the data present.
#'   Looks for the following columns by default:
#'   "upper","lower","TPS","SWC","AFPS","Temp","p","DSD0","D0","DS"
#'
#'
#' @param df (dataframe) the soilphys dataframe
#'
#' @param extra_vars (character vector) column names of additional variables to be
#'   checked.
#'
#' @param id_cols (character vector) the columns that, together, identify a
#' profile uniquely
#'
#' @return data frame of 'suspicious' parameter/depth combinations, where all
#'   values are NA.
#'
#' @examples {
#' data("soilphys")
#' check_soilphys(soilphys,id_cols = c("site","Date"))
#' }
#'
#' @family soilphys
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

check_soilphys <-function(df,
                          extra_vars=c(),
                          id_cols){

  df_names <- names(df)

  #defining a vector of obligatory parameter names
  param_names <-  c("upper","lower","TPS","SWC","AFPS","Temp","p","DSD0","D0","DS",extra_vars,id_cols)

  param_missing <- param_names[!param_names %in% df_names] #missing parameters in df

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
    if("p" %in% param_missing | "Temp" %in% param_missing){
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

  #checking for suspicious NAs
  susp <-df %>% dplyr::group_by(dplyr::across(dplyr::any_of({c(id_cols,"depth")}))) %>%
    dplyr::summarise(across(any_of(param_names),is.na)) %>%
    dplyr::summarise(across(any_of(param_names),all))

  #finding column names of suspects
  is_susp <- unlist(lapply(1:ncol(gasdata),function(i){
    is.logical(gasdata[1,i])
  }))
  pars_susp <- names(susp)[is_susp]

  if(length(pars_susp)>0){
  susp <-
    susp %>%
    tidyr::pivot_longer(cols = {pars_susp},
                        names_to = "param",
                        values_to = "value") %>%
    dplyr::filter(value == T)

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


  sp_ready <-ifelse(length(param_missing) == 0,T,F)
  sp_fixable <- ifelse(length(param_missing) == length(to_fix),T,F)


  cat(paste0("--------------------------------------------------------","\n"))
  cat(paste0("your soilphys-dataframe is","\n"),
      ifelse(sp_ready == T,green("ready"),red("!!not ready!!")),"\n")
  if(sp_ready == F){
  cat(paste0("the dataframe ",ifelse(sp_fixable == T,green("can"),red("cannot")) ," be fixed by complete_soilphys()","\n"))
  }
  if(sp_fixable ==F){
  cat(paste0("please provide the following parameters to the dataframe first:","\n",paste0(param_missing[!param_missing %in% c(to_fix,not_to_fix)],collapse = " , "),"\n"))
  } else {
  cat(paste0("the following parameters are still missing: ","\n",paste0(param_missing,collapse = " , "),"\n",
             "please note that for DSD0 calculation, there may be individual prerequesits of the fitting parameters you applied.","\n"))
  }
  if(!is.na(susp)){
  cat("\n")
  cat(yellow(paste0("the following parameters have depths with all NA","\n"," please check if they were discretized correctly","\n")))

  susp
  }
  cat(paste0("--------------------------------------------------------","\n"))

}




