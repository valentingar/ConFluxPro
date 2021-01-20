#' @title discretize_depth
#'
#' @description This function helps to interpolate and discretize depth-dependent data to match a set depth profile. \n
#' The idea is that the data is discretized into set depth steps (see depth_target), each specifying a layer with an
#' upper and lower boundary.
#' So that for example one depth step is from top of the humus layer to +5 cm, the next from 5 to 0 cm and so on.
#' The format of this final dataframe is specified in "depth_target", where a numeric vector of the interfaces of the steps is given
#' (e.g. c(6,3,0,-5,-10) resulting in 4 depth steps). \n
#' \n
#' There are different interpolation methods implemented, which might be more practical for different parameters or tasks. \n
#' \n
#'  A linear interpolation is possible for more or less contiuous parameters, (e.g. soil temperature). \n
#'  The boundary interpolation is a nearest neighbor interpolation and uses the values set in the "upper" and "lower" variables
#'  to map the parameter to any step within these limits. \n
#'  A linear spline interpolation fits a linear spline model to the data with knots defined in "knots". \n
#'  \n
#' It is possible to provide multiple parameters to be discretized. In this case it is also possible to define specific controls for each parameter individually. \n
#' However, if only one value is given for method, int_depth, or knots, the corresponding value is applied to all parameters given in "param".
#'
#' @param df (dataframe) The dataframe containing the parameters to be interpolated, as well as the columns "depth", "upper" and "lower".
#' @param param (character vector) The column names name of the parameters to be interpolated.
#' @param method (character vector) a character (-vector) specifying the methods to be used for interpolation. Must be in the same order as param. One of \n
#' "linear" = linear interpolation. \n
#' "boundary" = mapping values to any depth within the boundrys. Suited for discrete variables. \n
#' "linspline" = fits a linear spline. similar to linear interpolation but with knots defined in "knots".
#' @param depth_target (numeric vector or data frame) specifying the format of the target depths to be interpolated. Must include n+1 depths for n target depth steps.
#' If it is different per id_cols, enter a data.frame in long form instead. This data frame must have  a "depth" column, as well as the columns that identify the different cases. These id-columns must be the same as or a subset of id_cols.
#' @param boundary_nearest (logical) = TRUE/FALSE if it is TRUE then for target depth steps (partially) outside of the parameter boundaries, the neirest neighbor is returned, else returns NA. Default is FALSE.
#' @param int_depth (numeric vector)  = value between 0 and 1 for 1 = interpolation takes the top of each depth step, 0.5 = middle and 0= bottom. Default = 0.5
#' @param knots (numeric vector) = the depths at which knots for the linspline-method are to be placed. If this differs for the parameters, a list of numeric vectors with the same lenght as "param" can be provided. Cannot differ between id_cols.
#' @param id_cols (character vector) = The names of the columns to be grouped by, i.e. uniquely identifying one profile (e.g. c('Plot','Date')).
#'
#'
#' @return dataframe with the columns upper and lower derived from depth_target, depth being the middle of each depth step, as well as the interpolated and discretized parameters.
#'
#' @import dplyr
#' @import splines
#'
#' @example
#' discretize_depth(df = soilphys,
#'                  param = c("TPS","a","b"),
#'                  method = "boundary",
#'                  depth_target = depth_target,
#'                  boundary_nearest = T,
#'                  id_cols = c("Plot","Date))
#'
#' @export

discretize_depth<- function(df,
                          param,
                          method,
                          depth_target,
                          id_cols,
                          boundary_nearest = F,
                          int_depth = 0.5,
                          knots = NULL,
                          ...){


#First define a function to do the discretisation for one profile
discretize <- function(df,
                       depth_target,
                       depth_target_mid){

# interpolation function definitions: ----------------------
# each function must follow the following structure:
# method_name'_intdisc <-function(param,depth,upper,lower,depth_target,control)
# "control" is a list, where additional controlling variables will be stored.
# The function must be able to accept control, even if it is not evaluating anything from it.

linear_intdisc<-function(param,depth_target,int_depth){
  #Linear interpolation

  depth_target_tmp <- depth_target[-1]+diff(depth_target)*(int_depth-1)
  param_int <- approxfun(depth,param)(depth_target_tmp)
  return(param_int)
}



boundary_intdisc<-function(param){
  #Boundary interpolation (nearest neighbor)
  param_int <- param[indices]

  return(param_int)
}

linspline_intdisc<-function(param,depth_target,int_depth,knots){
  #Linear spline interpolation
  depth_target_tmp <- depth_target[-1]+diff(depth_target)*(int_depth-1)
  param_int <- predict(lm(param ~splines::bs(depth,knots=knots,degree=1)),newdata = list(depth =depth_target_tmp))
  return(param_int)
}



#create interpolation indices for boundary method, if a parameter is to be interpolated by boundary
if("boundary" %in% method){

  upper_orig <- df$upper
  lower_orig <- df$lower

  #if boundary_nearest is FALSE, NA is returned for not fitting steps
  if(boundary_nearest[1] == F){
    indices<-unlist(
      lapply(2:length(depth_target),function(i){
        id<-which(upper_orig >= depth_target[i] & lower_orig<= depth_target[i-1])
        if(length(id)==0){
          id <- NA
        }
        return(id)})
    )
  #if boundary nearest is T, then the highest (or lowest) value is returned instead
  } else {
    indices<-
      unlist(
        lapply(2:length(depth_target),function(i){
          id <-which(upper_orig >= depth_target[i] & lower_orig<= depth_target[i-1])
          if (length(id)==0){
            if(boundary_nearest ==F){
              id<-NA
            } else if(depth_target[i]>max(upper_orig)){
              id <- length(upper_orig)
            } else {
              id <- 1
            }

          }
          return(id)}
        ))
  }
}
#create depth variable if linear is in methods
if (any(c("linear","linspline") %in% method)){
  depth <- df$depth

}

df_discretized <- lapply(1:l_param, function(i){
  param_tmp <- unlist(df[,param[i]])
  meth_tmp <- method[i]
  if(meth_tmp == "boundary"){
    boundary_neares <- boundary_nearest[i]
    param_intdisc <- boundary_intdisc(param = param_tmp)
  } else if(meth_tmp == "linear"){
    int_depth_tmp <- int_depth[i]
    param_intdisc <- linear_intdisc(param_tmp,depth_target,int_depth_tmp)
  } else if(meth_tmp == "linspline"){
    int_depth_tmp <- int_depth[i]
    knots_tmp <- knots[[i]]
    param_intdisc <- linspline_intdisc(param_tmp,depth_target,int_depth_tmp,knots_tmp)

  } else if(meth_tmp == "asdasfdfg"){
    #for future interpolation methods
  } else {

  }

  return(param_intdisc)
})
names(df_discretized)<-param

return(df_discretized)

}


if (is.list(knots)==F){
  knots <-list(knots)
}


l_param <- length(param)
l_meth <- length(method)
l_incl <- length(boundary_nearest)
l_int <- length(int_depth)
l_knots <- length(knots)

warn_names <- c("methods","boundary_nearest","int_depth","knots")
warn_lengths <- c(l_meth,l_incl,l_int,l_knots)

for (i in 1:3){
  l <- warn_lengths[i]
  warn_name <- warn_names[i]

  if(!l == l_param & !l == 1 ){
    stop(paste0("'",warn_name,"' must be the same length of param or of length 1"))
  } else if (l == 1 & !l_param ==1){
    warning(paste("applying the same '",warn_name,"' to all parameters"))
  }
}

if(l_meth ==1){
  method <- rep(method, l_param)
}
if(l_incl == 1){
  boundary_nearest <- rep(boundary_nearest,l_param)
}
if(l_int == 1){
  int_depth <- rep(int_depth,l_param)
}
if(l_knots == 1){
  knots <- rep(knots,l_param)
}



#creating vectors for method control
boundary_nearest <- unlist(boundary_nearest)
method <- unlist(method)
int_depth <- unlist(int_depth)






#caring about the order of things:
if("depth" %in% names(df)){
  df <- df %>% dplyr::arrange(depth)
} else if("upper" %in% names(df)){
  df <- df %>% dplyr::arrange(upper)

}



#checking if id_cols are present in the provided data frame
df_names <- names(df)
if (!all(id_cols %in% df_names)){
  stop("id_cols not present in input dataframe")
}

if (is.vector(depth_target)){
  depth_target = data.frame(depth = depth_target, stringsAsFactors = F)
} else if (!is.data.frame(depth_target)){
  #if it isn't a data frame - what is it? stopping.
  stop("depth_target must be a numeric vector or a data frame!")
}

#checking for which id_cols are in target-depth data.frame
target_id <- id_cols[which(id_cols %in% names(depth_target))]

#flagging if there are no id cols in depth_target
target_flag <- (length(target_id) == 0)

#only selecting relevant columns in depth_target
depth_target <- depth_target %>%
  dplyr::select(any_of(c("depth",target_id)))

#sorting depth_target low2high
depth_target <- depth_target %>%
  dplyr::arrange(depth)

#creating id-column if target_id are not zero
depth_target$gr_id <- 1
if(length(target_id)>0){
depth_target <- depth_target %>%
  dplyr::ungroup() %>%
  dplyr::group_by(dplyr::across({{target_id}})) %>%
  dplyr::mutate(gr_id = cur_group_id())
}


depth_target_mid <- depth_target %>%
  dplyr::group_by(gr_id) %>%
  dplyr::mutate(depth_l = lag(depth,1)) %>%
  dplyr::mutate(depth = (depth+depth_l) / 2) %>%
  dplyr::select(-depth_l) %>%
  dplyr::filter(!is.na(depth))


#checking for duplicates
dup_flag <- !(nrow(depth_target) == depth_target %>% dplyr::distinct() %>% nrow())

if(dup_flag){
  stop("depth_target: rows are not unique!")
}

df <-depth_target %>%
  dplyr::select(-"depth") %>%
  dplyr::distinct() %>%
  dplyr::left_join(df %>%
                     dplyr::select(-dplyr::any_of("gr_id")),by = target_id) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(dplyr::across({{id_cols}}))


df <-df %>% mutate(prof_id = cur_group_id())

df <- as.data.frame(df)

df_ret <- lapply(unique(depth_target$gr_id),function(id_gr){

  dt <-depth_target$depth[depth_target$gr_id == id_gr]
  dt_mid <- depth_target_mid$depth[depth_target_mid$gr_id == id_gr]

  df_tmp <- df[df$gr_id == id_gr,]

  df_ret <-lapply((df_tmp %>% dplyr::group_by(prof_id) %>% dplyr::group_split()),function(df_sp){
    df_sp <- discretize(df =as.data.frame(df_sp),
                        depth_target = dt,
                        depth_target_mid = dt_mid)
    return(df_sp)

  }) %>% bind_rows()
  k <- nrow(df_ret)/(length(dt_mid))

  #print(k)
  lower <- dt[-length(dt)]
  upper <- dt[-1]

  df_ret$depth <- rep(dt_mid,times = k)
  df_ret$upper <- rep(upper,times = k)
  df_ret$lower <- rep(lower,times = k)
  df_ret$prof_id <- rep(sort(unique(df_tmp$prof_id)),each = length(upper))

  return(df_ret)

}) %>% dplyr::bind_rows()


df_ret <- df %>% select(any_of({c(id_cols,"prof_id")})) %>% distinct() %>%left_join(df_ret,by = "prof_id")

return(df_ret)

}

