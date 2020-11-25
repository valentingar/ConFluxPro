#' @title discretize_depth
#'
#' @description This function helps to interpolate and discretize the data in soiltemp and soilwater to match a set final
#' form for the soilphys dataframe. The idea is that the profile is discretised into set depth steps (see depth_target),
#' each specifying a layer with an upper and lower boundry. So that for example one depth step is from top of the humus layer to +5 cm, the next from 5 to 0 cm and so on.
#' The format of this final dataframe is specified in "depth_target", where a numeric vector of the boundrys is given. (e.g. c(6,3,0,-5,-10) resulting in 4 depth steps)
#' There are different interpolation methods implemented, which might be more practical for different parameters.
#'
#'  A linear interpolation is possible for more or less contiuous parameters, (e.g. top soil temperature)
#'  A boundry interpolation is similar to a neirest neighbour interpolation and uses the values set in the upper and lower variables
#'  to map the parameter to any depth within these limits.
#'  The control parameter "tie_up" determines here wether the upper or lower boundry is included. It is T by default.
#'  An exponential interpolation fits an exponential function to the parameter agains depth and predicts values accordingly.
#'  a linear spline interpolation fits a linear spline model to the data with knots defined in the controlvariable "spline_knots".
#'
#' Data given to discretize_depth must always represent one profile, i.e. must be grouped by Plot (& Date for temporal data).
#' If only one value is given for method, boundary_nearest, int_depth the same is applied to all parameters given in "param"
#'
#' @param df (dataframe) The dataframe containing the parameters to be interpolated, as well as the columns "depth", upper" and lower.
#' @param param (character vector) The column names name of the parameters to be interpolated.
#' @param method (character vector) a character (-vector) specifying the methods to be used for interpolation. Must be in the same order as param. One of
#' "linear" = linear interpolation.
#' "boundary" = mapping values to any depth within the boundarys. Suited for discrete variables.
#' "exp" = exponential fit
#' "spline_lin" = fits a linear spline. similar to linear interpolation but
#' @param depth_target (numeric vector) specifying the format of the depths to be interpolated. Must include n+1 depths for n target depth steps.
#' @param boundary_nearest (logical vector) = TRUE/FALSE if it is TRUE then for target depth steps (partially) outside of the parameter boundaries, the neigherst neighbour is returned, else returns NA. Default is FA
#' @param int_depth (numeric vector)  = value between 0 and 1 for 1 = interpolation takes the top of each depth step, 0.5 = middle and 0= bottom. Default = 0.5
#'
#' @return dataframe with the columns upper and lower derived from depth_target, depth being the middle of each depth step, as well as the interpolated and discretised parameters.
#'
#' @import dplyr
#' @import splines
#'
#' @example
#' soilphys %>% group_by(Plot) %>%
#'  group_modify(~{df<-discretize_depth(df = .x,
#'                                      param = c("TPS","a","b"),
#'                                      method = "boundary",
#'                                      depth_target = depth_target,
#'                                      boundary_nearest = T)
#'  return(df)
#'  })
#'
#' @export

discretize_depth<- function(df,
                          param,
                          method,
                          depth_target,
                          control){


boundary_nearest <- control[["boundary_nearest"]]
int_depth <- control[["int_depth"]]
knots <- control[["knots"]]

if (is.null(int_depth)==T){
  int_depth <- 0.5
}

if(is.list(boundary_nearest)==F){
  boundary_nearest <-   list(boundary_nearest)
}
if(is.list(int_depth)==F){
  int_depth <- list(int_depth)
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



#caring about the order of things:
depth_target <- sort(depth_target,decreasing = T)
if("depth" %in% names(df)){
df <- df %>% dplyr::arrange(desc(depth))
} else if("upper" %in% names(df)){
  df <- df %>% dplyr::arrange(desc(upper))

}

# interpolation function definitions: ----------------------
# each function must follow the following structure:
# method_name'_intdisc <-function(param,depth,upper,lower,depth_target,control)
# "control" is a list, where additional controlling variables will be stored.
# The function must be able to accept control, even if it is not evaluating anything from it.
linear_intdisc<-function(param,df,depth_target,control){
  depth <- df$depth
  int_depth <- control[["int_depth"]]
  depth_target_tmp <- depth_target[-1]-diff(depth_target)*(1-int_depth)
  param_int <- approxfun(depth,param)(depth_target_tmp)
  return(param_int)
}
boundary_intdisc<-function(param,df,depth_target,control){
boundary_nearest <- control[["boundary_nearest"]]
upper_orig <- df$upper
lower_orig <- df$lower
upper_target <- depth_target[-length(depth_target)]
lower_target <- depth_target[-1]




param_int <-unlist(lapply(seq(upper_target),function(i){
  if (upper_target[i] > upper_orig[1] & boundary_nearest == T){
    warning(paste0("using nearest neighbour (topmost) for depth_step ",upper_target[i]," - ",lower_target[i]))
    p <- 1
  } else if (lower_target[i]<lower_orig[length(lower_orig)] & boundary_nearest == T){
    warning(paste0("using nearest neighbour (lowest) for depth_step ",upper_target[i]," - ",lower_target[i]))
    p <- length(lower_orig)
  } else  if (length(which(c(upper_orig,lower_orig) >lower_target[i] & c(upper_orig,lower_orig) < upper_target[i]))>0){
    stop("error while using method boundary, discrete step within target depth-step")
  } else{
  p <- which(upper_target[i] <= upper_orig & lower_target[i] >= lower_orig)
  }
  if (length(p)==0){
    param_ret <-  NA
  } else {
    param_ret <- param[p]
  }
  return(param_ret)
}))
#print(param_int)

return(param_int)
}


spline_lin_intdisc<-function(param,df,depth_target,control){
  depth <- df$depth
  int_depth <- control[["int_depth"]]
  knots <- control[["knots"]]

  depth_target_tmp <- depth_target[-1]-diff(depth_target)*(1-int_depth)
  param_int <- predict(lm(param ~ bs(depth, knots = knots,degree = 1)),newdata = data.frame(depth = depth_target_tmp))
  return(param_int)
}


df_discretized <- lapply(1:l_param, function(i){
  param_tmp <- unlist(df[,param[i]])
  meth_tmp <- method[i]
  boundary_nearest_tmp <- boundary_nearest[[i]]
  int_depth_tmp <- int_depth[[i]]
  knots_tmp <- knots[[i]]
  param_intdisc<-do.call(paste0(meth_tmp,"_intdisc"),list(param = param_tmp,
                                                          df = df,
                                                          depth_target = depth_target,
                                                          control = list(boundary_nearest = boundary_nearest_tmp,
                                                                         int_depth = int_depth_tmp,
                                                                         knots = knots_tmp)))
  df_ret <-data.frame(paramname = param_intdisc)
  names(df_ret) <- c(param[i])
  return(df_ret)
}) %>%
  dplyr::bind_cols()

df_discretized <- df_discretized %>% dplyr::mutate(depth = !!depth_target[-1]-diff(!!depth_target)/2,
                                                   upper = !!depth_target[-length(!!depth_target)],
                                                   lower = !!depth_target[-1])

return(df_discretized)
}

