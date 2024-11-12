#' @title Interpolate over depth to layered profile
#'
#' @description Interpolate and discretize data into a layered structure.
#' The output is a data.frame where each profile is separated into layers
#' that intersect at depths defined in the function call. See
#' [cfp_layered_profile()].
#'
#' There are different interpolation methods implemented, which might be
#' more practical for different parameters or tasks.
#'   \itemize{
#'   \item A \code{'linear'} interpolation for continuous parameters, (e.g. soil
#'   temperature). \item The
#'   \code{'boundary'} interpolation is only suitable for data that is already
#'   layered. It selects the value from the old layer that in which
#'   the new layer will lay in.
#'   \item
#'   A \code{'linspline'} interpolation fits a linear
#'   spline model to the data with knots defined in  \code{knots}
#'   \item
#'   \code{'nearest'} finds the closest value to the new layer. You can
#'   define whether the closest value should be nearest to the top \code{1},
#'   or bottom \code{0} of the layer using \code{int_depth}
#'   \item
#'   \code{'harmonic'} is similar to a linear interpolation but it uses
#'   the harmonic mean [harm()] using the distance in depth to each
#'   value as weights.
#'   }
#'
#'   Multiple variables can be discretized at the same time by supplying
#'   multiple column names in \code{param}.
#'   It is also possible to use different \code{method} and controlling
#'   parameters \code{int_depth} and \code{knots} for each \code{param}.
#'   Just provide a list of settings the same length as \code{param}.
#'   If only one value is given, but multiple \code{param} the settings are
#'   reused for each parameter.
#'
#' @param df (dataframe) The dataframe containing the parameters to be
#'   interpolated, as well as the columns "depth", "upper" and "lower".
#' @param param (character vector) The column names name of the parameters to be
#'   interpolated.
#' @param method (character vector) a character (-vector) specifying the methods
#'   to be used for interpolation. Must be in the same order as param. One of
#'   \itemize{
#'   \item{linear}
#'   \item{boundary}
#'   \item{linspline}
#'   \item{nearest}
#'   \item{harmonic}
#'   }
#'
#' @param depth_target (numeric vector or data frame) specifying the new layers.
#'   Must include n+1 depths for n target layers.
#'
#'   If the target layers are different for id_cols, enter a data.frame instead.
#'   This data frame must
#'   have a "depth" column, as well as well as all \code{id_cols} needed that
#'   must be at least a subset of the \code{id_cols} of the original data.
#'
#' @param boundary_nearest (logical) = TRUE/FALSE if it is TRUE then for target
#'   depth steps (partially) outside of the parameter boundaries, the nearest
#'   neighbor is returned, else returns NA. Default is FALSE.
#' @param boundary_average ("character) Defines what happens if the
#' new layer contains multiple old layers. one of
#' \describe{
#' \item{none}{= the default \cr the new layer is set to NA}
#' \item{arith}{the new layer is calculated as the arithmetic mean of the old}
#' \item{harm}{the new layer is calculated as the harmonic mean of the old}
#'}
#'
#' @param int_depth (numeric vector)  = value between 0 and 1 for 1 =
#'   interpolation takes the top of each depth step, 0.5 = middle and 0= bottom.
#'   Default = 0.5
#' @param knots (numeric vector) = the depths at which knots for the
#'   'linspline' method are to be placed. If this differs for the parameters, a
#'   list of numeric vectors with the same length as "param" can be provided.
#'   Cannot differ between id_cols.
#'
#' @inheritDotParams cfp_profile id_cols
#'
#'
#' @return A [cfp_layered_profile()] data.frame with the variables \code{upper}
#' and \code{lower} defining the layers derived from depth_target.
#' The column \code{depth} is the middle of each layer. And all variables from
#' \code{param}
#'
#' @family soilphys
#'
# @import splines
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples {
#'
#' data("soiltemp")
#' library(dplyr)
#'
#' dt <- soiltemp %>%
#'   select(site,depth) %>%
#'   distinct() %>%
#'   group_by(site) %>%
#'   slice_max(depth) %>%
#'   reframe(depth = c(depth,seq(0,-100,-10)))
#'
#' discretize_depth(df = soiltemp,
#'                  param = "t",
#'                  method = "linear",
#'                  depth_target = dt,
#'                  id_cols = c(
#'                    "site","Date"))
#' }
#'
#' @export
discretize_depth<- function(df,
                            param,
                            method,
                            depth_target,
                            boundary_nearest = FALSE,
                            boundary_average = "none",
                            int_depth = 0.5,
                            knots = NULL,
                            ...){
  UseMethod("discretize_depth")
}

#' @rdname discretize_depth
#' @exportS3Method
discretize_depth.cfp_profile <-
  function(df,
           param,
           method,
           depth_target,
           boundary_nearest = FALSE,
           boundary_average = "none",
           int_depth = 0.5,
           knots = NULL,
           ...){
    rlang::check_dots_empty()

    id_cols <- cfp_id_cols(df)
    NextMethod(id_cols = id_cols)
}



#' @rdname discretize_depth
#' @inheritParams cfp_profile
#' @exportS3Method
discretize_depth.data.frame <-
  function(df,
           param,
           method,
           depth_target,
           boundary_nearest = FALSE,
           boundary_average = "none",
           int_depth = 0.5,
           knots = NULL,
           id_cols = NULL,
           ...){
rlang::check_dots_empty()


#make knots into a list if it isnt.
if (!is.list(knots)){
  knots <-list(knots)
}

if (any(id_cols %in% param)){
  to_keep <- !(param %in% id_cols)
  param <- param[to_keep]
}

l_param <- length(param)
l_meth <- length(method)
l_incl <- length(boundary_nearest)
l_b.av <- length(boundary_average)
l_int <- length(int_depth)
l_knots <- length(knots)

#check for correct input and warn if problems arise
warn_names <- c("method","boundary_nearest","boundary_average",
                "int_depth","knots")
warn_lengths <- c(l_meth,l_incl,l_b.av,l_int,l_knots)

for (i in 1:5){
  l <- warn_lengths[i]
  warn_name <- warn_names[i]
  if(l != 1){
    assign(warn_name, get(warn_name)) # trim to new params
    l <- length(get(warn_name))
  }
  if(!l == l_param & !l == 1 ){
    stop(paste0("'",warn_name,
                "' must be the same length of param or of length 1"))
  }
}


#checking if id_cols are present in the provided data frame
df_names <- names(df)
if (!all(id_cols %in% df_names)){
  stop("id_cols not present in input dataframe")
}

if (is.vector(depth_target)){
  depth_target <- data.frame(depth = depth_target, stringsAsFactors = FALSE)
} else if (!is.data.frame(depth_target)){
  #if it isn't a data frame - what is it? stopping.
  stop("depth_target must be a numeric vector or a data frame!")
}

#checking if depth is present if method = "linear"/"linspline
if (any(c("linear","linspline","nearest","harmonic") %in% method)){
  if(!("depth" %in% df_names)){
    stop("for this method, variable 'depth' must be present in df")
  }
}
#checking if upper/lower is present if method = "linear"/"linspline
if ("boundary" %in% method){
  if(!all((c("upper","lower") %in% df_names))){
    stop("for this method, variable 'upper' and 'lower' must be present in df")
  }
}

#make method, boundary nearest etc correct length
if(l_meth ==1){
  method <- rep(method, l_param)
}
if(l_incl == 1){
  boundary_nearest <- rep(boundary_nearest,l_param)
}
if(l_b.av == 1){
  boundary_average <- rep(boundary_average,l_param)
}
if(l_int == 1){
  int_depth <- rep(int_depth,l_param)
}
if(l_knots == 1){
  knots <- rep(knots,l_param)
}



#creating vectors for method control
#boundary_nearest <- unlist(boundary_nearest)
#method <- unlist(method)
#int_depth <- unlist(int_depth)



# caring about the order of things:
# the input data.frame should be ordered with ascending depth/upper
if("depth" %in% names(df)){
  df <- df %>%
    dplyr::arrange(depth)
} else if("upper" %in% names(df)){
  df <- df %>%
    dplyr::arrange(upper)
}


#checking  which id_cols are in target_depth data.frame
target_id <- id_cols[which(id_cols %in% names(depth_target))]

#flagging if there are no id cols in depth_target
target_flag <- (length(target_id) == 0)

#only selecting relevant columns in depth_target
depth_target <- depth_target %>%
  dplyr::select(dplyr::any_of(c("depth",
                         target_id)))

#sorting depth_target low2high
depth_target <- depth_target %>%
  dplyr::arrange(depth)

# creating gr_id-column if target_id are not zero, initializing gr_id = 1
# gr_id indicates the number of unique groups to be interpolated
# each group will have a different profile.
depth_target$gr_id <- 1
if(length(target_id)>0){

  depth_target <-
    depth_target %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(target_id))) %>%
    dplyr::mutate(gr_id = dplyr::cur_group_id())

}

# getting the mid depths of each layer in depth_target
# each group will have one less row than it started with
depth_target_mid <-
  depth_target %>%
  dplyr::group_by(.data$gr_id) %>% #must be grouped!
  dplyr::mutate(depth_l = dplyr::lag(.data$depth, 1)) %>%
  dplyr::mutate(depth = (.data$depth + .data$depth_l) / 2) %>%
  dplyr::select(-"depth_l") %>%
  dplyr::filter(!is.na(.data$depth)) #remove additional rows


#checking for duplicates and stopping if necessary
dup_flag <- !(nrow(depth_target) == depth_target %>%
                dplyr::distinct() %>% nrow())

if(dup_flag){
  stop("depth_target: rows are not unique!")
}

# joining df with only those groups (defined by id_cols)
# that are present in depth_target
if (length(id_cols)>0){
df <-depth_target %>%
  dplyr::select(-"depth") %>%
  dplyr::distinct() %>%
  dplyr::left_join(df %>%
                     dplyr::select(-dplyr::any_of("gr_id")),
                   by = target_id) %>%
  dplyr::ungroup()
} else {
  df$gr_id <- 1 #only one group if no id_cols provided
}

df <- df%>%
  dplyr::group_by(dplyr::across(!!id_cols)) %>%
  dplyr::mutate(prof_id = dplyr::cur_group_id()) %>% #add profile id
  as.data.frame() #faster than tibble?

#######################################
# creating return data frame: -----------

#looping over all groups and then all profiles within those groups

df_ret <- lapply(unique(depth_target$gr_id),function(id_gr){

  #creating vectors of boundaries (dt) and mids of layers (dt_mid)
  dt <-depth_target$depth[depth_target$gr_id == id_gr]
  dt_mid <- depth_target_mid$depth[depth_target_mid$gr_id == id_gr]

  #subsetting data.frame to currect group
  df_tmp <- df[df$gr_id == id_gr,]

  # loop over all profiles in the group and interpolate
  # via helper-function discretize()
  df_ret <-lapply((df_tmp %>%
                     dplyr::group_by(prof_id) %>%
                     dplyr::group_split()),
                  function(df_sp){
    df_sp <- discretize(df = as.data.frame(df_sp),
                        depth_target = dt,
                        depth_target_mid = dt_mid,
                        method,
                        boundary_nearest,
                        boundary_average,
                        l_param,
                        int_depth,
                        knots,
                        param)
    df_sp
  }) %>%
    dplyr::bind_rows()
  k <- nrow(df_ret)/(length(dt_mid))

  lower <- dt[-length(dt)]
  upper <- dt[-1]

  df_ret$depth <- rep(dt_mid, times = k)
  df_ret$upper <- rep(upper, times = k)
  df_ret$lower <- rep(lower, times = k)
  df_ret$prof_id <- rep(sort(unique(df_tmp$prof_id)),
                        each = length(upper))

  df_ret

}) %>%
  dplyr::bind_rows()


df_ret <- df %>%
  dplyr::select(dplyr::any_of({c(id_cols, "prof_id")})) %>%
  dplyr::distinct() %>%
  dplyr::left_join(df_ret, by = "prof_id")%>%
  dplyr::select(-"prof_id")

df_ret <- cfp_layered_profile(df_ret, id_cols = id_cols)
df_ret
}




###########################################################
######## HELPERS ------------------------------------------
###########################################################





#Function to do the discretisation for one profile ----------------------------
discretize <- function(df,
                       depth_target,
                       depth_target_mid,
                       method,
                       boundary_nearest,
                       boundary_average,
                       l_param,
                       int_depth,
                       knots,
                       param){


  df_discretized <- lapply(1:l_param, function(i){
    param_tmp <- unlist(df[,param[i]])
    meth_tmp <- method[i]

    if(meth_tmp == "boundary"){
      boundary_nearest_tmp <- boundary_nearest[i]
      boundary_average_tmp <- boundary_average[i]

      param_intdisc <-
        boundary_intdisc(lower = df$lower,
                         upper = df$upper,
                         param = param_tmp,
                         depth_target = depth_target,
                         boundary_nearest = boundary_nearest_tmp,
                         boundary_average = boundary_average_tmp)

    } else if(meth_tmp == "linear"){

      int_depth_tmp <- int_depth[i]
      param_intdisc <- linear_intdisc(param_tmp,
                                      depth_target,
                                      int_depth_tmp,
                                      df$depth)

    } else if(meth_tmp == "linspline"){
      int_depth_tmp <- int_depth[i]
      knots_tmp <- knots[[i]]
      param_intdisc <- linspline_intdisc(param_tmp,
                                         depth_target,
                                         int_depth_tmp,
                                         knots_tmp,
                                         df$depth)

    } else if(meth_tmp == "nearest"){

      int_depth_tmp <- int_depth[i]
      param_intdisc <- nearest_intdisc(param_tmp,
                                      depth_target,
                                      int_depth_tmp,
                                      df$depth)

    } else if(meth_tmp == "harmonic"){

      int_depth_tmp <- int_depth[i]
      param_intdisc <- harmonic_intdisc(param_tmp,
                                       depth_target,
                                       int_depth_tmp,
                                       df$depth)

    } else if(meth_tmp == "asdasfdfg"){
      #for future interpolation methods
    } else {

    }

    return(param_intdisc)
  })
  names(df_discretized)<-param

  return(df_discretized)

}


# interpolation function definitions: ----------------------

### linear interpolation ###
linear_intdisc<-function(param,depth_target,int_depth,depth){
  #Linear interpolation

  depth_target_tmp <- depth_target[-1]+diff(depth_target)*(int_depth-1)
  param_int <- stats::approxfun(depth,param)(depth_target_tmp)
  }


### nearest interpolation ###

nearest_intdisc <- function(param,depth_target,int_depth,depth){
  #Linear interpolation

  depth_target_tmp <- depth_target[-1]+diff(depth_target)*(int_depth-1)
  flex_length_apply(depth_target_tmp,
         function(i) param[which.min(abs(depth-i))])
}


### linear harmonic function ###

harmonic_intdisc <-function(param,depth_target,int_depth,depth){
  #Linear interpolation

  depth_target_tmp <- depth_target[-1]+diff(depth_target)*(int_depth-1)
  upper <- depth[-1]
  lower <- depth[-length(depth)]

  vapply(depth_target_tmp,function(i) {
    # identify the correct interval
    int_id <- which(upper >= i & lower < i)

    # harmonic mean of respective values with
    # the distance (of the counterpart) as
    # weight (i.e. the further away the __other__
    # boundary is, the higher the weight)
    p <- harm(c(param[int_id],param[int_id+1]),
              abs(i-c(upper[int_id],lower[int_id])))
  },
  FUN.VALUE = double(1))
}



### boundary interpolation ###
boundary_intdisc <- function(lower,
                             upper,
                             param,
                             depth_target,
                             boundary_nearest,
                             boundary_average){
  #everything must be sorted low2high

  #assinging upper / lower bounds of region
  l <- length(upper)
  upper_max <- upper[l]
  lower_min <- lower[1]

  #looping over all new depth-steps
  flex_length_apply(1:(length(depth_target)-1),
         FUN = function(i){

    #assinging new upper/lower boundary
    upper_new <- depth_target[i+1]
    lower_new <- depth_target[i]

      ######## IDEAL #########
    id <- which(upper >= upper_new &
                  lower <= lower_new
    )

    if (length(id) == 1){
      #return ideal fit
      return(param[id])

      ####### NEAREST #########

    } else if (boundary_nearest &
               upper_new>upper_max) {
      #return upper bound value if layer above region
      return(param[l])

    } else if (boundary_nearest &
               lower_new<lower_min) {
      #return lower bound value if layer below region
      return(param[1])

      ######## AVERAGE #########

    } else if (boundary_average == "none"){
      #return NA if no average is wished
      return(NA)

    } else if (upper_new > upper_max | lower_new < lower_min) {
      # if boundary_nearest == FALSE these must be NA,
      # otherwise wrong average below
      return(NA)

    } else if (!is.numeric(param)){
      # return NA if param is not numeric
      # (cant average discrete values)
      return(NA)

    } else {

      #find upper (u) lower (l) or middle (m) layers
      id_u <-which(upper >= upper_new &
                     lower < upper_new)
      id_l <-which(upper > lower_new &
                     lower <= lower_new)
      id_m <- which(upper < upper_new &
                      lower > lower_new)

      # get value and weigh with height of old layer
      # in new layer
      p_u <- param[id_u]
      w_u <- (upper_new-lower[id_u])
      p_l <- param[id_l]
      w_l <- (upper[id_l]-lower_new)
      p_m <- param[id_m]
      w_m <- (upper[id_m]-lower[id_m])


      # sum up and normalise to layer height
      # (counterpart to weights)
      if (boundary_average == "arith"){
        ## arithmetic
      p <- sum(c(w_u*p_u,w_l*p_l,w_m*p_m))/sum(c(w_u,w_l,w_m))
      } else {
        ## harmonic (Add condition if more averages are implemented)
      p <- harm(c(p_u,p_l,p_m),c(w_u,w_l,w_m))
      }
      p
    }
  })
}


### linear spline function ###
linspline_intdisc<-function(param,depth_target,int_depth,knots,depth){
  #Linear spline interpolation
  depth_target_tmp <- depth_target[-1]+diff(depth_target)*(int_depth-1)
  param_int <- stats::predict(
    stats::lm(
      param ~ splines::bs(depth,knots=knots,degree=1)),
    newdata = list(depth =depth_target_tmp))
  return(param_int)
}





