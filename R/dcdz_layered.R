#' @title dcdz_layered
#'
#' @description This function calculates concentration gradients using different
#' approaches.
#'
#'
#' @param df (dataframe) the gasdata dataframe, filtered to one profile
#'  (e.g. 1 day & one Plot ).
#' @param layers_map (dataframe) containing the following parameters:
#' \itemize{
#' \item "layer"=name of the layer;
#' \item "upper"=upper limit of layer in cm;
#' \item "lower" = lower limit of the layer in cm;}
#' @param mode (character) One of ("LL","LS","EF","DA").
#'
#' @return df (dataframe) same structure as layer_map with folowing columns:
#' @return mode (character) the used gradient method.
#' @return layer (character) the layer name
#' @return upper (numeric) upper boundry in cm
#' @return lower (numeric) lower boundry in cm
#' @return dcdz_ppm (numeric) concentration gradient in ppm/cm.
#' @return dcdz_sd (numeric) standard error in ppm/cm.
#' @return dc_ppm (numeric) concentration difference in ppm.
#' @return r2 (linearised) R^2 for the model.
#'
#' @examples {
#' df <- data.frame(depth = c(10,0,-100),
#'                  x_ppm = c(400,800,5000),
#'                  gd_id = c(1,1,1))
#'
#' lmap <- data.frame(upper = c(10,0),
#'                    lower = c(0,-100),
#'                    layer = c("HU","MIN"))
#' dcdz_layered(df,
#'              lmap,
#'              mode = "LL"
#' )
#'
#'
#' }
#'
#' @family FLUX
# @import splines
#'
#' @keywords internal
#'
#' @importFrom rlang .data
#'
#' @export

dcdz_layered <- function(df,
                         layers_map,
                         mode
                         ){
valid_modes <- c("LS","LL","EF","DA")

if (!(mode %in% valid_modes)){
  stop(paste0("wrong mode selected: ",mode,
              ". Please use one of the following modes: ",
              paste0(valid_modes,collapse = ", ")))
}

upper <- layers_map$upper
lower <- layers_map$lower
layers <- layers_map$layer
depth_steps <- upper[-1]
gd_id <- df$gd_id[1]


#depths including boundaries from top to bottom
depths <- rev(sort(unique(c(upper, lower))))

#depths with values in the df
depths_df <- rev(sort(unique(df$depth)))

#true if the there are depths that exceed depths of non-NA values in df.
ls_flag <- length(which(depths > max(depths_df, na.rm = TRUE) |
                          depths < min(depths_df,na.rm = TRUE)))>0

#initializing NA-logical for return
return_na <- FALSE

if (mode == "LS"){

#not enough non-NA values or outside range.
if (nrow(df)<length(depths) | ls_flag){
  return_na <- TRUE
} else {
#spline model
mod<-stats::lm(x_ppm~splines::bs(depth,knots=depth_steps,#depth_steps,
                       degree = 1
),
data=df)

# dc = the difference in concentration in ppm
dc <- -diff(stats::predict(mod,newdata = data.frame(depth =depths)))
# dcdz = the concentration gradient in ppm/m
dcdz <- dc / -diff(depths) *100 #from cm^-1 to m^-1


# error estimate in ppm/m:
dcdz_sd <- -rev(
  as.numeric(
    summary(mod)$coefficients[,2][-1]+
      c(0, dplyr::lag(summary(mod)$coefficients[,2][-1],1)[-1])))/
  diff(depths) *100
if (!length(dcdz_sd)==length(layers)){
  dcdz_sd <- rep(NA,length(layers))
}

#r_squared
r2 <- summary(mod)$r.squared
}
#create return table
create_return <- TRUE

} else if (mode == "LL"){


  df_ret <- data.frame(layer = layers,
                       upper = upper,
                       lower = lower)

  #A local linear approach using a linear regession model within each layer
  df_ret <- lapply(1:length(upper), function(i){
    df_part <- df[df$depth <= upper[i] &
                    df$depth >= lower[i], ]

mod <- stats::lm(x_ppm ~depth, data = df_part)
    dcdz <- as.numeric(stats::coef(mod)[2]) * 100 #gradient in ppm/m
    dc <- dcdz * (abs(diff(c(upper[i], lower[i]))) / 100)

    suppressWarnings(mod_summary <- summary(mod))
    #error of gradient in ppm/m
    dcdz_sd <- as.numeric(mod_summary$coefficients[2,2])*100
    r2 <- mod_summary$r.squared

    if (abs(dcdz/100) < 1E-9){
      dcdz <- 0
      dc <- 0
      dcdz_sd <- NA
      r2 <- NA
    }

    df_ret <- data.frame(dcdz_ppm = dcdz,
                         dcdz_sd = dcdz_sd,
                         dc_ppm = dc,
                         r2 = r2)
    return(df_ret)
  }) %>%
    do.call(what = rbind) %>%
    cbind.data.frame(df_ret)



  if(all(is.na(df_ret))){
    return_na <- TRUE
  } else {
    create_return <- FALSE

  }



} else if (mode == "EF"){

  if(nrow(df)>1){
  starts<-stats::coef(stats::lm(x_ppm~I((depth-min(depths))^1.5),data=df))
  } else {
    starts <- NA
  }
  #If all values are basically the same,
  #problems arise, this checks for the relative
  #difference of all values being less than 1e-10
  sing_flag <- mean(abs(na.omit(df$x_ppm)-mean(df$x_ppm,na.rm=TRUE)))/
    mean(df$x_ppm,na.rm=TRUE) < 1e-10

  if(anyNA(starts) | nrow(df)<4 | sum(starts)==0 | sing_flag){
    #preventing model errors before they occur and replacing with NA
    return_na <- TRUE
  } else {
    #exponential model

    mod <- try(stats::nls(x_ppm~(starts[1]+(b*((depth-min(depths))^c))),
               data = df,
               start = list(#"a"=as.numeric(starts[1]),
                            "b"=as.numeric(starts[2])*stats::rnorm(1,1,0.01),
                            "c"=1.1*stats::rnorm(1,1,0.01)),
               algorithm = "plinear",
               control = stats::nls.control(warnOnly = TRUE)),silent = TRUE)

    #check for convergence
    conv_flag <- ifelse(inherits(mod, "nls"),
                        conv_flag <- !mod$convInfo$isConv,FALSE)

    #na if no convergence or error
    if (conv_flag | inherits(mod, "try-error")){
      return_na <- TRUE
    } else {
      #a <- coef(mod)[1]
      b <- stats::coef(mod)[1]
      c <- stats::coef(mod)[2]
      d <- lower-diff(depths)/2-min(depths)

      db <- summary(mod)$coefficients[1,2]
      dc <- summary(mod)$coefficients[2,2]
      #calculating dcdz via the 1st derivative of the exponential Function.
      dcdz <- (c*b)*(d)^(c-1) *100 #in ppm/m
      dcdz_sd <-(b*d^(c-1) * (c*log(d)+1) * dc) +(c*d^(c-1)*db) *100 #in ppm/m
      dc<--diff(starts[1]+(b*((depths-min(depths))^c)))
      r2 <- summary(stats::lm(x_ppm ~ I(starts[1]+(b*(depth-min(depths))^c)),
                              data=df))$r.squared

      create_return <- TRUE
    }
  }

}else if (mode == "DA"){

  if(nrow(df)>1){
    y_min <- mean(df$x_ppm[df$depth == max(depths)])
    y_max <- mean(df$x_ppm[df$depth == min(depths)])
    d_max <- max(depths)
    starts<-stats::coef(
      stats::lm(I(log(1 - (x_ppm - y_min)/y_max)) ~ 0 +I(-(d_max - depth)),
                data=df))
    starts <- c(y_max, starts)
    } else {
    starts <- NA
  }

  if(anyNA(starts) | nrow(df)<4 | sum(starts)==0){
    #preventing model errors before they occur and replacing with NA
    return_na <- TRUE
  } else {
    #exponential model based on Davidson 2006


    mod <- try(stats::nls(
      x_ppm ~ b  * (1 - exp(-a*I(d_max - depth))) + I(y_min),
      data = df,
      start = list(
        "a"= starts[2],
        "b"= starts[1]),
      algorithm = "plinear",
      control = stats::nls.control(warnOnly = TRUE, maxiter = 50)),
      silent = TRUE)

    #check for convergence
    conv_flag <- ifelse(inherits(mod, "nls"),
                        conv_flag <- !mod$convInfo$isConv,FALSE)

    #na if no convergence or error
    if (conv_flag | inherits(mod, "try-error")){
      return_na <- TRUE
    } else {
      b <- stats::coef(mod)[2]
      a <- stats::coef(mod)[1]
      d <- d_max - (upper+lower)/2

      sum_mod <- try(summary(mod))
      if(inherits(sum_mod, "try-error")){
        da <- NA
        db <- NA
      } else {
        da <- summary(mod)$coefficients[1,2]
        db <- summary(mod)$coefficients[2,2]
      }
      #calculating dcdz via the 1st derivative of the exponential Function.
      dcdz <- -a*b*exp(-a*d) *100 #in ppm/m
      dcdz_sd <- (abs(db*a*exp(-a*d)) +
                    abs(da*(b-a*b*d)*exp(-a*d)))  *100 #in ppm/m
      dc<- ((b*exp(-a*upper)) - (b*exp(-a*lower)))
      r2 <- 1 -(stats::deviance(mod) /
                  (stats::var(df$x_ppm)*(length(df$x_ppm)-1)))

      create_return <- TRUE
    }
  }
}

if (return_na){
  dcdz <- NA
  dc <- NA
  dcdz_sd <- NA
  r2 <- NA

  create_return <- TRUE


}
if (create_return){
  df_ret <- data.frame(layers_map,
                       dcdz_ppm = dcdz,
                       dcdz_sd = dcdz_sd,
                       dc_ppm = dc,
                       r2 = r2)
}

cbind(df_ret, gd_id = gd_id)
}



