#' @title dcdz_layered
#'
#' @description This function calculates concentration gradients using different approaches.
#'
#'
#' @param df (dataframe) the gasdata dataframe, filtered to one profile (e.g. 1 day & one Plot ).
#' @param layers_map (dataframe) containing the following parameters: "layer"=name of the layer;
#' "upper"=upper limit of layer in cm; "lower" = lower limit of the layer in cm;
#' @param mode (character) One of ("LL","LS","EF").
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
#' @examples
#'
#' @import dplyr
#' @import splines
#'
#' @export

dcdz_layered <- function(df,
                         layers_map,
                         mode
                         ){
valid_modes = c("LS","LL","EF")

if ((mode %in% valid_modes)==F){
  stop(paste0("wrong mode selected: ",mode,". Please use one of the following modes: ",valid_modes))
}

layers_map <- layers_map %>%
  dplyr::arrange(dplyr::desc(upper))

upper <- layers_map$upper
lower <- layers_map$lower
layers <- layers_map$layer


depth_steps <- rev(sort(upper))[-1] #depths between the layers from top to bottom
depths <- rev(sort(unique(c(upper, lower)))) #depths including boundaries from top to bottom

#depths with values in the df
depths_df <- df %>% dplyr::filter(is.na(depth)==F,
                     is.na(NRESULT_ppm)==F) %>%
  dplyr::pull(depth) %>% unique() %>%sort() %>% rev()

#true if the there are depths that exceed depths of non-NA values in df.
ls_flag <- length(which(depths >max(depths_df,na.rm=T) | depths <min(depths_df,na.rm=T)))>0

#turns Inf-values to NA
df$NRESULT_ppm[is.infinite(df$NRESULT_ppm)==T] <- NA


if (mode == "LS"){

#not enough non-NA values or outside range.
if (nrow(df[all(is.na(df$depth)==F,is.na(df$NRESULT_ppm)==F),])<length(depths) | ls_flag){
  dc <-NA
  dcdz <-NA
  dcdz_sd <-NA
  r2 <- NA
} else {
#spline model
mod<-lm(NRESULT_ppm~bs(depth,knots=depth_steps,#depth_steps,
                       degree = 1
),
data=df)

# dc = the difference in concentration in ppm
dc <- -diff(predict(mod,newdata = data.frame(depth =depths)))
# dcdz = the concentration gradient in ppm/m
dcdz <- dc / -diff(depths) *100 #from cm^-1 to m^-1


# error estimate in ppm/m:
dcdz_sd <- -rev(as.numeric(summary(mod)$coefficients[,2][-1]+c(0,lag(summary(mod)$coefficients[,2][-1],1)[-1]))) /diff(depths) *100
if (!length(dcdz_sd)==length(layers)){
  dcdz_sd <- rep(NA,length(layers))
}

#r_squared
r2 <- summary(mod)$r.squared
}
#create return table
df_ret<- data.frame(mode = rep(mode),
           layer = layers,
           upper = upper,
           lower = lower,
           dcdz_ppm = dcdz,
           dcdz_sd = dcdz_sd,
           dc_ppm = dc,
           r2 = rep(r2))

} else if (mode == "LL"){

  #This implements a local linear approach using a linear regession model within each layer
  df_ret <- lapply(seq(upper), function(i){
    df_part <- df %>% dplyr::filter(depth <= upper[i],
                                    depth >= lower[i])
    mod <- lm(NRESULT_ppm ~depth, data = df_part)
    dcdz <- as.numeric(coef(mod)[2])*100 #gradient in ppm/m
    dc <- dcdz * (abs(diff(c(upper[i],lower[i]))) / 100)
    dcdz_sd <- as.numeric(summary(mod)$coefficients[2,2])*100 #error of gradient in ppm/m
    r2 <- summary(mod)$r.squared

    df_ret <- data.frame(mode = mode,
                         layer = layers[i],
                         upper = upper[i],
                         lower = lower[i],
                         dcdz_ppm = dcdz,
                         dcdz_sd = dcdz_sd,
                         dc_ppm = dc,
                         r2 = r2)
    return(df_ret)
  }) %>%
    dplyr::bind_rows()

} else if (mode == "EF"){

  starts<-coef(lm(NRESULT_ppm~I((depth-min(depths))^1.1),data=df))

  if(anyNA(starts) | nrow(df)<4 | sum(starts)==0){ #preventing model errors before they occur and replacing with NA
    dcdz <- NA
    dc <- NA
    dcdz_sd <- NA
    r2 <- NA
  } else {
    #print(starts)

    #exponential model
    mod <- nls(NRESULT_ppm~(starts[1]+(b*((depth-min(depths))^c))),
               data = df,
               start = list(#"a"=as.numeric(starts[1]),
                            "b"=as.numeric(starts[2]),
                            "c"=1.1),
               algorithm = "plinear")

    #a <- coef(mod)[1]
    b <- coef(mod)[1]
    c <- coef(mod)[2]
    d <- lower-diff(depths)/2-min(depths)

    db <- summary(mod)$coefficients[1,2]
    dc <- summary(mod)$coefficients[2,2]

    #calculating dcdz via the 1st derivative of the exponential Function.
    dcdz <- (c*b)*(d)^(c-1) *100 #in ppm/m
    dcdz_sd <-(b*d^(c-1) * (c*log(d)+1) * dc) +(c*d^(c-1)*db) *100 #in ppm/m
    dc<--diff(starts[1]+(b*((depths-min(depths))^c)))
    r2 <- summary(lm(NRESULT_ppm ~ I(starts[1]+(b*(depth-min(depths))^c)),data=df))$r.squared
  }

  df_ret <- data.frame(mode = mode,
                       layer = layers,
                       upper = upper,
                       lower = lower,
                       dcdz_ppm = dcdz,
                       dcdz_sd = dcdz_sd,
                       dc_ppm = dc,
                       r2 = r2)
}

return (df_ret)
}



