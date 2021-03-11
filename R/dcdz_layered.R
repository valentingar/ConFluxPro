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
#' @family FLUX
#' @import dplyr
#' @import splines
#'
#' @export

dcdz_layered <- function(df,
                         layers_map,
                         mode,
                         depth_steps
                         ){
valid_modes = c("LS","LL","EF")

if ((mode %in% valid_modes)==F){
  stop(paste0("wrong mode selected: ",mode,". Please use one of the following modes: ",paste0(valid_modes,collapse = ", ")))
}

upper <- layers_map$upper
lower <- layers_map$lower
layers <- layers_map$layer


depths <- rev(sort(unique(c(upper, lower)))) #depths including boundaries from top to bottom

#depths with values in the df
depths_df <- rev(sort(unique(df$depth)))

#true if the there are depths that exceed depths of non-NA values in df.
ls_flag <- length(which(depths >max(depths_df,na.rm=T) | depths <min(depths_df,na.rm=T)))>0

#initializing NA-logical for return
return_na <- F

if (mode == "LS"){

#not enough non-NA values or outside range.
if (nrow(df)<length(depths) | ls_flag){
  return_na <- T
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
create_return <- T

} else if (mode == "LL"){

  #This implements a local linear approach using a linear regession model within each layer
  df_ret <- lapply(1:length(upper), function(i){
    df_part <- df %>% dplyr::filter(depth <= !!upper[i],
                                    depth >= !!lower[i])
    df_ret <- data.frame(mode = mode,
                         layer = layers[i],
                         upper = upper[i],
                         lower = lower[i])


    df_valid <- df_part %>% dplyr::filter(!is.na(NRESULT_ppm),!is.na(depth))
    d_flag <- (df_valid  %>% dplyr::pull(depth) %>% unique() %>%length() <2)
    val_flag <- (df_valid %>% nrow() < 2)

    if (any(d_flag,val_flag)){
      return(df_ret)
    } else{
    mod <- lm(NRESULT_ppm ~depth, data = df_part)
    dcdz <- as.numeric(coef(mod)[2])*100 #gradient in ppm/m
    dc <- dcdz * (abs(diff(c(upper[i],lower[i]))) / 100)
    dcdz_sd <- as.numeric(summary(mod)$coefficients[2,2])*100 #error of gradient in ppm/m
    r2 <- summary(mod)$r.squared
    }

    df_ret <- cbind.data.frame(df_ret,
                         dcdz_ppm = dcdz,
                         dcdz_sd = dcdz_sd,
                         dc_ppm = dc,
                         r2 = r2)
    return(df_ret)
  }) %>%
    dplyr::bind_rows()

  if(all(is.na(df_ret))){
    return_na <- T
  } else {
    create_return <- F

  }



} else if (mode == "EF"){

  if(nrow(df)>1){
  starts<-coef(lm(NRESULT_ppm~I((depth-min(depths))^1.1),data=df))
  } else {
    starts <- NA
    }
  #If all values are basically the same, problems arise, this checks for the relative difference of all values being less than 1e-10
  sing_flag <- mean(abs(na.omit(df$NRESULT_ppm)-mean(df$NRESULT_ppm,na.rm=T)))/mean(df$NRESULT_ppm,na.rm=T) < 1e-10

  if(anyNA(starts) | nrow(df)<4 | sum(starts)==0 | sing_flag){ #preventing model errors before they occur and replacing with NA
    return_na <- T
  } else {
    #print(df$NRESULT_ppm)
    #print(df$depth)
    #print(starts)
    #print("premod")
    #exponential model

    mod <- try(nls(NRESULT_ppm~(starts[1]+(b*((depth-min(depths))^c))),
               data = df,
               start = list(#"a"=as.numeric(starts[1]),
                            "b"=as.numeric(starts[2])*rnorm(1,1,0.01),
                            "c"=1.1*rnorm(1,1,0.01)),
               algorithm = "plinear",
               control = nls.control(warnOnly = T)),silent = T)

    #check for convergence
    conv_flag <- ifelse(class(mod) == "nls",conv_flag <- !mod$convInfo$isConv,F)

    #na if no convergence or error
    if (conv_flag | class(mod)=="try-error"){
      return_na <- T
    } else {
      #print("aftermod")
      #print(mod)
      #a <- coef(mod)[1]
      b <- coef(mod)[1]
      c <- coef(mod)[2]
      d <- lower-diff(depths)/2-min(depths)

      db <- summary(mod)$coefficients[1,2]
      dc <- summary(mod)$coefficients[2,2]
      #print(db)
      #print(dc)
      #calculating dcdz via the 1st derivative of the exponential Function.
      dcdz <- (c*b)*(d)^(c-1) *100 #in ppm/m
      dcdz_sd <-(b*d^(c-1) * (c*log(d)+1) * dc) +(c*d^(c-1)*db) *100 #in ppm/m
      dc<--diff(starts[1]+(b*((depths-min(depths))^c)))
      r2 <- summary(lm(NRESULT_ppm ~ I(starts[1]+(b*(depth-min(depths))^c)),data=df))$r.squared

      create_return <- T
    }
  }
    }

if (return_na){
  dcdz <- NA
  dc <- NA
  dcdz_sd <- NA
  r2 <- NA

  create_return <- T


}
if (create_return){
  df_ret <- data.frame(layers_map[,!names(layers_map) =="Plot"],
                       dcdz_ppm = dcdz,
                       dcdz_sd = dcdz_sd,
                       dc_ppm = dc,
                       r2 = r2)
}

return (df_ret)
}



