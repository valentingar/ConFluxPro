#' @title dcdz_ef
#'
#' @description This function calculates concentration gradients for complete profiles with different approaches.
#'
#'
#' @param df (dataframe) the gasdata dataframe, filtered to one profile (e.g. 1 day & one Plot).
#' @param topheight (numeric) Depth at the Atmosphere/Soil interface. (e.g. Height of Humus).
#' @param mode (character) how should the concentration gradient be calculated?
#' One of ("EF","LR"). \n
#' "EF" fits an exponential model of form a+b*depth^c to the data and takes the corresponding gradient from the derivative
#' at the depth defined in "topheight" \n
#' "LR" fits a linear regression model of form a*depth+c to the data and takes a as gradient.
#'
#'
#' @return df (dataframe) with the following columns:
#' @return mode (character) the used gradient method.
#' @return topheight (numeric) the topheight of that profile
#' @return dcdz_ppm (numeric) concentration gradient in ppm/cm.
#' @return dcdz_sd (numeric) standard error in ppm/cm.
#' @return r2 (linearised) R^2 for the model.
#'
#' @examples
#' @family FLUX
#' @import dplyr
#' @import splines
#'
#' @export


dcdz_ef <- function(df,
                    topheight,
                    mode){

  valid_modes <- c("EF","LR")
  if (!mode %in% valid_modes){
    stop(paste0("wrong mode selected: ",mode,". Please use one of the following modes: ",paste0(valid_modes,collapse = ", ")))
  }


  #remove NA rows
  df <- df %>% dplyr::filter(is.na(depth)==F,
                             is.na(NRESULT_ppm)==F)

  #initialize return data frame
  df_ret <- data.frame(mode = mode,
                       topheight = topheight)

  #initialize na return logical
  return_na <- F

  if (mode == "LR"){

    if (nrow(df) < 2){
      return_na <- T
    } else{

      mod <- lm(NRESULT_ppm ~depth, data = df)
      dcdz <- as.numeric(coef(mod)[2])*100 #gradient in ppm/m
      dcdz_sd <- as.numeric(summary(mod)$coefficients[2,2])*100 #error of gradient in ppm/m
      r2 <- summary(mod)$r.squared
    }
  }else if (mode == "EF"){

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
        d <- topheight

        db <- summary(mod)$coefficients[1,2]
        dc <- summary(mod)$coefficients[2,2]
        #print(db)
        #print(dc)
        #calculating dcdz via the 1st derivative of the exponential Function.
        dcdz <- (c*b)*(d)^(c-1) *100 #in ppm/m
        dcdz_sd <-(b*d^(c-1) * (c*log(d)+1) * dc) +(c*d^(c-1)*db) *100 #in ppm/m
        r2 <- summary(lm(NRESULT_ppm ~ I(starts[1]+(b*(depth-min(depths))^c)),data=df))$r.squared

      }

    }
  }
  if (return_na){
    dcdz <- NA
    dcdz_sd <- NA
    r2 <- NA
  }

  df_ret <- cbind.data.frame(df_ret,
                             dcdz_ppm = dcdz,
                             dcdz_sd = dcdz_sd,
                             r2 = r2)

  return (df_ret)
}
