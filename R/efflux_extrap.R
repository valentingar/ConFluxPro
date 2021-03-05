#' @title efflux_extrap
#'
#' @description This function extrapolates fluxes calculated with the layers approach to
#' the surface with different methods.
#' To see the different approaches implemented check the "method" parameter below. The values are grouped by
#' Plot,Date,gas,mode.
#'
#' @param FLUX (dataframe) the FLUX dataframe
#' @param gases (character vector) A (vector) of gases to be extrapolated as stated
#' in the function call. Fefaults to all gases.
#' @param method (character) A string defining the method to be used for extrapolation.
#' Must be called multiple times for different methods. \n
#' Possible methods are: \n
#' "lm" implements a linear model approach. Here a linear model is fit to
#' flux ~ depth and the value for the surface estimated \n
#' "linextrap" implements a linear extrapolation approach.
#' Here, two layer names must be given using the control parameters below,
#' that will then be used for an linear extrapolation.
#' This can be used to implement a Hirano et al. (2003) or Tang et al. (2005) approach (see Maier & Schack_Kirchner (2014) below).
#'
#' @param layers (character vector) layers to be used in the linextrap-approach.
#' @param modename (character) A character defining the value of the variable "mode" in the returned dataframe.
#' @param id_cols (character) A character vector defining the columns that uniquely identify one profile.
#'
#' @return EFFLUX
#' @family FLUX
#' @examples efflux_extrap(FLUX,
#'                         gases = c("CO2","O2"),
#'                         method = "linextrap",
#'                         layers = c("MIN1","MIN2"),
#'                         modename = "Hirano";
#'                         id_cols = c("Date"))
#'
#'
#'
#'
#' @references M. Maier, H. Schack-Kirchner,
#' Using the gradient method to determine soil gas flux: A review,
#' Agricultural and Forest Meteorology,Volumes 192–193,2014,Pages 78-95, https://doi.org/10.1016/j.agrformet.2014.03.006.
#'
#' @import dplyr
#' @export

efflux_extrap <-function(FLUX,
                         gases = "all",
                         method,
                         layers = NA,
                         modename = NA,
                         id_cols){
  valid_methods <- c("lm","linextrap","nearest")

  if (!method %in% valid_methods){
    stop(paste0("invalid method! Please choose one of the following: ", valid_methods))
  }

  if (method == "linextrap"){
    if(!length(layers) == 2){
      stop(paste0("invalid number of layers! Must be 2!"))
    }
    if(all(layers %in% unique(FLUX$layer))==F){
     l_nv <- layers[!layers %in% FLUX$layer]
      stop(paste0("The following layers are not present in the FLUX dataframe provided! ",l_nv))
    }
  }
  if(method == "nearest"){
    if(!length(layers)==1){
      stop(paste0("invalid number of layers for method 'nearest'. Must be 1!"))
    }
  }
  if(!gases[1] == "all"){
    if (all(gases %in% unique(FLUX$gas))){
      FLUX <- FLUX %>% dplyr::filter(gas %in% !!gases)
    } else {
      stop(paste("The following gases are not present in the FLUX tibble: ",paste0(gases[!gases %in% unique(FLUX$gas)],collapse = ", ") ))
    }
  } else if (!length(gases)==1){
    stop("gases set to all but does not have length 1. Check input.")
  }

  if(!all(id_cols %in% names(FLUX))){
    stop("Not all id_cols are present in FLUX.")
  }

  id_cols <- c(id_cols,"gas","mode","Plot")


if(method == "lm"){
  EFFLUX <- FLUX %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({id_cols}))) %>%
    dplyr::group_modify(~{
      h<-.x$topheight[1]
      #print(paste(.y$Plot,.y$Date))
      if (nrow(.x %>% dplyr::filter(is.na(depth)==F,is.na(flux)==F))<2){
        efflux <- NA
      } else {
      mod <- lm(flux~depth,data = .x)
      efflux <- predict(mod,newdata = list(depth = h))
      }
      return(data.frame(efflux = efflux))
    })
} else if (method == "linextrap"){
  EFFLUX <- FLUX %>%
    dplyr::filter(layer %in% layers) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({id_cols}))) %>%
    dplyr::group_modify(~{
      #print(paste(.y$Plot,.y$Date,.y$gas,.y$mode))
      #print(.x$depth)
      #print(.x$flux)
      #print(.x$layer)
      h<-.x$topheight[1]
      efflux <- lin_extrap(.x$depth,.x$flux,h)
      df <- data.frame(efflux=efflux)
      return(df)
      })

} else if (method == "nearest"){
  EFFLUX <- FLUX %>% dplyr::filter(layer == layers) %>% dplyr::rename(efflux = flux)
}
  if (is.na(modename)==T){
    modename<-paste0(na.omit(c(method,layers)),collapse = "_")
  }
  EFFLUX$extrapmode <- modename
return(EFFLUX)
}

