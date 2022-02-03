#' @title cfp_layers_map
#'
#' @description A function to conveniently create a vaild cfp_layers_map object
#' with options to add more gases and corresponding parameters for \code{\link{pro_flux()}} models.
#' Please note that only \code{layers_map} and code{id_cols} are obligatory. Adding the necessary
#' columns to \code{layers_map} beforehand allows for more fine-tuned control of models.
#'
#' @param layers_map (dataframe) That defines the layers of homogeneous
#'   production as well as the upper and lower limits of production rate. \ Must
#'   include
#'   \itemize{
#'   \item the relevant id_cols (see below)
#'   \item the upper
#'   and lower boundaries of each layer (upper, lower)
#'   \item upper and lower
#'   limits of the production rate to be modeled in \eqn{\mu mol m^{-3}}
#'   (highlim, lowlim)
#'   \item the parameter layer_couple, that indicates how
#'   strongly the layer should be linked to the one below it (0 for no coupling)
#'   }
#' @param id_cols (character vector) The names of the columns that together
#'   uniquely identify one profile.
#' @param gas (character vector) A vector of gas names to be added to layers_map.
#' The input layers_map is then repeated for each gas.
#' @param lowlim (numeric vector) A vector the same length as gas with the lower limit
#' of possible production allowed in \code{pro_flux()} models.
#' @param highlim (numeric vecotr)  A vector the same length as gas with the upper limit
#' of possible production allowed in \code{pro_flux()} models.
#' @param layer_couple (numeric_vector) A vector the same length as gas that indicates how
#' strongly the layer should be linked to the one below it (0 for no coupling)
#'
#'
#' @export
cfp_layers_map <- function(layers_map,
                           id_cols,
                           gas = NULL,
                           lowlim = NULL,
                           highlim = NULL,
                           layer_couple = NULL
){

  stopifnot("layers_map must be a data frame!" = is.data.frame(layers_map))
  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("\nadded 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  #convenient way of adding multiple gases to layers_map
  if (!is.null(gas)){

    stopifnot("gas must not be present in layers_map already!" = !"gas" %in% names(layers_map))
    stopifnot("gas must be a character (-vector)!" = is.character(gas))
    stopifnot("gas must contain unique values only!" = length(gas) == length(unique(gas)))

    layers_map <-
      lapply(gas, function(i){
        layers_map$gas <- i
        layers_map
      }) %>%
      dplyr::bind_rows()


    layers_map <- add_if_missing(layers_map,
                                 gas,
                                 lowlim = lowlim)

    layers_map <- add_if_missing(layers_map,
                                 gas,
                                 highlim = highlim)
    layers_map <- add_if_missing(layers_map,
                                 gas,
                                 layer_couple = layer_couple)

  }


  #automated adding of "layer" column
  if(!"layer" %in% names(layers_map)){
    layers_map <-
      layers_map %>%
      dplyr::arrange(upper) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(layer = 1:n())

    message("automatically added 'layer' column")
  }

  layers_map <-
  layers_map %>%
    dplyr::arrange(upper) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(pmap = dplyr::row_number())

  x <- new_cfp_layers_map(layers_map,
                          id_cols)

  x <- validate_cfp_layers_map(x)

  x
}


#constructor
new_cfp_layers_map <- function(layers_map,
                               id_cols){

  x <- structure(layers_map,
                 id_cols = id_cols,
                 class = c("cfp_layers_map","data.frame"))
  x
}

#validator
validate_cfp_layers_map <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_layers_map"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("upper","lower","layer","lowlim","highlim","layer_couple","gas")
  id_cols <- cfp_id_cols(x)

  error_if_missing(x, c(base_cols,id_cols))

  # is the data.frame upper/lower consistent?
  stopifnot("layers_map must be unique and upper/lower consitent" =
              is_ul_consistent(x,id_cols = cfp_id_cols(x)))

  x

}



#helpers------------


add_if_missing  <- function(df,
                            gas,
                            ...){


  obj_list <- list(...)
  stopifnot(length(obj_list) == 1)
  obj_name <- names(obj_list)[1]
  obj <- obj_list[[1]]

  if (!is.null(obj)){
    if(obj_name %in% names(df)){
      stop(paste0(obj_name," must not be present in layers_map already!"))
    }
    if(!(length(obj) == length(gas)  |  length(obj) == 1)){
      stop(paste0(obj_name," must be length 1 or the same as gas"))
    }
    stopifnot(is.numeric(obj))

    if (length(obj) == 1){
      df[[obj_name]] <- obj
    } else {
      df <-
        lapply(seq_along(gas), function(i){
          df_part <- df[df$gas == gas[i],]
          df_part[[obj_name]] <- obj[i]
          df_part
        }) %>%
        dplyr::bind_rows()
    }
  } else {
    # columns must still be present but are NA instead
    df[[obj_name]] <- NA
  }

  df
}


error_if_missing <- function(df,cols){
  lapply(cols, function(i) if(!i %in% names(df)) stop(paste0(i, " must be present in layers_map!")))
}

# methods -----------

#' @exportS3Method
print.cfp_layers_map <- function(x){
  cat("\nA cfp_layers_map object \n")
  id_cols <- cfp_id_cols(x)
  cat("id_cols:", id_cols, "\n")
  cat("\n")
  NextMethod()
}
