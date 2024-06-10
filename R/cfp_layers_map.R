#' @title cfp_layers_map
#'
#' @description A function to create a cfp_layers_map object that defines the
#' layers of both [fg_flux()] and [pro_flux()] models.
#'
#' @param layers_map (data.frame) That defines the layers for which the
#' production or flux is modeled. Note that some parameters can also be provided
#' directly to the function call instead (see Details).
#'   \itemize{
#'   \item \code{id_cols} the relevant id_cols (see below)
#'   \item \code{gas}, the gas that is modelled.
#'   \item \code{upper}, \code{lower} the upper and lower boundaries
#'   of each layer
#'   \item \code{lowlim}, \code{highlim} as the lower and upper limits of the
#'   production rate to be modeled in \eqn{\mu~mol~m^{-3}}
#'   \item the parameter \code{layer_couple}, that indicates how
#'   strongly the layer should be linked to the one below it (0 for no coupling)
#'   }
#' @inheritParams cfp_profile
#' @param gas (character vector) of gas names to be added to
#' layers_map. The input layers_map is then repeated for each gas.
#' @param lowlim (numeric vector) the same length as \code{gas} with the
#' lower limit of possible production allowed in [pro_flux()] models.
#' @param highlim (numeric vector)  the same length as gas with the
#' upper limit of possible production allowed in [pro_flux()] models.
#' @param layer_couple (numeric_vector) A vector the same length as gas that
#' indicates how strongly the layer should be linked to the one below it
#' (0 for no coupling, the default).
#'
#' @details
#' # Add lowlim and highlim for multiple gases
#' Sometimes it is practical to model different gases with different limits.
#' For example, it is a reasonable assumption that CO2 is not consumed in
#' relevant amounts in most soils, whereas CH4 may be both produced or consumed.
#' Therefore we may want to limit production rates of CO2 to only positive
#' values, whereas allowing for negative CH4 production rates (i.e. consumption)
#' as well.
#'
#' To make this setup easy, you can provide a \code{gas} vector to the function
#' together with \code{highlim} and \code{lowlim} vectors of the same length.
#' The provided \code{layers_map} \code{data.frame} will then be replicated for
#' each gas with the respective values of the production limits provided.
#'
#' @returns A [cfp_layered_profile()] \code{data.frame} with the columns
#' described above as well as \code{layer} and \code{pmap} columns that identify
#' each layer with an integer (ascending from bottom to top).
#'
#' @examples
#' df <- data.frame(
#'   site = rep(c("site_a", "site_b"), each = 2),
#'   upper = c(5, 0, 7, 0),
#'   lower = c(0, -100, 0, -100))
#'
#' cfp_layers_map(df,
#'   id_cols = "site",
#'   gas = c("CO2", "CH4"),
#'   lowlim = c(0, -1000),
#'   highlim = c(1000, 1000),
#'   layer_couple = 0
#' )
#' @export
cfp_layers_map <- function(layers_map,
                           id_cols,
                           gas = NULL,
                           lowlim = NULL,
                           highlim = NULL,
                           layer_couple = 0
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

  } else if (!("gas" %in% names(layers_map))){
    stop("Please provide 'gas' parameter either as variable in layers_map or directly to this function.")
  } else if (!is.null(lowlim) | !is.null(highlim)){
    message("'lowlim' and/or 'highlim' ignored because 'gas' argument missing")
  }


  #automated adding of "layer" column
  if(!"layer" %in% names(layers_map)){
    layers_map <-
      layers_map %>%
      dplyr::arrange(upper) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(layer = 1:dplyr::n())

    #message("automatically added 'layer' column")
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

  x <- new_cfp_layered_profile(
    layers_map,
    id_cols = id_cols,
    class = "cfp_layers_map")
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
print.cfp_layers_map <- function(x, ...){
  NextMethod()
}
