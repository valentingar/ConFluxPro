#' Get the unique gases of an object
#' @description
#' Get the gases from a CFP object.
#'
#' @param x the object to extract the gases from.
#'
#' @returns A character vector of gases in that object.
#'
#' @export

unique_gases <- function(x){
  UseMethod("unique_gases")
}

#' @exportS3Method
unique_gases.cfp_dat <- function(x){
  x <- unique(x$profiles$gas)
  x
}

#' @exportS3Method
unique_gases.cfp_profile <- function(x){
  x_id_cols <- cfp_id_cols(x)
  if (!("gas" %in% x_id_cols)){
    stop("'gas' is not an id_col of this object.")
  } else {
    x <- unique(x$gas)
  }
  x
}

