#' @title Get parameter descriptions and units
#'
#' @description Function to access parameter descriptions and units
#' used in \code{ConFluxPro}
#'
#' @param x Any object or data.frame to match the parameters to, or a character
#' vector of parameter names.
#'
#' @returns A \code{data.frame()} with the name, description and unit of the
#' parameter
#'
#' @examples
#' #list parameters within an object
#' cfp_parameter(soilphys)
#' cfp_parameter(gasdata)
#'
#' #list all paramters
#' cfp_parameter()
#'
#'
#' @export
#'

cfp_parameter <- function(x = NULL){
  UseMethod("cfp_parameter")
}

#' @exportS3Method
cfp_parameter.default <- function(x = NULL){
  y <- parameter

  if (is.null(x)){
    return(y)
  }

  cat("\nThis object contains the following parameters:\n")
  y[y$name %in% names(x), ]
}

#' @exportS3Method
cfp_parameter.character <- function(x){
  y <- parameter

  y[y$name %in% x, ]
}
