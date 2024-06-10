#' @title cfp_layered_profile
#'
#' @description
#' A subclass of \code{\link{cfp_profile}()} where each profile consists of
#' layers that are defined by their \code{upper} and \code{lower}
#' boundary without gaps or duplicates.
#'
#' @param x A \code{data.frame} with columns \code{upper} and \code{lower}.
#'
#' @param id_cols Column names in x that uniquely identify each profile.
#'
#' @details \code{upper} and \code{lower} define the upper and lower bounds of
#' each layer in cm. Higher values lay on top of lower values.
#'
#'
#' @export

cfp_layered_profile <- function(
    x,
    id_cols = NULL){

  x <- as.data.frame(x)

  x <- new_cfp_layered_profile(
    x = x,
    id_cols = id_cols
  )

  x <- validate_cfp_layered_profile(x)
  x
}

# constructor

new_cfp_layered_profile <- function(
    x,
    class = "cfp_layered_profile",
    id_cols){

  x <- new_cfp_profile(x = x,
                       id_cols = id_cols,
                       class = class)

  x
}


validate_cfp_layered_profile <- function(x){

  validate_cfp_profile(x)
  stopifnot("The data.frame must have the columns 'upper' and 'lower'" =
              all(c("upper", "lower") %in% names(x)))
  stopifnot("Each unique profile must be layered (by columns 'upper' and 'lower') without gaps or duplicates" = is_ul_consistent(x, cfp_id_cols(x)))

  x
}

# PRINTING
#' @exportS3Method
print.cfp_layered_profile <- function(x, ...){
  NextMethod()
}
