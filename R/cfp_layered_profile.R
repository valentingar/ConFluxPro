#' @title Object for layered soil profiles
#'
#' @description
#' A subclass of \code{\link{cfp_profile}()} where each profile consists of
#' layers that are defined by their \code{upper} and \code{lower}
#' boundary without gaps or duplicates.
#'
#' @param x A \code{data.frame} with columns \code{upper} and \code{lower}.
#'
#' @inheritParams cfp_profile
#'
#' @family data formats
#'
#' @returns A \code{cfp_layered_profile} object. This is a
#' \[cfp_profile()] that is further subdivided into layers by the
#' columns \code{upper} and \code{lower}.
#'
#' @details \code{upper} and \code{lower} define the upper and lower bounds of
#' each layer in cm. Higher values lay on top of lower values.
#'
#' @examples
#' df <- data.frame(
#'   site = rep(c("site_a", "site_b"), each = 2),
#'   upper = c(10, 0, 7, 0),
#'   lower = c(0, -100, 0, -100),
#'   variable = 1:4)
#'
#' cfp_layered_profile(df, id_cols = "site")

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
    class = character(),
    id_cols){

  x <- new_cfp_profile(x = x,
                       id_cols = id_cols,
                       class = c(class,"cfp_layered_profile"))

  x
}


validate_cfp_layered_profile <- function(x){

  validate_cfp_profile(x)
  stopifnot("The data.frame must have the columns 'upper' and 'lower'" =
              all(c("upper", "lower") %in% names(x)))
  stopifnot("Each unique profile must be layered
            (by columns 'upper' and 'lower') without gaps or duplicates" =
              is_ul_consistent(x, cfp_id_cols(x)))

  x
}

# PRINTING
#' @exportS3Method
print.cfp_layered_profile <- function(x, ...){
  NextMethod()
}
