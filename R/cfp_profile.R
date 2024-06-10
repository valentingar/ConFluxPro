#' @title cfp_profile
#'
#' @description
#' A central S3 class that defines a \code{data.frame} where columns given in
#' \code{id_cols} define distinct soil profiles.
#'
#' @param x A \code{data.frame}
#'
#' @param id_cols Column names in x that uniquely identify each profile.
#'
#'
#' @export

cfp_profile <- function(
    x,
    id_cols = NULL){

  x <- as.data.frame(x)

  x <- new_cfp_profile(
    x = x,
    id_cols = id_cols
  )

  x <- validate_cfp_profile(x)
  x
}

# constructor

new_cfp_profile <- function(x,
                            ...,
                            class = character(),
                            id_cols){

  x <- structure(x,
                 class = c(class, "cfp_profile", "data.frame"),
                 id_cols = id_cols
  )

  x
}


validate_cfp_profile <- function(x){

  stopifnot(inherits(x, c("cfp_profile", "data.frame")))
  stopifnot(all(cfp_id_cols(x) %in% names(x)))

  x
}


# PRINTING
#' @exportS3Method
print.cfp_profile <- function(x, ...){
  main_class <- class(x)[1]
  cat("\nA", main_class, "object \n")
  print_id_cols(x)
  cat("\n")
  NextMethod()
}

