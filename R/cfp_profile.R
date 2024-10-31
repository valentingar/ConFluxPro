#' @title Make profiles
#'
#' @description
#' A central S3 class that defines a \code{data.frame} where columns given in
#' \code{id_cols} define distinct soil profiles.
#'
#' @param x A \code{data.frame}
#'
#' @param id_cols Column names in data.frame that uniquely identify each profile.
#'
#' @family data formats
#'
#' @returns A \code{cfp_profile} object. This is a \code{data.frame}
#' with the \code{id_cols} attribute.
#'
#' @examples
#' df <- data.frame(
#'   site = rep(c("site_a", "site_b"), each = 2),
#'   variable = 1:4)
#'
#' cfp_profile(df, id_cols = "site")
#'
#' ### multiple id_cols
#' df <- data.frame(
#'   site = rep(c("site_a", "site_b"), each = 4),
#'   replicate = rep(c(1,2), times = 4),
#'   variable = 1:8)
#'
#' cfp_profile(df, id_cols = c("site", "replicate"))

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
  id_cols <- cfp_id_cols(x)
  stopifnot("id_cols must be unique!" = length(unique(id_cols)) == length(id_cols))
  id_cols_present <- id_cols %in% names(x)

  if (any(!id_cols_present)){
    stop(paste0("missing id_cols ", paste0(id_cols[!id_cols_present], collapse = " ")))
  }

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

# SUBSETTING
#' @export
'[.cfp_profile' <- function(x,
                            ...){
  id_cols <- cfp_id_cols(x)
  x_class <- class(x)
  x_class <- x_class[-match(class(new_cfp_profile(data.frame(), id_cols = NULL)),
                           x_class)]
  x <- data.frame(x)
  x <- x[...]
  id_cols <- id_cols[id_cols %in% names(x)]
  if(length(id_cols) == 0) id_cols <- NULL

  new_cfp_profile(x,
                  id_cols = id_cols,
                  class = x_class)
}
