#'@title cfp_soilphys
#'
#' @description Create and validate cfp_soilphys objects. A cfp_soilphys object has the following
#' characteristics. Each profile is uniquely identifiable by its \code{id_cols} without duplicates.
#' The data is upper/lower consistent, meaning that the steps of each profile cover a range
#' from min(lower) to max(upper) without gaps, overlap or duplicates.
#'
#' @param soilphys A data.frame with (at least) the following columns:
#' \describe{
#' \item{upper (cm) }{The upper bound of each step.}
#' \item{lower (cm)}{The lower bound of each step.}
#' \item{gas}{The gas of that step.}
#' \item{DS (\eqn{m^2 s^-1})}{The specific diffusion coefficient of that gas in that step.}
#' \item{c_air (\eqn{mol m^-3})}{The number density of air in that step.}
#' \item{any of \code{id_cols}}{All id_cols that identify one profile uniquely.}
#'
#' }
#'
#'
#' @inheritParams cfp_layers_map
#'
#' @return cfp_soilphys
#'
#' @export
#helper
cfp_soilphys <- function(x,
                         ...){
  UseMethod("cfp_soilphys")
}

#'@rdname cfp_soilphys
#'@exportS3Method
cfp_soilphys.cfp_dat <- function(
    x,
    ...){
  rlang::check_dots_empty(...)

  x <- get_soilphys(x)
  x
}


#'@rdname cfp_soilphys
#'@exportS3Method
cfp_soilphys.data.frame <- function(
    x,
    id_cols,
    ...){
  rlang::check_dots_empty(...)


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("\nadded 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <-
    x %>%
    dplyr::mutate(depth = (upper + lower) / 2)

  x <- new_cfp_soilphys(x,
                        id_cols
  )

  x <- validate_cfp_soilphys(x)
  x
}

#constructor
new_cfp_soilphys <- function(soilphys,
                             id_cols){
  x <- new_cfp_layered_profile(
    soilphys,
    id_cols = id_cols,
    class = "cfp_soilphys")
  x
}

#validator
validate_cfp_soilphys <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_soilphys"))
  validate_cfp_layered_profile(x)

  # are the necessary columns present?
  base_cols <- c("upper","lower","DS","c_air","gas")
  base_cols_present <- base_cols %in% names(x)

  if (any(!base_cols_present) ){
    stop(paste0("missing columns ", base_cols[!base_cols_present]))
  }


  # no negative values in DS and c_air
  any_negative_DS <- min(x$DS, na.rm = TRUE) < 0
  any_negative_c_air <- min(x$c_air, na.rm = TRUE) < 0
  stopifnot("Negative diffusion coefficient DS is not allowed!" = !any_negative_DS)
  stopifnot("Negative air density c_air is not allowed!" = !any_negative_c_air)

  x
}


#methods --------------

# PRINTING
#' @exportS3Method
print.cfp_soilphys <- function(x, ...){
  NextMethod()
}


# SUBSETTING
#' @export
'[.cfp_soilphys' <- function(x,
                             ...){
  id_cols <- cfp_id_cols(x)
  x_class <- class(x)

  x <- data.frame(x)
  x <- x[...]

  if (all(id_cols %in% names(x))){
    # if all id_cols present, coerce back to soilphys object
    attr(x, "id_cols") <- id_cols
    class(x) <- x_class
  }

  x
}

