#'@title Soil physical parameters data
#'
#' @description
#' Create a [cfp_soilphys] object. This is a data.frame containing layered
#' data of soil physical properties, at the minimum of the air density
#' \code{c_air} and diffusion coefficient \code{DS} for one or multiple soil
#' profiles.
#' Each soil profile is uniquely identified by columns in the data.frame
#' specified by the \code{id_cols} attribute. Each profile is further subdivided
#' into layers by columns \code{upper} and \code{lower}
#' (see [cfp_layered_profile]).
#'
#' @param x A data.frame with (at least) the following columns:
#' \describe{
#' \item{upper (cm) }{The upper bound of each step.}
#' \item{lower (cm)}{The lower bound of each step.}
#' \item{gas}{The gas of that step.}
#' \item{DS (\eqn{m^2 s^-1})}{The specific diffusion coefficient of that gas in
#' that step.} \item{c_air (\eqn{mol m^-3})}{The number density of air in that
#' step.} \item{any of \code{id_cols}}{All id_cols that identify one profile
#' uniquely.}
#'
#' }
#' @inheritDotParams cfp_profile id_cols
#'
#' @family data formats
#'
#' @returns A \code{cfp_soilphys} object.
#'
#' @examples
#' cfp_soilphys(
#'   ConFluxPro::soilphys,
#'   id_cols = c("site", "Date", "gas")
#' )
#' ### Also used to extract an soilphys object from cfp_dat
#' cfp_soilphys(ConFluxPro::base_dat)

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
  rlang::check_dots_empty()

  x <- get_soilphys(x)
  x
}


#' @rdname cfp_soilphys
#' @inheritParams cfp_profile
#'@exportS3Method
cfp_soilphys.data.frame <- function(
    x,
    id_cols,
    ...){
  rlang::check_dots_empty()


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
  stopifnot("Negative diffusion coefficient DS is not allowed!" =
              !any_negative_DS)
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

