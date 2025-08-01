#' @title Model frame for fg_flux
#'
#' @description An S3 class for \code{fg_flux()} models. The class inherits from
#' cfp_dat and adds any model specific parameters.
#'
#' @param x A \code{cfp_dat} object with all the necessary input datasets.
#'
#' @param gases (character) A character vector defining the gases for which
#' fluxes shall be calculated.
#' @param modes (character) A character vector specifying mode(s) for dcdz
#'   calculation. Can be \code{"LL"},\code{"LS"},\code{"EF"}.
#'   \describe{
#'   \item{LL}{local linear approach: within each layer a linear model is
#'   evaluated of concentration over the depth.}
#'   \item{LS}{linear spline approach: A linear spline is fitted over the
#'   complete profile with nodes at the layer intersections.}
#'   \item{EF}{exponential fit approach: An exponential function of form
#'   y=a+b*x^c is fit of concentration against depth. Using the first derivative
#'   of that function the concentration gradient is evaluated for each layer.}
#'   \item{DA}{exponential fit approach: An exponential function of form
#'   `y=a+b*(1-exp(-a*x))` is fit of concentration against depth. Using the
#'   first derivative of that function the concentration gradient is evaluated
#'   for each layer. From Davidson (2006).}
#'   }
#' @param param (character) A vector containing the the parameters of soilphys,
#'   for which means should be calculated, must contain "c_air" and "DS", more
#'   parameters may help interpretation.
#' @param funs (character) A vector defining the type of mean to be used for
#' each parameter in \code{param}. One of "arith" or "harm".
#'
#' @family model frames
#'
#' @returns A \code{cfp_fgmod} object. This inherits from [cfp_dat()] and
#' adds model specific parameters.
#'
#' @references
#' DAVIDSON, E. A., SAVAGE, K. E., TRUMBORE, S. E., & BORKEN, W. (2006).
#' Vertical partitioning of CO2 production within a temperate forest soil.
#' In Global Change Biology (Vol. 12, Issue 6, pp. 944–956). Wiley.
#' https://doi.org/10.1111/j.1365-2486.2005.01142.x
#'
#' @examples
#' cfp_fgmod(ConFluxPro::base_dat)
#'
#' ### coercion from other object types (internal)
#' fg_flux(ConFluxPro::base_dat) |>
#'   as_cfp_fgmod()
#'

#' @export


cfp_fgmod <- function(x,
                      gases = unique_gases(x),
                      modes = "LL",
                      param = c("c_air", "DS"),
                      funs = c("arith", "harm")){

  stopifnot(inherits(x, "cfp_dat"))

  stopifnot("Selected gases not present in dataset" =
              all(gases %in% unique_gases(x)))


  if (length(gases) > 1){
    if ( length(modes) == 1){
      modes <- rep(modes, length(gases))
    } else if (is.null(match.call()$gases)){
      stop("Please manually assign each gas a mode when using multiple modes.")
    }
  }

  if(is.null(gases)){
    if(length(modes) != 1){
      stop("Only provide one modes or use gases argument.")
    }
    gases <- unique(x$profiles$gas, na.rm = TRUE)
  }

  x <- structure(x,
                 class = c("cfp_fgmod", "cfp_dat", "list"),
                 gases = gases,
                 modes = modes,
                 param = param,
                 funs = funs)
  x <- validate_fgmod(x)
  x
}

validate_fgmod <- function(x){

  validate_cfp_dat(x)

  stopifnot("length of modes must match gases!" =
              length(cfp_gases(x)) == length(cfp_modes(x)),
            "length of funs must match param!" =
              length(cfp_param(x)) == length(cfp_funs(x)))

  stopifnot("DS mus be in param!" = "DS" %in% cfp_param(x),
            "c_air must be in param!" = "c_air" %in% cfp_param(x))

  stopifnot("not a valid mode!" =
              all(cfp_modes(x) %in% c("LL", "LS", "EF", "DA")))

  x
}


###### methods ---------

###### PRINTING #######
#' @exportS3Method
print.cfp_fgmod <- function(x, ...){
  cat("\nA cfp_fgmod fg_flux model. \n")
  cat("gases:", cfp_gases(x)," \n")
  cat("modes: ", cfp_modes(x), " \n")
  cat("param: ", cfp_param(x), " \n")
  cat("funs: ", cfp_funs(x)," \n")
  NextMethod()
}


######## EXTRACTORS #########


#' @rdname extractors
#' @export
cfp_gases <- function(x){
  UseMethod("cfp_gases")
}
#' @exportS3Method
cfp_gases.cfp_fgmod <- function(x){
  attr(x, "gases")
}


#' @rdname extractors
#' @export
cfp_modes <- function(x){
  UseMethod("cfp_modes")
}
#' @exportS3Method
cfp_modes.cfp_fgmod <- function(x){
  attr(x, "modes")
}


#' @rdname extractors
#' @export
cfp_param <- function(x){
  UseMethod("cfp_param")
}
#' @exportS3Method
cfp_param.cfp_fgmod <- function(x){
  attr(x, "param")
}


#' @rdname extractors
#' @export
cfp_funs <- function(x){
  UseMethod("cfp_funs")
}
#' @exportS3Method
cfp_funs.cfp_fgmod <- function(x){
  attr(x, "funs")
}




###### COERCION #######
#' @describeIn coercion to cfp_fgmod
#' @keywords internal
#' @export
as_cfp_fgmod <- function(x){
  UseMethod("as_cfp_fgmod")
}

#'@exportS3Method
as_cfp_fgmod.cfp_fgres <- function(x){
  x <- cfp_fgmod(x = new_cfp_dat(x$gasdata,
                                 x$soilphys,
                                 x$layers_map,
                                 x$profiles,
                                 cfp_id_cols(x)),
                  gases = cfp_gases(x),
                  modes = cfp_modes(x),
                  param = cfp_param(x),
                  funs = cfp_funs(x))
  x
}
