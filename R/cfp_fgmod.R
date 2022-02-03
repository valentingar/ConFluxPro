#' @title cfp_pfmod
#'
#' @description An S3 class for \code{fg_flux()} models. The class inherits from
#' cfp_dat and adds any model specific parameters.
#'
#' @param x A \code{cfp_dat} object with all the necessary input datasets.
#'
#' @inheritParams fg_flux
#'
#'

cfp_fgmod <- function(x,
                      gases,
                      modes,
                      param,
                      funs){

  stopifnot(inherits(x, "cfp_dat"))

  x <- structure(x,
                 class = c("cfp_fgmod", class(x)),
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
            "rho_air must be in param!" = "rho_air" %in% cfp_param(x))

  match.arg(cfp_modes(x), c("LL", "LS", "EF"))

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

#' @export
cfp_gases <- function(x){
  UseMethod("cfp_gases")
}
#' @exportS3Method
cfp_gases.cfp_fgmod <- function(x){
  attr(x, "gases")
}

#' @export
cfp_modes <- function(x){
  UseMethod("cfp_modes")
}
#' @exportS3Method
cfp_modes.cfp_fgmod <- function(x){
  attr(x, "modes")
}


#' @export
cfp_param <- function(x){
  UseMethod("cfp_param")
}
#' @exportS3Method
cfp_param.cfp_fgmod <- function(x){
  attr(x, "param")
}



#' @export
cfp_funs <- function(x){
  UseMethod("cfp_funs")
}
#' @exportS3Method
cfp_funs.cfp_fgmod <- function(x){
  attr(x, "funs")
}



