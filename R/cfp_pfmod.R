#' @title cfp_pfmod
#'
#' @description An S3 class for \code{pro_flux()} models. The class inherits from
#' cfp_input and adds any model specific parameters.
#'
#' @param x A \code{cfp_dat} object with all the necessary input datasets.
#'
#' @inheritParams pro_flux
#'
#'

cfp_pfmod <- function(x,
                      zero_flux = TRUE,
                      zero_limits = c(-Inf,Inf),
                      DSD0_optim = FALSE,
                      evenness_factor = 0,
                      known_flux_factor = 0){

  x <- new_cfp_pfmod(x,
                      zero_flux,
                      zero_limits,
                      DSD0_optim,
                      evenness_factor,
                      known_flux_factor
  )
  x <- validate_cfp_pfmod(x)
  x
}


new_cfp_pfmod <- function(x,
                          zero_flux,
                          zero_limits,
                          DSD0_optim,
                          evenness_factor,
                          known_flux_factor
){
  stopifnot(inherits(x,"cfp_dat"))

  x <- structure(x,
                 class = c("cfp_pfmod","cfp_dat"),
                 zero_flux = zero_flux,
                 zero_limits = zero_limits,
                 DSD0_optim = DSD0_optim,
                 evenness_factor = evenness_factor,
                 known_flux_factor = known_flux_factor)
  x
}


validate_cfp_pfmod <- function(x){

  lmap <- x$layers_map

  stopifnot("layers_map cannot contain NAs!" = anyNA(lmap) == FALSE)
  x
}


# methods ---------


###### PRINTING #######
#' @exportS3Method
print.cfp_pfmod <- function(x, ...){
  cat("\nA cfp_pfmod pro_flux model. \n")
  cat("zero_flux:", cfp_zero_flux(x)," \n")
  cat("zero_limits: ", cfp_zero_limits(x), " \n")
  cat("DSD0_optim: ", cfp_DSD0_optim(x), " \n")
  cat("evenness_factor: ", cfp_evenness_factor(x)," \n")
  cat("known_flux_factor: ", cfp_known_flux_factor(x)," \n")
  NextMethod()
}



#extractors
#' @export
cfp_zero_flux <- function(x){
  UseMethod("cfp_zero_flux")
}
#' @exportS3Method
cfp_zero_flux.default <- function(x){
  out <- attr(x,"zero_flux")
  out
}

#' @export
cfp_zero_limits <- function(x){
  UseMethod("cfp_zero_limits")
}
#' @exportS3Method
cfp_zero_limits.default <- function(x){
  out <- attr(x,"zero_limits")
  out
}

#' @export
cfp_DSD0_optim <- function(x){
  UseMethod("cfp_DSD0_optim")
}
#' @exportS3Method
cfp_DSD0_optim.default <- function(x){
  out <- attr(x,"DSD0_optim")
  out
}


#' @export
cfp_evenness_factor <- function(x){
  UseMethod("cfp_evenness_factor")
}
#' @exportS3Method
cfp_evenness_factor.default <- function(x){
  out <- attr(x,"evenness_factor")
  out
}

#' @export
cfp_known_flux_factor <- function(x){
  UseMethod("cfp_known_flux_factor")
}
#' @exportS3Method
cfp_known_flux_factor.default <- function(x){
  out <- attr(x,"known_flux_factor")
  out
}
