#' @title cfp_pfmod
#'
#' @description An S3 class for \code{pro_flux()} models. The class inherits from
#' cfp_dat and adds any model specific parameters.
#'
#' @param x A \code{cfp_dat} object with all the necessary input datasets.
#'
# @param storage_term (logical) Should changes in storage be accounted for?
#   Default is F. Only works if data is present in a temporal dimension as well
#   and is probably only representative for a high temporal resolution (hours).
#
#' @param zero_flux (logical) Applies the zero-flux boundary condition? If
#'   FALSE, the first value in X represents the incoming flux to the lowest
#'   layer.
#'
#' @param zero_limits (numeric vector) a vector of length 2 defining the lower
#'   and upper limit of the lowest flux if zero_flux = F.
#'
#' @param known_flux_factor RESERVED FOR FUTURE EXPANSION
# (numeric) a numeric value > 0 that represents a
#   weight for the error calculation with the known flux. A higher value means
#   that the optimisation will weigh the error to the efflux more than in
#   regard to the concentration measurements. Must be determined manually by
#   trying out!
#'
#' @param DSD0_optim RESERVED FOR FUTURE EXPANSION
# (logical) If TRUE, the diffusion coefficient (DSD0) values are
#   also object to optimisation together with the production. DSD0 is varied between
#   values 0 and 1, DS is then recalculated from D0 to be used in the model. The fit values
#   are given as DSD0_fit in the return table. Only makes sense to use in
#   combination with known_flux.
#'
#' @param evenness_factor (numeric) A user defined factor used to penalise strong
#' differences between the optimised production rates. This must be identified by
#' trial-and-error and can help prevent that production rates are simply set to zero
#' basically the lower a production is relative to the the maximum of the absolute of
#' all productions, the higher it is penalised. The \code{evenness_factor} then
#' defines the weight of this penalty in the optimisation algorithm \code{\link{prod_optim}}.
#'
#'

cfp_pfmod <- function(x,
                      zero_flux,
                      zero_limits,
                      DSD0_optim,
                      evenness_factor,
                      known_flux_factor){

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



### EXTRACTORS #####
#' @describeIn extractors zero_flux
#' @export
cfp_zero_flux <- function(x){
  UseMethod("cfp_zero_flux")
}
#' @exportS3Method
cfp_zero_flux.default <- function(x){
  out <- attr(x,"zero_flux")
  out
}


#' @describeIn extractors zero_limits
#' @export
cfp_zero_limits <- function(x){
  UseMethod("cfp_zero_limits")
}
#' @exportS3Method
cfp_zero_limits.default <- function(x){
  out <- attr(x,"zero_limits")
  out
}

#' @describeIn extractors DSD0_optim
#' @export
cfp_DSD0_optim <- function(x){
  UseMethod("cfp_DSD0_optim")
}
#' @exportS3Method
cfp_DSD0_optim.default <- function(x){
  out <- attr(x,"DSD0_optim")
  out
}


#' @describeIn extractors evenness_factor
#' @export
cfp_evenness_factor <- function(x){
  UseMethod("cfp_evenness_factor")
}
#' @exportS3Method
cfp_evenness_factor.default <- function(x){
  out <- attr(x,"evenness_factor")
  out
}


#' @describeIn extractors known_flux_factor
#' @export
cfp_known_flux_factor <- function(x){
  UseMethod("cfp_known_flux_factor")
}
#' @exportS3Method
cfp_known_flux_factor.default <- function(x){
  out <- attr(x,"known_flux_factor")
  out
}

###### COERCING #######
#' @describeIn coercion to cfp_pfmod
#' @export
as_cfp_pfmod <- function(x){
  UseMethod("as_cfp_pfmod")
}

#'@exportS3Method
as_cfp_pfmod.cfp_pfres <- function(x){
  x <- new_cfp_pfmod(new_cfp_dat(x$gasdata,
                                 x$soilphys,
                                 x$layers_map,
                                 x$profiles,
                                 cfp_id_cols(x)),
                     cfp_zero_flux(x),
                     cfp_zero_limits(x),
                     cfp_DSD0_optim(x),
                     cfp_evenness_factor(x),
                     cfp_known_flux_factor(x))
  x
}
