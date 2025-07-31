#' @title Model frame for pro_flux
#'
#' @description An S3 class for \code{pro_flux()} models. The class inherits
#'   from cfp_dat and adds any model specific parameters.
#'
#' @param x A \code{cfp_dat} object with all the necessary input datasets.
#'
# @param storage_term (logical) Should changes in storage be accounted for?
#   Default is F. Only works if data is present in a temporal dimension as well
#   and is probably only representative for a high temporal resolution (hours).
#
#' @param zero_flux (logical) Applies the zero-flux boundary condition? If
#'   `FALSE`, `F0` is optimized alongside the production rates.
#'
#' @param zero_limits (numeric vector) a vector of length 2 defining the lower
#'   and upper limit of the lowest flux if `zero_flux = FALSE`.
#'
#' @param known_flux_factor `r lifecycle::badge("deprecated")`
# (numeric) a numeric value > 0 that represents a
#   weight for the error calculation with the known flux. A higher value means
#   that the optimisation will weigh the error to the efflux more than in
#   regard to the concentration measurements. Must be determined manually by
#   trying out!
#'
#' @param DSD0_optim `r lifecycle::badge("deprecated")`
# (logical) If TRUE, the diffusion coefficient (DSD0) values are also object to
# optimisation together with the production. DSD0 is varied between values 0 and
# 1, DS is then recalculated from D0 to be used in the model. The fit values are
# given as DSD0_fit in the return table. Only makes sense to use in combination
# with known_flux.
#'
#' @param evenness_factor `r lifecycle::badge("experimental")`(numeric) A user
#'   defined factor used to penalise strong differences between the optimised
#'   production rates. This must be identified by trial-and-error and can help
#'   prevent that production rates are simply set to zero basically the lower a
#'   production is relative to the the maximum of the absolute of all
#'   productions, the higher it is penalised. The \code{evenness_factor} then
#'   defines the weight of this penalty in the optimisation algorithm
#'   \code{\link{prod_optim}}.
#'
#' @family model frames
#'
#' @returns A \code{cfp_pfmod} object that inherits from [cfp_dat()]
#'
#' @examples
#' cfp_pfmod(ConFluxPro::base_dat)
#'
#' ### coercion from other object types (internal)
#' pro_flux(ConFluxPro::base_dat) |>
#'   as_cfp_pfmod()
#'
#' @export

cfp_pfmod <- function(x,
                      zero_flux = TRUE,
                      zero_limits = c(-Inf, Inf),
                      DSD0_optim = deprecated(),
                      evenness_factor = 0,
                      known_flux_factor = deprecated(),
                      fit_to = "concentration"){

  fit_to <- match.arg(fit_to, c("concentration", "molar_fraction"))

  if(lifecycle::is_present(DSD0_optim)){
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "cfp_pfmod(DSD0_optim)",
    details = "This deprecated feature was removed entirely."
  )}
  if(lifecycle::is_present(known_flux_factor)){
  lifecycle::deprecate_warn(
    when = "1.0.0",
    what = "cfp_pfmod(known_flux_factor)",
    details = "This deprecated feature was removed entirely."
  )}

  x <- new_cfp_pfmod(x,
                     zero_flux,
                     zero_limits,
                     evenness_factor,
                     fit_to
  )
  x <- validate_cfp_pfmod(x)
  x
}


new_cfp_pfmod <- function(x,
                          zero_flux,
                          zero_limits,
                          evenness_factor,
                          fit_to
){
  stopifnot(inherits(x,"cfp_dat"))

  #make sure soilphys is in ascending order
  x$soilphys <- dplyr::arrange(x$soilphys,upper)


  x <- structure(x,
                 class = c("cfp_pfmod", "cfp_dat", "list"),
                 zero_flux = zero_flux,
                 zero_limits = zero_limits,
                 DSD0_optim = NULL,
                 evenness_factor = evenness_factor,
                 known_flux_factor = NULL,
                 fit_to = fit_to)
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
  #cat("DSD0_optim: ", cfp_DSD0_optim(x), " \n")
  cat("evenness_factor: ", cfp_evenness_factor(x)," \n")
  #cat("known_flux_factor: ", cfp_known_flux_factor(x)," \n")
  cat("fit to:", cfp_fit_to(x), "\n")
  NextMethod()
}



### EXTRACTORS #####
#' @rdname extractors
#' @export
cfp_zero_flux <- function(x){
  UseMethod("cfp_zero_flux")
}
#' @exportS3Method
cfp_zero_flux.default <- function(x){
  out <- attr(x,"zero_flux")
  out
}


#' @rdname extractors
#' @export
cfp_zero_limits <- function(x){
  UseMethod("cfp_zero_limits")
}
#' @exportS3Method
cfp_zero_limits.default <- function(x){
  out <- attr(x,"zero_limits")
  out
}

#' @rdname extractors
#' @export
cfp_DSD0_optim <- function(x){
  UseMethod("cfp_DSD0_optim")
}
#' @exportS3Method
cfp_DSD0_optim.default <- function(x){
  out <- attr(x,"DSD0_optim")
  out
}


#' @rdname extractors
#' @export
cfp_evenness_factor <- function(x){
  UseMethod("cfp_evenness_factor")
}
#' @exportS3Method
cfp_evenness_factor.default <- function(x){
  out <- attr(x,"evenness_factor")
  out
}


#' @rdname extractors
#' @export
cfp_known_flux_factor <- function(x){
  UseMethod("cfp_known_flux_factor")
}
#' @exportS3Method
cfp_known_flux_factor.default <- function(x){
  out <- attr(x,"known_flux_factor")
  out
}


#' @rdname extractors
#' @export
cfp_fit_to <- function(x){
  UseMethod("cfp_fit_to")
}
#' @exportS3Method
cfp_fit_to.default <- function(x){
  out <- attr(x,"fit_to")
  out
}

###### COERCING #######
#' @describeIn coercion to cfp_pfmod
#' @keywords internal
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
