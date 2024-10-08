% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfp_pfmod.R
\name{cfp_pfmod}
\alias{cfp_pfmod}
\title{cfp_pfmod}
\usage{
cfp_pfmod(
  x,
  zero_flux = TRUE,
  zero_limits = c(-Inf, Inf),
  DSD0_optim = FALSE,
  evenness_factor = 0,
  known_flux_factor = 0
)
}
\arguments{
\item{x}{A \code{cfp_dat} object with all the necessary input datasets.}

\item{zero_flux}{(logical) Applies the zero-flux boundary condition? If
FALSE, the first value in X represents the incoming flux to the lowest
layer.}

\item{zero_limits}{(numeric vector) a vector of length 2 defining the lower
and upper limit of the lowest flux if zero_flux = F.}

\item{DSD0_optim}{RESERVED FOR FUTURE EXPANSION}

\item{evenness_factor}{(numeric) A user defined factor used to penalise strong
differences between the optimised production rates. This must be identified by
trial-and-error and can help prevent that production rates are simply set to zero
basically the lower a production is relative to the the maximum of the absolute of
all productions, the higher it is penalised. The \code{evenness_factor} then
defines the weight of this penalty in the optimisation algorithm \code{\link{prod_optim}}.}

\item{known_flux_factor}{RESERVED FOR FUTURE EXPANSION}
}
\value{
A \code{cfp_pfmod} object that inherits from \code{\link[=cfp_dat]{cfp_dat()}}
}
\description{
An S3 class for \code{pro_flux()} models. The class inherits from
cfp_dat and adds any model specific parameters.
}
\examples{
cfp_pfmod(ConFluxPro::base_dat)
}
\keyword{internal}
