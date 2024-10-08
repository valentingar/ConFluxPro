% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfp_fgres.R
\name{cfp_fgres}
\alias{cfp_fgres}
\title{cfp_fgres}
\usage{
cfp_fgres(x, y)
}
\arguments{
\item{x}{A valid \code{cfp_fgmod} object}

\item{y}{The corresponding FLUX \code{data.frame}.}
}
\value{
A \code{cfp_fgres} object. This inherits from \code{\link[=cfp_fgmod]{cfp_fgmod()}}.
}
\description{
A function to create an object of class cfp_fgres. This is the
central result class generated by running \code{fg_flux()}. Intended for
internal use only.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
FLUX <- fg_flux(ConFluxPro::base_dat)
cfp_fgres(
  cfp_fgmod(ConFluxPro::base_dat),
  FLUX$FLUX
)
\dontshow{\}) # examplesIf}
}
\keyword{internal}
