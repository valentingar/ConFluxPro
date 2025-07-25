% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_map.R
\name{run_map}
\alias{run_map}
\title{run_map}
\usage{
run_map(
  x,
  params = list(),
  type = NULL,
  method = NULL,
  n_runs = NULL,
  layers_different = FALSE,
  layers_from = "layers_map",
  layers_altmap = NULL,
  topheight_adjust = FALSE
)
}
\arguments{
\item{x}{Either a \link{cfp_pfres} or \link{cfp_fgres} model result.}

\item{params}{A named list of numeric vectors. Names indicate column names in
soilphys, vectors either distinct values (method permutation) or limits
(method random).}

\item{type}{A vector of length param indicating what the values in params
represent. One of
\describe{
\item{abs}{Absolute values that are applied as-is.}
\item{factor}{Factors to be multiplied with the original values.}
\item{addition}{Factors to be added to the original values.}
}}

\item{method}{Either 'random', where a random value is chosen within the
bounds set in \code{params} or 'permutation', where every permutation of the
values in \code{params} is added.}

\item{n_runs}{Integer value of the number of alterations to be done for method
= 'random'.}

\item{layers_different}{Should layers from layers_map be changed individually?
If \code{TRUE} this allows for different changes at different depths.}

\item{layers_from}{(character) If layers_different is TRUE, from which source
should the layers be created? One of:
\describe{
\item{layers_map}{(default) Use the layers that are defined in layers_map.}
\item{soilphys}{Use the layers as defined in soilphys}
\item{layers_altmap}{Use the layers as defined in the provided layers_altmap
object.}
}}

\item{layers_altmap}{An optional layers_map created using layers_map() that
defines the layers to be used if layers_different = TRUE.}

\item{topheight_adjust}{(logical) If the proposed change in topheight is
larger than the highest layer in soilphys, should the limits be
automatically adjusted per id_cols individually? Default is FALSE, which
leads to an error in that case.}
}
\value{
An object of type \code{cfp_run_map} that can be used within
\link{alternate}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

\code{run_map()} was deprecated in favor of \link{cfp_run_map} for consistency.

Create a cfp_run_map for model alteration in alternate()
}
\examples{
PROFLUX <- ConFluxPro::base_dat |> pro_flux()
# Create a cfp_run_map where TPS is changed between 90 \% and 110 \%
# of the original value for 50 runs.
cfp_run_map(
  PROFLUX,
  list("TPS" = c(0.9, 1.1)),
  "factor",
  n_runs = 50)

}
