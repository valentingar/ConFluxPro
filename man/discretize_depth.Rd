% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discretize_depth.R
\name{discretize_depth}
\alias{discretize_depth}
\title{discretize_depth}
\usage{
discretize_depth(
  df,
  param,
  method,
  depth_target,
  id_cols = NULL,
  boundary_nearest = F,
  boundary_average = "none",
  int_depth = 0.5,
  knots = NULL
)
}
\arguments{
\item{df}{(dataframe) The dataframe containing the parameters to be
interpolated, as well as the columns "depth", "upper" and "lower".}

\item{param}{(character vector) The column names name of the parameters to be
interpolated.}

\item{method}{(character vector) a character (-vector) specifying the methods
to be used for interpolation. Must be in the same order as param. One of
\describe{
\item{linear}{linear interpolation.}
\item{boundary}{mapping values to any
depth within the boundary. Suited for discrete variables.}
\item{linspline}{fits a linear spline. similar to \code{linear} interpolation
but with knots defined in \code{knots}}.
\item{nearest}{finds the nearest value and sets that. int_depth applies.}
\item{harmonic}{similar to \code{linear} but instead, harmonic means are calculated,
using the distance of the counterpart as weight. I.e.: new depth = 2.5, old depths = c(0,10)
tehn weights are 7.5 (for depth == 0) and 2.5 (for depth == 10)}
}}

\item{depth_target}{(numeric vector or data frame) specifying the format of
the target depths to be interpolated. Must include n+1 depths for n target
depth steps. If it is different per id_cols, enter a data.frame in long
form instead. This data frame must have  a "depth" column, as well as the
columns that identify the different cases. These id-columns must be the
same as or a subset of id_cols.}

\item{id_cols}{(character vector) = The names of the columns to be grouped
by, i.e. uniquely identifying one profile (e.g. c('Plot','Date')).}

\item{boundary_nearest}{(logical) = TRUE/FALSE if it is TRUE then for target
depth steps (partially) outside of the parameter boundaries, the neirest
neighbor is returned, else returns NA. Default is FALSE.}

\item{boundary_average}{("character) Defines what happens iff the
new layer contains multiple old layers. one of
\describe{
\item{none}{= the deafult \cr the new layer is set to NA}
\item{arith}{the new layer is calculated as the arithmetic mean of the old}
\item{harm}{the new layer is calculated as the harmonic mean of the old}
}}

\item{int_depth}{(numeric vector)  = value between 0 and 1 for 1 =
interpolation takes the top of each depth step, 0.5 = middle and 0= bottom.
Default = 0.5}

\item{knots}{(numeric vector) = the depths at which knots for the
linspline-method are to be placed. If this differs for the parameters, a
list of numeric vectors with the same lenght as "param" can be provided.
Cannot differ between id_cols.}
}
\value{
dataframe with the columns upper and lower derived from depth_target,
  depth being the middle of each depth step, as well as the interpolated and
  discretized parameters.
}
\description{
This function helps to interpolate and discretize
  depth-dependent data to match a set depth profile. The idea is that the
  data is discretized into set depth steps (see depth_target), each
  specifying a layer with an upper and lower boundary. So that for example
  one depth step is from top of the humus layer to +5 cm, the next from 5 to
  0 cm and so on. The format of this final dataframe is specified in
  "depth_target", where a numeric vector of the interfaces of the steps is
  given (e.g. c(6,3,0,-5,-10) resulting in 4 depth steps). \cr
  There are
  different interpolation methods implemented, which might be more practical
  for different parameters or tasks.
  \itemize{
  \item
  A linear interpolation is possible
  for more or less contiuous parameters, (e.g. soil temperature).
  \item The
  boundary interpolation is a nearest neighbor interpolation and uses the
  values set in the "upper" and "lower" variables to map the parameter to any
  step within these limits.
  \item
  A linear spline interpolation fits a linear
  spline model to the data with knots defined in  \code{knots}. It is
  possible to provide multiple parameters to be discretized. In this case it
  is also possible to define specific controls for each parameter
  individually. However, if only one value is given for method, int_depth,
  or knots, the corresponding value is applied to all parameters given in
  "param".}
}
\examples{
{

data("soiltemp")
library(dplyr)

dt <- soiltemp \%>\%
  select(site,depth) \%>\%
  distinct() \%>\%
  group_by(site) \%>\%
  slice_max(depth) \%>\%
  summarise(depth = c(depth,seq(0,-100,-10)))

discretize_depth(soiltemp,
                 "t",
                 "linear",
                 dt,c(
                   "site","Date"))
}

}
\seealso{
Other soilphys: 
\code{\link{check_soilphys}()},
\code{\link{complete_soilphys}()},
\code{\link{soilphys_layered}()}
}
\concept{soilphys}
