% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_profile.R
\name{plot_profile}
\alias{plot_profile}
\title{plot_profile}
\usage{
plot_profile(x)
}
\arguments{
\item{x}{A cfp_pfres, cfp_fgres model result, or a cfp_soilphys, cfp_gasdata
or cfp_layers_map object}
}
\value{
A ggplot2 plot with facets for each distinct profile. If more than 20
profiles are plotted a message is sent because this can take a long time.
}
\description{
Plot vertical soil-profiles of ConFluxPro objects using ggplot. This is mainly
intended for diagnostic purposes and better understand the underlying data.
Supported objects:
\describe{
\item{cfp_pfres}{Displays TPS, SWC and AFPS, as well as production and
measured and modelled gas concentrations.}
\item{cfp_fgres}{Displays TPS, SWC and AFPS, as well as the measured
concentration profile, and concentration gradients for each layer.}
\item{cfp_soilphys}{Displays TPS, SWC and AFPS, as well as values of
Ds and Temperature.}
\item{cfp_gasdata}{Displays the concentration profile.}
\item{cfp_layers_map}{Displays the layer names, pmap and layer_couple,
as well as the allowed production range.}
}
}
