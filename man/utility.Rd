% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfp_dat.R, R/n_profiles.R
\name{n_groups}
\alias{n_groups}
\alias{n_profiles}
\title{Get number of groups/profiles}
\usage{
n_groups(x)

n_profiles(x)
}
\arguments{
\item{x}{A \code{cfp_dat} object.}
}
\value{
An integer giving the number of groups of the object.

An integer giving the number of profiles of the object.
}
\description{
Get number of groups/profiles
}
\examples{
n_groups(base_dat)

n_profiles(base_dat)
n_profiles(cfp_soilphys(base_dat))
}
