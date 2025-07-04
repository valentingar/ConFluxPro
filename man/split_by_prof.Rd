% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cfp_dat.R, R/split_by_prof.R
\name{split_by_group}
\alias{split_by_group}
\alias{split_by_prof}
\alias{split_by_prof.cfp_dat}
\alias{split_by_prof.cfp_profile}
\title{Split by profile}
\usage{
split_by_group(x)

split_by_prof(x)

\method{split_by_prof}{cfp_dat}(x)

\method{split_by_prof}{cfp_profile}(x)
}
\arguments{
\item{x}{A object that is grouped into profiles by its \code{id_cols}.}
}
\value{
A list  where each entry is one profile/group of the same
class as \code{x}.
}
\description{
Split profile data into a list for each single profile.
}
\examples{
split_by_group(base_dat)

df <- cfp_profile(
  data.frame(
      site = rep(c("site_a", "site_b"),
                 each = 2),
      variable = 1:4),
   id_cols = "site")
split_by_prof(df)

base_dat <- ConFluxPro::base_dat
split_by_prof(base_dat)
}
\keyword{internal}
