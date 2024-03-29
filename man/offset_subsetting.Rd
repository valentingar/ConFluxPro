% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/offset_subsetting.R
\name{offset_subsetting}
\alias{offset_subsetting}
\title{offset_subsetting}
\usage{
offset_subsetting(df, start, end, mode)
}
\arguments{
\item{df}{(dataframe) The gasdata-dataframe.}

\item{start}{(vector, Date) A vector of type Date that specifies the starting
date of each section (the starting date is included in the section!).}

\item{end}{(vector, Date) A vector of type Date that specifies the end date
of each section (the end date is included in the section!). The start date
of element i should be the end date of (i-1) +1 day.}

\item{mode}{(vector, character) A vector of type character that specifies the
correction method to be used in each section. Valid methods are "const",
here the median value is being used as a constant factor, and "lin", where
a linear model is fit against time, to account for an temporal drift.}
}
\value{
df (dataframe) The corr_map dataframe used in the offset_correction() function
}
\description{
This function helps to create the corr_map dataframe for the offset correction.
Based on the Date input, it creates a new section variable and maps it to all SAMPLE_NO.
}
\examples{
{
data("gasdata")

library(dplyr)

gasdata <- gasdata \%>\%
  mutate(depth_cat = ifelse(depth>0,"HU","MIN"),
         SAMPLE_NO = row_number())

cmap <-
  offset_subsetting(gasdata,
                    start = "2021-01-01",
                    end = "2022-01-01",
                    mode = "const")

offset_correction(gasdata,
                  corr_map = cmap,
                  gases = "CO2",
                  gases_std = 400e-6,
                  depth_cal = "HU")

}
}
\seealso{
offset_correction

offset_override

Other gasdata: 
\code{\link{balance_correction}()},
\code{\link{offset_correction}()}
}
\concept{gasdata}
