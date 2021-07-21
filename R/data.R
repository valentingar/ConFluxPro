#' Soil CO2 concentrations
#'
#' A synthetic dataset of soil CO2 concentrations at
#' two sites over a one-year period.
#'
#' @format A tibble with 312 rows and 6 variables:
#' \describe{
#'   \item{ste}{name of the site}
#'   \item{Date}{Date in the format "YYYY-MM-DD"}
#'   \item{depth}{depth from mineral soil in cm}
#'   \item{repetition}{id for which repetition in each depth}
#'   \item{NRESULT_ppm}{concentration, in ppm}
#'   \item{gas}{name of the gas}
#' }
"gasdata"
