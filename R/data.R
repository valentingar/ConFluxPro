#' Example soil CO2 concentrations
#'
#' A synthetic dataset of soil CO2 concentrations at
#' two sites over a one-year period.
#'
#' @format A tibble with 312 rows and 6 variables:
#' \describe{
#'   \item{site}{name of the site}
#'   \item{Date}{Date in the format "YYYY-MM-DD"}
#'   \item{depth}{depth from mineral soil in cm}
#'   \item{repetition}{id for which repetition in each depth}
#'   \item{x_ppm}{concentration, in ppm}
#'   \item{gas}{name of the gas}
#' }
"gasdata"


#' Example soil diffusion models
#'
#' A synthetic dataset of soil total pore space and
#' diffusion models after the general formula a*AFPS^b.
#'
#' @format A tibble with 8 rows and 6 variables:
#' \describe{
#'   \item{site}{name of the site}
#'   \item{upper}{upper limit for layer in cm}
#'   \item{lower}{lower limit for layer in cm}
#'   \item{TPS}{total pore space as fraction of volume}
#'   \item{a}{diffusion-model fit parameter a}
#'   \item{b}{diffusion-model fit parameter b}
#' }
"soildiff"


#' Example soil water content
#'
#' A synthetic dataset of soil water content in a
#' layered structure. The dates correspond to \code{gasdata}.
#'
#' @format A tibble with 180 rows and 5 variables:
#' \describe{
#'   \item{site}{name of the site}
#'   \item{Date}{Date in the format "YYYY-MM-DD"}
#'   \item{upper}{upper limit for layer in cm}
#'   \item{lower}{lower limit for layer in cm}
#'   \item{SWC}{soil water content as fraction of volume}
#' }
"soilwater"


#' Example soil temperature
#'
#' A synthetic dataset of soil temperature at
#' discrete depths. The dates correspond to \code{gasdata}.
#'
#' @format A tibble with 120 rows and 4 variables:
#' \describe{
#'   \item{site}{name of the site}
#'   \item{Date}{Date in the format "YYYY-MM-DD"}
#'   \item{depth}{depth in cm}
#'   \item{t}{temperature in °C}
#' }
"soiltemp"

#' Example cfp_soilphys object
#'
#' An example dataset for \code{soilphys} based on the sets
#' \code{soiltemp}, \code{soilwater} and \code{soildiff}
#'
#' @format A tibble with 120 rows and 4 variables:
#' \describe{
#'   \item{site}{name of the site}
#'   \item{Date}{Date in the format "YYYY-MM-DD"}
#'   \item{depth}{depth in cm}
#'   \item{t}{temperature in °C}
#' }
"soilphys"

#' Example cfp_layers_map object
#'
#' An example dataset for \code{layers_map} that devides each \code{site} into
#' two layers.
#'
#' @format A data.frame with 4 rows and 3 variables:
#' \describe{
#'   \item{site}{name of the site}
#'   \item{upper}{upper limit for layer in cm}
#'   \item{lower}{lower limit for layer in cm}
#' }
"layers_map"


#' Example cfp_dat object
#'
#' An example [cfp_dat()] object that combines all other example data.
#'
#' @format A [cfp_dat()] object as a list with
#' \describe{
#'   \item{profiles}{The profiles of the data.}
#'   \item{gasdata}{The [gasdata] object.}
#'   \item{soilphys}{The [soilphys] object.}
#'   \item{layers_map}{The [layers_map] object.}
#' }
"base_dat"
