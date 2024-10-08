#' @title Calculate DSD0
#' @name DSD0
#' @description
#' Different functions to estimate soil diffusivity
#' from the air-filled pore space.
#'
#' @param AFPS The air-filled porosity.
#'
#' @examples
#' DSD0_millington_quirk(0.5,0.6)
#'
#' @details
#' \itemize{
#' \item{
#' [DSD0_millington_quirk()] is of the form \eqn{D_s / D_0 = \Xi \cdot \epsilon}
#' where \eqn{\Xi} is the tortuosity factor (\code{tortuosity}) calulcated as
#' \eqn{\Xi = \frac{\epsilon^{(10/3)}}{\Phi^2}}
#' }; \eqn{\epsilon} is the air-filled pore space (\code{AFPS}) and \eqn{\Phi} is
#' the porosity (\code{TPS}). From Millington & Quirk (1961).
#' \item{
#' [DSD0_moldrup()] is of the form \eqn{D_s / D_0 = (2 \cdot \epsilon_{100}^3 + 0.04 \cdot \epsilon_{100}) \cdot (\frac{\epsilon}{\epsilon_{100}})^{(2 + \frac{3}{b_{campbell}})}}
#' where \eqn{\epsilon_{100}} is the air-filled pore space at a matric potential head of -100 cm and
#' \eqn{b_{campbell}} is the slope of the water retention curve. From Moldrup et al. (2000).
#' }
#' \item{
#' [DSD0_currie()] is of the form \eqn{D_s / D_0 = a \cdot \epsilon^b} where
#' \eqn{a} and \eqn{b} are fit parameter of an exponential model. From Currie (1960)
#' with default values (a=1.9; b=1.4)from Troeh (1982).
#' }
#' \item{
#' [DSD0_linear()] is a linear model of form \eqn{D_s / D_0 = a \cdot \epsilon + b}.
#' }
#' }
#'
#'
#'
#'
#' @references
#' Millington, R. J., & Quirk, J. P. (1961). Permeability of porous solids. In Transactions of the Faraday Society (Vol. 57, p. 1200). Royal Society of Chemistry (RSC). https://doi.org/10.1039/tf9615701200
#'
#' Moldrup, P., Olesen, T., Schjønning, P., Yamaguchi, T., & Rolston, D. E. (2000). Predicting the Gas Diffusion Coefficient in Undisturbed Soil from Soil Water Characteristics. In Soil Science Society of America Journal (Vol. 64, Issue 1, pp. 94–100). Wiley. https://doi.org/10.2136/sssaj2000.64194x
#'
#' Currie, J. A. (1960). Gaseous diffusion in porous media. Part 2. - Dry granular materials. In British Journal of Applied Physics (Vol. 11, Issue 8, pp. 318–324). IOP Publishing. https://doi.org/10.1088/0508-3443/11/8/303
#'
#' Troeh, F. R., Jabro, J. D., & Kirkham, D. (1982). Gaseous diffusion equations for porous materials. In Geoderma (Vol. 27, Issue 3, pp. 239–253). Elsevier BV. https://doi.org/10.1016/0016-7061(82)90033-7

#' @rdname DSD0
#' @param TPS Total pore space
#' @param tortuosity the tortuosity of the soil
#'
#' @export

DSD0_millington_quirk <-
  function(AFPS,
           TPS = NULL,
           tortuosity = NULL){

    if (is.null(tortuosity)){
      tortuosity <- (AFPS)^(10/3) / (TPS)^2
    }
    DSD0 <- tortuosity*AFPS
    DSD0
  }


#' @rdname DSD0
#' @param AFPS_100 Air filled porosity at -100cm soil water matric head.
#' @param b_campbell Campbell (1974) PSD index
#' @export

DSD0_moldrup <- function(AFPS,
                         AFPS_100,
                         b_campbell){
  DSD0 <- (2*AFPS_100^3 + 0.04*AFPS_100) * (AFPS/AFPS_100)^(2+3/b_campbell)
  DSD0
}

#' @rdname DSD0
#' @param a_currie,b_currie fit parameter of Currie-style models
#' @export

DSD0_currie <- function(AFPS,
                        a_currie = 1.9,
                        b_currie = 1.4){
  DSD0 <- a_currie*AFPS^b_currie
  DSD0
}

#' @rdname DSD0
#' @param a_lin,b_lin linear model coefficients
#' @export
 DSD0_linear <- function(AFPS,
                         a_lin,
                         b_lin){
   DSD0 <- a_lin*AFPS + b_lin
   DSD0
 }
