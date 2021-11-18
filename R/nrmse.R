#' @title (N)RMSE
#'
#' @title Calculate the (normalized) root-mean-square-error of
#' two vectors.
#'
#' @param a,b numeric vectors of same length to be compared
#'
#' @param normer a character string defining the type of
#' normalization to be applied. Can be one of
#' \describe{
#' \item{mean}{the arithmetic mean of a}
#' \item{sd}{the standard deviation of a}
#' \item{range}{the difference between the range of a}
#' \item{IQR}{the difference between the interquantile range of a}
#' }
#'
#'
#'
#' @export
  rmse <- function(a,
                   b
  ){
    RMSE <- sqrt(mean((a-b)^2,na.rm = TRUE))
  }


#' @rdname rmse
  nrmse <- function(a,
                    b,
                    normer){
    RMSE <-rmse(a,b)
    if(normer == "mean"){
      NRMSE <- RMSE/abs(mean(a,na.rm = TRUE))
    } else if (normer == "range"){
      NRMSE <- RMSE / diff(range(a,na.rm = TRUE))
    } else if (normer == "IQR"){
      NRMSE <- RMSE / diff(IQR(a),na.rm = TRUE)
    }  else if (normer == "sd"){
      NRMSE <- RMSE / sd(a,na.rm = TRUE)
    } else {
      stop("normer must be one of mean,sd , range, IQR")
    }
  }
