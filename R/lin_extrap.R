#' @title lin_extrap
#'
#' @description This function linearly extrapolates based on two value-pairs (x1|y1),(X2|y2) to a third point of x (x_new).
#' @param x (numeric) Two values of X
#' @param y (numeric) Two values of y (same order as x)
#' @param x_new (numeric) x-values to be extrapolated to.
#'
#' @return y_new
#'
#' @examples
#' lin_extrap(c(3,4),c(10,20),5)
#' @export


lin_extrap<-function(x,y,x_new){
  if(!length(x)==2 | !length(y)==2){
    stop("length of x and y must be 2!")
  }
  y_new <- (x_new-x[1])*(diff(y)/diff(x))+y[1]
  return(y_new)
}
