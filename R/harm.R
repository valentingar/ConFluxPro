#'@title Harmonic mean
#'
#'@description This function calculates harmonic mean of a vector and can be
#'  used analogous to the base functions mean() or median()
#'
#'@param x (numeric vector)
#'@param w (numeric vector) optional vector of weights corresponding to x.
#'  Default is 1 for all.
#'@param na.rm (logical) If TRUE, then NA values are omitted and the mean
#'  calculated with the remaining values. If FALSE (default) then returns NA if
#'  x contains NA values.
#'
#'@returns (numeric) harmonic mean of x
#'
#'@examples
#'  harm(c(1:10))
#'  harm(c(1:10),c(10:1))
#'
#'
#'
#'@export

harm <- function(x, w = 1, na.rm = FALSE){
  if (length(w) == 1){
    w <- rep(w, length(x))
  } else if (!length(w) == length(x)){
    stop(paste0("Length of w (",
                length(w),") must be the same length of x (",
                length(x),") or 1"))
  } else if (anyNA(w)){
    stop("Cannot compute weighted harmonic mean if w contains NAs")
  }
  if (na.rm & anyNA(x)){
    toremove <- which(is.na(x))
    x <- x[-toremove]
    w <- w[-toremove]
  }
  (sum(w*x^-1)/sum(w))^-1

}
