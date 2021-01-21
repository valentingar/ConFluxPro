#' @title season
#'
#' @description A simple function to return a character (-vector) of the season from a Date (-vector).
#' Months:
#' spring 3-5;
#' summer 6-8;
#' fall 9-11;
#' winter 12-2;
#'
#' @param d (Date)
#'
#' @return m (character)
#'
#' @examples d <- as.Date("2015-10-15")
#' s <- season(d)
#' s
#' [1] "fall"
#'
#' d <- as.Date(c("1955-01-15","1985-06-15","2015-10-15"))
#' s <- season(d)
#' s
#' [1] "winter" "summer" "fall"
#'
#' @import lubridate
#'
#' @export


season <- function(d){
  m<-unlist(lapply(d, function(d){
    m<-lubridate::month(d)
    if(m %in% c(3,4,5)){
      return("spring")
    } else if (m %in% c(6,7,8)){
      return("summer")
    } else if (m %in% c(9,10,11)){
      return("fall")
    } else if(m %in% c(12,1,2)){
      return("winter")
    } else {
      return(NA)
    }
  }))
  return(m)
}
