#' @title Get season of a date-time
#'
#' @description A simple function to return a character (-vector)
#' of the season from a Date (-vector).
#' Months:
#' \describe{
#' \item{spring}{3-5}
#' \item{summer}{6-8}
#' \item{fall}{9-11}
#' \item{winter}{12-2}
#' }
#'
#' @param d (Date) Any date object
#'
#' @returns A character vector the same length as d
#'
#' @examples season(as.Date(c("1955-01-15","1985-06-15","2015-10-15")))
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
