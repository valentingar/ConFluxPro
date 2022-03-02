#'@title cfp_soilphys
#'
#' @description Create and validate cfp_soilphys objects. A cfp_soilphys object has the following
#' characteristics. Each profile is uniquely identifiable by its \code{id_cols} without duplicates.
#' The data is upper/lower consistent, meaning that the steps of each profile cover a range
#' from min(lower) to max(upper) without gaps, overlap or duplicates.
#'
#' @param soilphys A data.frame with (at least) the following columns:
#' \describe{
#' \item{upper (cm) }{The upper bound of each step.}
#' \item{lower (cm)}{The lower bound of each step.}
#' \item{gas}{The gas of that step.}
#' \item{DS (\eqn{m^2 s^-1})}{The specific diffusion coefficient of that gas in that step.}
#' \item{c_air (\eqn{mol m^-3})}{The number density of air in that step.}
#' \item{any of \code{id_cols}}{All id_cols that identify one profile uniquely.}
#'
#' }
#'
#'
#' @inheritParams cfp_layers_map
#'
#' @return cfp_soilphys
#'
#' @export
#helper
cfp_soilphys <- function(soilphys,
                         id_cols){


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("\nadded 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <- new_cfp_soilphys(soilphys,
                        id_cols
  )

  x <- validate_cfp_soilphys(x)
}

#constructor
new_cfp_soilphys <- function(soilphys,
                             id_cols){
  x <- structure(soilphys,
                 class = c("cfp_soilphys","data.frame"),
                 id_cols = id_cols)
  x
}

#validator
validate_cfp_soilphys <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_soilphys"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("upper","lower","DS","c_air","gas")
  id_cols <- cfp_id_cols(x)

  stopifnot("data.frame lacks obligatory coluns" = base_cols %in% names(x),
            "id_cols must be present in the data.frame" = id_cols %in% names(x)
  )

  # is the data frame upper/lower consistent?
  stopifnot("The data is not unique and upper/lower consistent!" = is_ul_consistent(x,id_cols))

  x
}


#methods --------------

# PRINTING
#' @exportS3Method
print.cfp_soilphys <- function(x){
  cat("\nA cfp_soilphys object \n")
  print_id_cols(x)
  cat("\n")
  NextMethod()
}


