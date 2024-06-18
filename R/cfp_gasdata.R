#' @title cfp_gasdata
#' @description
#' Create a [cfp_gasdata] object. This is a data.frame containing gas
#' concentration data for one or multiple soil profiles.
#' Each soil profile is uniquely identified by columns in the data.frame
#' specified by the \code{id_cols} attribute.
#'
#' @param x A \code{data.frame} with the following columns:
#' \describe{
#' \item{gas}{The gas of that observation.}
#' \item{depth (cm)}{The depth of the observation.}
#' \item{x_ppm (ppm)}{The concentration in ppm.}
#' \item{any of \code{id_cols}}{All id_cols that identify one profile uniquely.}
#'}
#' @param ... not used
#'
#' @returns A \code{cfp_gasdata} object.
#'
#' @examples
#' cfp_gasdata(
#'   ConFluxPro::gasdata,
#'   id_cols = c("site", "Date"))
#' ### Also used to extract the gasdata object from cfp_dat
#' cfp_gasdata(ConFluxPro::base_dat)



#' @name cfp_gasdata
#'
#' @importFrom rlang .data
#'
#'@export
# helper
cfp_gasdata <- function(x,
                        ...){
  UseMethod("cfp_gasdata")
}

#' @rdname cfp_gasdata
#' @inheritParams cfp_profile
#' @exportS3Method
cfp_gasdata.data.frame <-
  function(x,
           id_cols,
           ...){


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("\nadded 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <- new_cfp_gasdata(x,
                id_cols)

  validate_cfp_gasdata(x)
  }

#' @rdname cfp_gasdata
#' @exportS3Method
cfp_gasdata.cfp_dat <-
  function(x,
           ...){
    rlang::check_dots_empty()
    get_gasdata(x)
  }


#'
# constructor
new_cfp_gasdata <- function(x,
                     id_cols){


  x <- new_cfp_profile(
    x,
    class = "cfp_gasdata",
    id_cols = id_cols)
  x
}


#'
# validator
validate_cfp_gasdata <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_gasdata"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("x_ppm","gas","depth")
  id_cols <- cfp_id_cols(x)

  stopifnot("data.frame lacks obligatory coluns" = base_cols %in% names(x),
            "id_cols must be present in the data.frame" = id_cols %in% names(x)
  )

  #check for NAs in id_cols
  stopifnot("id_cols cannot contain NAs" =
              anyNA(x[id_cols]) == FALSE)

  #check that at least two depths per group are present
  problem_groups <-
    x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::summarise(n_depths = length(unique(depth[!is.na(x_ppm)]))) %>%
    dplyr::filter(.data$n_depths < 2)

  stopifnot("There are combinations of id_cols with less than 2 non-NA depths" =
              nrow(problem_groups) == 0 )

  # no negative values in x_ppm
  any_negative_x_ppm <- min(x$x_ppm, na.rm = TRUE) < 0
  stopifnot("Negative mixing ratios are not allowed!" = !any_negative_x_ppm)

  x
}


# methods -----------

#' @exportS3Method
print.cfp_gasdata <- function(x, ...){
  NextMethod()
}
