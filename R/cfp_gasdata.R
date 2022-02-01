#' @title cfp_gasdata
#' @description A function to create and validate cfp_gasdata objects. A gasdata
#' object is characterised by the following criteria: Each row is one observation
#' of the concentration of a gas at a depth and can be attributed to a distinct
#' profileidentified by any \code{id_cols} columns.
#' @param gasdata A \code{data.frame} with the following columns:
#' \describe{
#' \item{gas}{The gas of that observation.}
#' \item{depth (cm)}{The depth of the observation.}
#' \item{NRESULT_ppm (ppm)}{The observed concentration of that gas.}
#' \item{any of \code{id_cols}}{All id_cols that identify one profile uniquely.}
#'}
#'@inheritParams cfp_layers_map
#'
#'@return cfp_gasdata
#'


#'@export
# helper
cfp_gasdata <- function(gasdata,
                        id_cols){


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("added 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <- new_gdat(gasdata,
                id_cols)

  validate_cfp_gasdata(x)
}

#'
# constructor
new_gdat <- function(gasdata,
                     id_cols){

  structure(gasdata,
            class = c("cfp_gasdata","data.frame"),
            id_cols = id_cols)
}


#'
# validator
validate_cfp_gasdata <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_gasdata"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("NRESULT_ppm","gas","depth")
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
    dplyr::summarise(n_depths = length(unique(depth[!is.na(NRESULT_ppm)]))) %>%
    dplyr::filter(n_depths < 2)

  stopifnot("There are combinations of id_cols with less than 2 non-NA depths" =
              nrow(problem_groups) == 0 )

  x
}


# methods -----------

#' @exportS3Method
print.cfp_gasdata <- function(x){
  cat("\nA cfp_gasdata object \n")
  print_id_cols(x)
  cat("\n")
  NextMethod()
}
