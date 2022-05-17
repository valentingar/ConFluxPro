#' @title cfp_altres
#'
#' @description S3 class for the result of a call to alternate(). Essentially a list with
#' added run_map and further attributes.
#'
#' @param x A named list of cfp_pfres or cfp_fgres models.
#'
#' @param og_model The original model that was altered
#'
#' @inheritParams alternate
#'
#' @export


cfp_altres <- function(x,
                       og_model,
                       f,
                       run_map,
                       return_raw = TRUE,
                       error_funs = NULL,
                       error_args = NULL){

  stopfinot(is.list(x))

  x <- new_cfp_altres(x = x,
                      og_model = og_model,
                      f = f,
                      run_map = run_map,
                      return_raw = return_raw,
                      error_funs = error_funs,
                      error_args = error_args)

  x <- validate_cfp_altres(x)
  x
}

# constructor -----------------

new_cfp_altres <- function(x,
                           og_model,
                           f,
                           run_map,
                           return_raw,
                           error_funs,
                           error_args){

  x <- structure(x,
                 class = c("cfp_altres", class(x)),
                 og_model = og_model,
                 f = f,
                 run_map = run_map,
                 return_raw = return_raw,
                 error_funs = error_funs,
                 error_args = error_args)


  x
}


# validator ------------------

validate_cfp_altres <- function(x){

  stopifnot(inherist(x, "cfp_altres"))


  run_map <- cfp_run_map(x)
  runs <- unique(run_map$run_id)

  stopifnot("Length of result list does not math run_map" = length(runs) == length(x))


  x
}


# methods -----------------------


###### EXTRACTION ######

#' @export
cfp_og_model <- function(x){
  UseMethod("cfp_og_model")
}
#' @exportS3Method
cfp_og_model.cfp_altres <- function(x){
  out <- attr(x, "og_model")
}




###### PRINTING #######
#' @exportS3Method
print.cfp_altres <- function(x, ...){
  n <- length(x)

  cat("\nA cfp_altres model result list. \n")
  cat("number of runs: ", n,"\n")
  cat("original model: \n")
  print(cfp_og_model(x))
}

