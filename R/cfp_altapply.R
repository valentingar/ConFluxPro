#' @title cfp_altapply
#'
#' @description Apply a function to a list of cfp_pfres pr cfp_fgres objects
#' stored in an cfp_altres object. This can be used to summarise
#' alternate() results.
#'
#'
#' @param X Either a cfp_altres object or a list.
#'
#' @inheritParams base::lapply
#'
#' @return data.frame with the results of FUN bound together with added
#' column run_id as identifier of the original list elements.
#'
#' @export

cfp_altapply <- function(X,
                         FUN,
                         ...){
  UseMethod("cfp_altapply")
}

#' @exportS3Method
cfp_altapply.cfp_altres <- function(X,
                                    FUN,
                                    ...){
  NextMethod()
}

#' @exportS3Method
cfp_altapply.list <- function(X,
                              FUN,
                              ...){

  lapply(X = X,
         FUN = function(PF, ...){
           df <- FUN(PF, ...)
           stopifnot("FUN must return a data.frame or NULL!" = is.data.frame(df) | is.null(df))
           if (is.null(df)){
             df <- tibble::tibble()
           }
           df
         },
         ...
         ) %>%
    dplyr::bind_rows(.id = "run_id") %>%
    dplyr::mutate(run_id = as.numeric(run_id))

}


