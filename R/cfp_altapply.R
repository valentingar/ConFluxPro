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

  p <- progressr::progressor(length(X))

  if(is.null(names(X))) names(X) <- 1:length(X)

  X_env <- rlang::new_environment(data = list(X = X))

  purrr::pmap(.l = list(i = 1:length(X), run_id = names(X)),
             .f = function(i, run_id, p, X_env, ...){
               df <- FUN(X_env$X[i][[1]], ...)
               p()
               stopifnot("FUN must return a data.frame or NULL!" = is.data.frame(df) | is.null(df))
               if (is.null(df)){
                 df <- tibble::tibble()
               }
               df$run_id <- as.numeric(run_id)
               df
             },
             p = p,
             X_env = X_env,
             ...
  ) %>%
    dplyr::bind_rows()

}


