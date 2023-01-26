#' Automatically plan for parallel computing in cfp.
#'
#' @param x A cfp_dat object.
#' @param number_of_workers Number of session to create in total.
#' Defaults to number of available cores.
#'
#' @export

plan_parallel_automatic <- function(x, number_of_workers){
  UseMethod("plan_parallel_automatic")
}

#' @exportS3Method
plan_parallel_automatic.cfp_dat <-
  function(x,
           number_of_workers = future::availableCores()){

    number_of_groups = n_groups(x)

    future::plan(
      list(
        future::tweak(
          future::multisession,
          workers = number_of_groups),
        future::tweak(
          future::multisession,
          workers = future::availableCores() %/% number_of_groups
        ))
    )
  }
