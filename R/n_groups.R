#' n_groups
#'
#' Get the number of groups in cfp objects.
#'
#' @param x
#'
#' @export
n_groups <- function(x) {
  UseMethod("n_groups")
}


#' @exportS3Method
n_groups.cfp_dat <- function(x) {
  length(unique(x$profiles$group_id))
}
