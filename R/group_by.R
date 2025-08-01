#' @name group_by
#'
#' @description
#' Group profiles within a [cfp_dat()] object to change the behavior of
#' [filter()] and [slice()]. This is built on [dplyr::filter()].
#'
#' @inherit dplyr::group_by
#'
#'
#' @export
group_by.cfp_dat <- function(
    .data,
    ...,
    .add = FALSE,
    .drop = dplyr::group_by_drop_default(.data)){

  .data$profiles <- group_by(.data$profiles, ..., .add = .add, .drop = .drop)
  class(.data$profiles) <- c("cfp_profile", "data.frame")
  .data
}
