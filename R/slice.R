#' Slice profiles
#'
#' @name slice
#'
#' @description
#' Slice profiles from a [cfp_dat()] object. This is built on [dplyr::slice()].
#'
#'
#' @inherit dplyr::slice
#'
#'
#' @rdname slice
#' @exportS3Method dplyr::slice
slice.cfp_dat <-
  function(.data, ..., .by = NULL, .preserve = FALSE){

    .data$profiles <- .data$profiles %>%
      dplyr::slice(...)

    reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_head
slice_head.cfp_dat <-
  function(.data, ..., .by = NULL, .preserve = FALSE){

  .data$profiles <- .data$profiles %>%
    dplyr::slice_head(...)

  reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_tail
slice_tail.cfp_dat <-
  function(.data, ..., .by = NULL, .preserve = FALSE){

    .data$profiles <- .data$profiles %>%
      dplyr::slice_tail(...)

    reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_min
slice_min.cfp_dat <-
  function(
    .data,
    order_by,
    ...,
    n,
    prop,
    by = NULL,
    with_ties = TRUE,
    na_rm = FALSE){

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_min
    fcall[[2]] <- .data$profiles
    .data$profiles <- eval(fcall, parent.frame())

    reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_max
slice_max.cfp_dat <-
  function(
    .data,
    order_by,
    ...,
    n,
    prop,
    by = NULL,
    with_ties = TRUE,
    na_rm = FALSE){

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_max
    fcall[[2]] <- .data$profiles
    .data$profiles <- eval(fcall, parent.frame())

    reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_sample
slice_sample.cfp_dat <-
  function(
    .data,
    order_by,
    ...,
    n,
    prop,
    by = NULL,
    with_ties = TRUE,
    na_rm = FALSE){

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_sample
    fcall[[2]] <- .data$profiles
    .data$profiles <- eval(fcall, parent.frame())

    reduce_cfp_profiles(.data)
  }

