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

    .data$profiles <- profiles_to_grouped_df(.data$profiles)

    .data$profiles <-
      new_cfp_profile(
        slice(.data$profiles, ...),
        id_cols = "prof_id"
      )

    reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_head
slice_head.cfp_dat <-
  function(.data, ..., .by = NULL, .preserve = FALSE){

    .data$profiles <- profiles_to_grouped_df(.data$profiles)

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_head
    fcall[[2]] <- .data$profiles

    .data$profiles <-
      new_cfp_profile(
        eval(fcall),
        id_cols = "prof_id"
      )

    reduce_cfp_profiles(.data)
  }

#' @rdname slice
#' @exportS3Method dplyr::slice_tail
slice_tail.cfp_dat <-
  function(.data, ..., .by = NULL, .preserve = FALSE){

    .data$profiles <- profiles_to_grouped_df(.data$profiles)

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_tail
    fcall[[2]] <- .data$profiles

     .data$profiles <-
      new_cfp_profile(
        eval(fcall),
        id_cols = "prof_id"
      )

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

    .data$profiles <- profiles_to_grouped_df(.data$profiles)

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_min
    fcall[[2]] <- .data$profiles

    .data$profiles <-
      new_cfp_profile(
        eval(fcall),
        id_cols = "prof_id"
      )

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

    .data$profiles <- profiles_to_grouped_df(.data$profiles)

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_max
    fcall[[2]] <- .data$profiles

     .data$profiles <-
      new_cfp_profile(
        eval(fcall),
        id_cols = "prof_id"
      )

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

    .data$profiles <- profiles_to_grouped_df(.data$profiles)

    fcall <- match.call(expand.dots = FALSE)
    fcall[[1]] <- slice_sample
    fcall[[2]] <- .data$profiles

    .data$profiles <-
      new_cfp_profile(
        eval(fcall),
        id_cols = "prof_id"
      )

    reduce_cfp_profiles(.data)
  }


### helper -------
profiles_to_grouped_df <- function(profiles){
  if("groups" %in% names(attributes(profiles))){
    profiles <-
      dplyr::new_grouped_df(
        profiles,
        groups = attr(profiles, "groups"))
  }
  profiles
}
