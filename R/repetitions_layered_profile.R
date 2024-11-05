#' @title Repetitions in layered profiles
#'
#' @description \code{check_matching_repetitions()} compares the structure of
#' layered profile objects. A [cfp_layered_profile()] object is split along
#' parameter(s) defined in \code{rep_cols}. Returns TRUE if all profiles match
#' in their structure.
#'
#' @param x A \code{cfp_layered_profile} object.
#' @param rep_cols A character vector of columns that define repetitions of
#'   profiles within \code{x}. Must be a subset of the \code{id_cols} of x.
#'
#' @keywords internal
check_matching_repetitions <- function(x, rep_cols){

  x_id_cols <- cfp_id_cols(x)

  if (any(!(rep_cols %in% x_id_cols))){
    message("Some rep_cols are no id_cols of x.")
    return(FALSE)
  }
  if (all(x_id_cols %in% rep_cols)){
    message("rep_cols cannot include all id_cols of x.")
    return(FALSE)
  }

  x_id_cols_new <- x_id_cols[!x_id_cols %in% rep_cols]

  new_structure <-
  x %>%
    dplyr::ungroup() %>%
    dplyr::select(
      dplyr::all_of(c(x_id_cols,
                      "upper",
                      "lower"))) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(x_id_cols))) %>%
    dplyr::mutate(upper_max = max(upper),
                  lower_min = min(lower)) %>%
    dplyr::ungroup() %>%
    dplyr::select(!dplyr::all_of(rep_cols)) %>%
    dplyr::distinct()

  ret <- is_ul_consistent(new_structure, x_id_cols_new)
  if (ret == FALSE){
    message("Cannot combine along ", x_id_cols_new,
            " into valid cfp_layered_profile.")
  }

  ret
}
