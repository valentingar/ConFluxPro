#' @title Split by profile
#'
#' @description
#' Split profile data into a list for each single profile.
#' @param x A object that is grouped into profiles by its \code{id_cols}.
#'
#' @returns A list with where each entry is one profile of the same
#' class as \code{x}.
#'
#' @examples
#' df <- cfp_profile(
#'   data.frame(
#'       site = rep(c("site_a", "site_b"),
#'                  each = 2),
#'       variable = 1:4),
#'    id_cols = "site")
#' split_by_prof(df)
#'
#' base_dat <- ConFluxPro::base_dat
#' split_by_prof(base_dat)



#' @name split_by_prof
#' @export
split_by_prof <- function(x){
  UseMethod("split_by_prof")
}

#' @rdname split_by_prof
#' @exportS3Method
split_by_prof.cfp_dat <- function(x){

  profiles <- x$profiles
  profiles_list <- profiles %>%
    dplyr::arrange(.data$prof_id) %>%
    dplyr::group_by(.data$prof_id) %>%
    dplyr::group_split()

  soilphys_list <-
    profiles %>%
    dplyr::select(sp_id, prof_id) %>%
    dplyr::left_join(x$soilphys, by = "sp_id") %>%
    dplyr::arrange(.data$prof_id) %>%
    dplyr::group_by(.data$prof_id) %>%
    dplyr::group_split()

  gasdata_list <-
    profiles %>%
    dplyr::select(gd_id, prof_id) %>%
    dplyr::left_join(x$gasdata, by = "gd_id") %>%
    dplyr::arrange(.data$prof_id) %>%
    dplyr::group_by(.data$prof_id) %>%
    dplyr::group_split()

  lmap <- x$layers_map

  atts <- attributes(x)


  out <-
    mapply(profiles_list,
           soilphys_list,
           gasdata_list,
           MoreArgs = list(
             lmap = lmap,
             id_cols = cfp_id_cols(x),
             atts = list(atts)),
           FUN = function(profs,
                          sp,
                          gd,
                          lmap,
                          id_cols,
                          atts){
             cfp_dat_group <- new_cfp_dat(gd[!colnames(gd) == "prof_id"],
                                          sp[!colnames(sp) == "prof_id"],
                                          lmap,
                                          profs,
                                          id_cols = id_cols)
             attributes(cfp_dat_group) <- unlist(atts, recursive = FALSE)
             cfp_dat_group
           },
           SIMPLIFY = FALSE)
  out
}

#' @rdname split_by_prof
#' @exportS3Method
split_by_prof.cfp_profile <- function(x){
  id_cols <- cfp_id_cols(x)
  x_class <- class(x)
  x_class <- x_class[!(x_class) %in% class(cfp_profile(data.frame()))]

  x <-
    x %>%
    dplyr::group_by(dplyr::across(tidyr::all_of(id_cols))) %>%
    dplyr::group_split() %>%
    lapply(data.frame) %>%
    lapply(new_cfp_profile,
           id_cols = id_cols,
           class = x_class)
  x

}





