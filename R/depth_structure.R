#' @title Unique layers depths
#'
#' @description
#' Get the unique layers or depths, i.e. the backbone
#' of an object given a set of identifying columns.
#'
#' @param x An object to get general structure of.
#' @inheritParams cfp_profile id_cols
#'
#' @name depth_structure
#' @export
depth_structure <- function(x, id_cols = NULL, ...){
  UseMethod("depth_structure")
}


#' @rdname depth_structure
#' @exportS3Method
depth_structure.cfp_layered_profile <- function(x, id_cols = NULL, ...){
  rlang::check_dots_empty()

  x <-
  x %>%
   dplyr::select(dplyr::all_of(c(id_cols, "upper", "lower"))) %>%
   dplyr::distinct() %>%
   dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
   dplyr::arrange(dplyr::desc(upper), .by_group = TRUE)


  if (is_ul_consistent(x, id_cols)){
    return(cfp_layered_profile(x, id_cols = id_cols))
  } else {
    message("Resulting profiles not correctly layered! More id_cols needed?")
    x <- cfp_profile(as.data.frame(x), id_cols)
  }
  x
}

#' @rdname depth_structure
#' @exportS3Method
depth_structure.cfp_profile <- function(x, id_cols = NULL, ...){
  rlang::check_dots_empty()
  stopifnot("only applicable for cfp_profiles with 'depth' column" =
              "depth" %in% names(x))


  x <-
    x %>%
    dplyr::select(dplyr::all_of(c(id_cols, "depth"))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::arrange(dplyr::desc(depth), .by_group = TRUE) %>%
    cfp_profile(id_cols)

  x
}


#' @rdname depth_structure
#' @exportS3Method
depth_structure.cfp_dat <- function(x, id_cols = NULL, structure_from = NULL, ...){
  rlang::check_dots_empty0()

  stopifnot("Must provide 'structure_from'" = !is.null(structure_from))
  structure_from <- match.arg(structure_from, c("gasdata",
                                                "soilphys",
                                                "layers_map"))

  id_cols_lmap <- cfp_id_cols(x$layers_map)
  x <- x[[structure_from]]

  id_cols_x <- cfp_id_cols(x)

  id_cols_common <- id_cols_lmap[id_cols_lmap %in% id_cols_x]
  depth_structure(x, id_cols_common)
}
