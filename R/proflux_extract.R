#' @title proflux_extract
#' @description Utility function for the fast extraction and aggregation of
#' parameters exported by pro_flux
#' @param PROFLUX The PFres object returned by \code{\link{pro_flux}}
#' @param id_cols The id_cols used in the function call(only needed
#' if PROFLUX is not a PFres object).
#' @return data.frame
#' @export
#'

  pf_efflux <- function(PROFLUX,
                        ...) {
    UseMethod("pf_efflux")
  }

  #' @export
  pf_efflux.PFres <- function(PROFLUX,
                              ...) {
    id_cols <- PF_id_cols(PROFLUX)
    NextMethod(id_cols = id_cols)
  }

  #' @export
  pf_efflux.data.frame <- function(PROFLUX,
                                   ...) {
    #extract the topmost flux per date per id_col
    efflux <- get_efflux(PROFLUX,
                         id_cols)
  }

  #' @export
  pf_prod <- function(PROFLUX,
                      ...) {
    UseMethod("pf_prod")
  }

  #' @export
  pf_prod.PFres <- function(PROFLUX,
                            ...) {
    id_cols <- PF_id_cols(PROFLUX)
    NextMethod(id_cols = id_cols)
  }

  #' @export
  pf_prod.data.frame <- function(PROFLUX,
                                 ...) {
    #extract the total production per layer
    prod <- get_prod(PROFLUX,
                     id_cols)
  }

  ####################
  ##### HELPERS ######
  ####################

  get_efflux <- function(PROFLUX,
                         id_cols) {
    PROFLUX %>%
      dplyr::select(dplyr::any_of({
        c(id_cols, "upper")
      })) %>%
      dplyr::distinct() %>%
      dplyr::group_by(dplyr::across(dplyr::any_of({
        id_cols
      }))) %>%
      dplyr::slice_max(upper) %>%
      dplyr::left_join(PROFLUX) %>%
      dplyr::select(dplyr::any_of({
        c(id_cols, "flux")
      })) %>%
      dplyr::rename(efflux = flux) %>%
      dplyr::ungroup()
  }


  get_prod <- function(PROFLUX,
                       id_cols) {
    PROFLUX %>%
      dplyr::mutate(dprod = prod * height) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of({
        c(id_cols, "layer")
      }))) %>%
      dplyr::summarise(dflux = sum(dprod))
  }

