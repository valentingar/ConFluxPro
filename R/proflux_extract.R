#' @title proflux_extract
#' @description Utility function for the fast extraction and aggregation of
#' parameters exported by pro_flux
#' @param PROFLUX The PFres object returned by \code{\link{pro_flux}}
#' @param id_cols The id_cols used in the function call(only needed
#' if PROFLUX is not a PFres object).
#' @return data.frame


#' EXPORT EFFLUX
#' @export
  pf_efflux <- function(PROFLUX,
                        ...) {

    UseMethod("pf_efflux",
              PROFLUX
              )

  }



#' @export
  pf_efflux.PFres <- function(PROFLUX,
                              ...) {

    id_cols <- pf_id_cols(PROFLUX)
    PROFLUX <- data.frame(PROFLUX)
    NextMethod(id_cols = id_cols)

  }



#' @export
  pf_efflux.data.frame <- function(PROFLUX,
                                   id_cols,
                                   ...) {

    #extract the topmost flux per date per id_col
    efflux <- get_efflux(PROFLUX,
                         id_cols = id_cols)
  }


#' EXPORT PROUDCTION
#' @export
  pf_prod <- function(PROFLUX,
                      ...) {
    UseMethod("pf_prod",
              PROFLUX
              )
  }



#' @export
  pf_prod.PFres <- function(PROFLUX,
                            ...) {
    id_cols <- pf_id_cols(PROFLUX)
    NextMethod(id_cols = id_cols)
  }



#' @export
  pf_prod.data.frame <- function(PROFLUX,
                                 id_cols,
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
      dplyr::summarise(
        prod = mean(prod,na.rm = T),
        dflux = sum(dprod))
  }

