#' @title proflux_summarise
#'
#' @description Summarises key parameters of a PFres object
#' (produced by a call to \code{\link{pro_flux}}) based on the id_cols of its layers_map.
#'
#' @inheritParams pro_flux
#' @param PROFLUX Either an object of class \code{\link{PFres}}
#' (see \code{\link{pro_flux}}), or a data.frame following the same convention.
#'
#'
#'
#' @name proflux_summarise
#' @aliases proflux_summarize
#'
#' @export

proflux_summarise <- function(PROFLUX,
                              ...){
  UseMethod("proflux_summarise",
            PROFLUX)
}

#' @rdname proflux_summarise
#' @export
proflux_summarize <- proflux_summarise



#' @rdname proflux_summarise
#' @export
proflux_summarise.data.frame <- function(PROFLUX,
                                         layers_map,
                                         id_cols,
                                         ...){

  id_cols_tmp <- id_cols[id_cols %in% names(layers_map)]

  ans <- get_summary(PROFLUX = PROFLUX,
                     id_cols_tmp = id_cols_tmp,
                     layers_map = layers_map,
                     id_cols = id_cols)
  ans
}

#' @rdname proflux_summarise
#' @export
proflux_summarise.PFres <- function(PROFLUX,
                          ...){

  id_cols <- pf_id_cols(PROFLUX)
  layers_map <- pf_layers_map(PROFLUX)

  #id_cols_tmp <- id_cols[id_cols %in% names(layers_map)]

  NextMethod(id_cols = id_cols,
             layers_map = layers_map)
}


###### HELPERS ######

get_summary <- function(PROFLUX,
                        id_cols_tmp,
                        ...){

  EFFLUX <- pf_efflux(PROFLUX,
                      ...) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols_tmp))) %>%
    dplyr::summarise(efflux = mean(efflux,na.rm = T))

  PROD <- pf_prod(PROFLUX,
                  ...) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c("layer",id_cols_tmp)))) %>%
    dplyr::summarise(prod = mean(prod, na.rm = T),
                     dflux = mean(dflux,na.rm = T))


  ans <- list(PROD = PROD,
              EFFLUX = EFFLUX)
  class(ans) <- "PFsummary"
  ans

}
