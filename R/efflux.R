#' @title efflux
#'
#' @description Calculate or extract the soil/atmosphere efflux
#' from cfp_pfres or cfp_fgres model results.
#'
#' @param x A cfp_pfres or cfp_fgres model result.
#'
#' @param ... Further arguments passed on for cfp_fgres efflux calculation.
#'
#'
#' @export

efflux <- function(x, ...){
  UseMethod("efflux")
}

#' @exportS3Method
efflux.cfp_pfres <- function(x, ...){
  pf_efflux(x)
}

#' @exportS3Method
efflux.cfp_fgres <- function(x, ...){
  fg_efflux(x, ...)
}

# helpers ----------------------

pf_efflux <- function(x) {

  PROFLUX <- x$PROFLUX
  profiles <- x$profiles
  id_cols <- cfp_id_cols(x)
  merger <- id_cols[id_cols %in% names(PROFLUX)] %>% c("step_id")

  PROFLUX %>%
    dplyr::select(step_id, prof_id) %>%
    dplyr::distinct() %>%
    dplyr::group_by(prof_id) %>%
    dplyr::slice_max(step_id) %>%
    dplyr::left_join(PROFLUX,
                     by = c("prof_id","step_id")) %>%
    dplyr::left_join(profiles, by = "prof_id") %>%
    dplyr::select(dplyr::any_of({
      c(id_cols, "flux")
    })) %>%
    dplyr::rename(efflux = flux) %>%
    dplyr::ungroup()
}


fg_efflux <- function(x,
                      method,
                      layers = NULL
                      ){

  method <- match.arg(method, c("top", "lm", "lex"), several.ok = TRUE)

  stopifnot("Only one method can be applied" =
              length(method) == 1)

  stopifnot("length of layers must be 2!" = is.null(layers) || length(layers) == 2)

  stopifnot("layers must be supplied for method = lex" =
              method == "lex" && !is.null(layers))

  if(method == "top"){
    return(get_top_efflux(x))
  } else if (method == "lm"){
    return(get_lm_efflux(x))
  } else if (method == "lex"){
    return(get_lex_efflux(x, layers))
  }



}


get_top_efflux <- function(x){
  id_cols <- cfp_id_cols(x)
  FLUX <- x$FLUX

  FLUX %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::slice_max(upper) %>%
    dplyr::select(dplyr::any_of(c(id_cols, "mode", "flux"))) %>%
    dplyr::rename(efflux = flux)

}

get_lm_efflux <- function(x){
  id_cols <- cfp_id_cols(x)
  FLUX <- x$FLUX

  FLUX %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(depth = (upper + lower) / 2,
                  topheight = max(upper)) %>%
    dplyr::group_modify(~{
      h<-.x$topheight[1]
      if (nrow(.x %>% dplyr::filter(is.na(depth)==F,is.na(flux)==F))<2){
        efflux <- NA
      } else {
        mod <- lm(flux~depth, data = .x)
        efflux <- predict(mod, newdata = list(depth = h))
      }
      data.frame(efflux = efflux)
    })

}

get_lex_efflux <- function(x,
                                 layers){
  id_cols <- cfp_id_cols(x)
  FLUX <- x$FLUX

  FLUX %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(depth = (upper + lower) / 2,
                  topheight = max(upper)) %>%
    dplyr::arrange(desc(depth)) %>%
    dplyr::slice(layers) %>%
    dplyr::group_modify(~{
      h<-.x$topheight[1]
      mod <- lm(flux~depth, data = .x)

      efflux <- tryCatch(lin_extrap(.x$depth, .x$flux,h),
                         error = function(x) NA)
      data.frame(efflux = efflux)
    })
}

