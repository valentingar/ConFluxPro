#' @title efflux
#'
#' @description Calculate or extract the soil/atmosphere efflux
#' from \code{cfp_pfres} or \code{cfp_fgres} model results.
#'
#' @param x A cfp_pfres or cfp_fgres model result.
#'
#' @importFrom rlang .data
#'
#' @export

efflux <- function(x, ...){
  UseMethod("efflux")
}

#' @rdname efflux
#' @exportS3Method
efflux.cfp_pfres <- function(x, ...){
  rlang::check_dots_empty()
  pf_efflux(x)
}

#' @rdname efflux
#' @param method Method(s) used to interpolate the efflux at the top of the soil
#' from partial fluxes within the soil. One of
#' \describe{
#' \item{top}{Use the flux in the topmost model layer.}
#' \item{lm}{A linear model where each partial flux is centered in the respective
#' layer and the model is evaluated at the top of the soil.}
#' \item{lex}{Linearly exterpolate using fluxes of two layers in the soil.}
#' }
#' @param layers Vector of two integers selecting the layers for the \code{lex}
#' method. Layers are indexed from 1 (topmost) to the number of layers used in the
#' flux calculation.
#' @exportS3Method
efflux.cfp_fgres <- function(x,
                             ...,
                             method = "lm",
                             layers = NULL){

  rlang::check_dots_empty()

  fg_efflux(x, method = method, layers = layers)
}

# helpers ----------------------
pf_efflux <- function(x) {

  PROFLUX <- x$PROFLUX
  profiles <- x$profiles

  id_cols <- cfp_id_cols(x)
  merger <- id_cols[id_cols %in% names(PROFLUX)] %>% c("step_id")

  PROFLUX %>%
    dplyr::group_by(.data$prof_id) %>%
    dplyr::arrange(dplyr::desc(.data$step_id)) %>%
    dplyr::summarise(efflux = .data$flux[1],
                     dplyr::across(dplyr::any_of(c("DELTA_flux", "mean_flux")), ~.x[1])) %>%
    dplyr::rename(dplyr::any_of(c(DELTA_efflux = "DELTA_flux",
                                  mean_efflux = "mean_flux"))) %>%
    dplyr::left_join(profiles, by = "prof_id") %>%
    dplyr::select(dplyr::any_of({
      c(id_cols, "efflux", "prof_id", "DELTA_efflux", "mean_efflux")
    })) %>%
    dplyr::ungroup() %>%
    cfp_profile(id_cols = id_cols)
}


fg_efflux <- function(x,
                      method,
                      layers
                      ){

  method <- match.arg(method, c("top", "lm", "lex"), several.ok = TRUE)

  stopifnot("Only one method can be applied" =
              length(method) == 1)

  stopifnot("length of layers must be 2!" = is.null(layers) || length(layers) == 2)

  if(method == "top"){
    EFFLUX <- get_top_efflux(x)
  } else if (method == "lm"){
    EFFLUX <- get_lm_efflux(x)
  } else if (method == "lex"){
    stopifnot("layers must be supplied for method = lex" =
                !is.null(layers))
    stopifnot("undefined layer selected - check that all layers are present!",
              !any(layers > max(FLUX$FLUX$layer)))
    EFFLUX <- get_lex_efflux(x, layers)
  }

  EFFLUX %>%
    dplyr::left_join(x$profiles,
                     by = "prof_id") %>%
    dplyr::ungroup() %>%
    dplyr::select(!dplyr::any_of(c("gd_id", "sp_id", "group_id")))%>%
    cfp_profile(id_cols = cfp_id_cols(x))
}


get_top_efflux <- function(x){
  FLUX <- x$FLUX

  FLUX %>%
    dplyr::group_by(mode, prof_id) %>%
    dplyr::slice_max(upper) %>%
    dplyr::select(dplyr::any_of(c("prof_id", "mode", "flux"))) %>%
    dplyr::rename(efflux = flux)

}

get_lm_efflux <- function(x){
  FLUX <- x$FLUX

  FLUX %>%
    dplyr::group_by(mode, prof_id) %>%
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
  FLUX <- x$FLUX

  FLUX %>%
    dplyr::group_by(.data$mode, .data$prof_id) %>%
    dplyr::mutate(depth = (.data$upper + .data$lower) / 2,
                  topheight = max(.data$upper)) %>%
    dplyr::arrange(dplyr::desc(.data$depth)) %>%
    dplyr::slice(layers) %>%
    dplyr::group_modify(~{
      h<-.x$topheight[1]
      efflux <- tryCatch(lin_extrap(.x$depth, .x$flux,h),
                         error = function(x) NA)
      data.frame(efflux = efflux)
    })
}

