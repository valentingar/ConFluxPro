#' @title soilphys_layered
#'
#' @description This function calculates (harmonic-) means of the soilphys
#'   dataframe per layer for the flux calculation.
#'
#' @param soilphys (dataframe) the soilphys dataframe
#' @param layers_map (dataframe) containing the following parameters:
#' \describe{
#'   \item{upper}{upper limit of the layer in cm}
#'   \item{lower}{lower limit of the layer in cm}
#'   \item{layer}{the name of the layer}
#'   \item{...}{any \code{id_cols} necessary}
#'   }
#' @param param (character vector) A vector containing the names of the
#'   variables in soilphys to be carried over
#' @param funs (character vector) A vector defining the type of mean to be used.
#'   One of "arith" or "harm"
#' @param id_cols (character vector) A list of all columns, that, together,
#'   uniquely identify one profile.
#' @return soilphys, reduced to any \code{id_cols} and \code{param} columns and
#' summarised to the layers provided in \code{layers_map}
#'
#' #' @examples {data(soildiff)
#' layers_map <- data.frame(site = rep(c("site_a",
#'                                       "site_b"),each = 2),
#'                          upper = c(5,0,7,0),
#'                          lower = c(0,-100,0,-100),
#'                          layer = rep(c("HU","M1"),times = 2)
#' )
#'
#' soildiff$depth <- (soildiff$upper + soildiff$lower) / 2
#'
#' soilphys_layered(soilphys = soildiff,
#'                  layers_map = layers_map,
#'                  param = c("TPS","a"),
#'                  funs = c("harm","arith"),
#'                  id_cols = c("site"))

#' }
#' @family soilphys
#'
#' @import dplyr
#' @export



soilphys_layered <- function(soilphys,
                             layers_map,
                             param,
                             funs,
                             id_cols) {
  id_cols_s <- c(id_cols, "layer")

  param_arith <- param[funs == "arith"]
  param_harm <- param[funs == "harm"]

  if (any(param_harm %in% param_arith)) {
    warning("Some parameters appear to be calculated with multiple methods!")
  }

  soilphys <- soilphys %>%
    set_layer(layers_map = layers_map, id_cols = id_cols) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({
      id_cols_s
    }))) %>%
    dplyr::mutate(height = abs(upper - lower))

  soilphys_arith <- soilphys %>%
    dplyr::summarise(dplyr::across(
      .cols = any_of(!!param_arith),
      .fns =  ~ weighted.mean(.x, w = height, na.rm = T)
    ))

  soilphys_harm <- soilphys %>%
    dplyr::summarise(dplyr::across(
      .cols = any_of(!!param_harm),
      .fns =  ~ harm(.x, w = height, na.rm = T)
    ))

  soilphys <-
    dplyr::left_join(
      soilphys_harm,
      soilphys_arith,
      by = id_cols_s,
      suffix = c("_harm", "_arith")
    )
  soilphys <- dplyr::left_join(soilphys, layers_map)
  return(soilphys)
}



## helpers -------------

set_layer <- function(df, layers_map, id_cols) {
  layers_map$j_help <- 1
  id_lmap <- c(id_cols[id_cols %in% names(layers_map)], "j_help")

  df %>%
    mutate(j_help = 1) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_lmap))) %>%
    dplyr::group_modify( ~ {
      l_tmp <- .y %>% dplyr::left_join(layers_map, by = id_lmap)
      .x %>%
        dplyr::rowwise() %>%
        dplyr::mutate(layer = l_tmp$layer[l_tmp$upper > depth &
                                            l_tmp$lower < depth])
    })
}




