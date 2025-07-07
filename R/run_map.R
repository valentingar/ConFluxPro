#'@title run_map
#'
#'@description
#' `r lifecycle::badge('deprecated')`
#'
#' `run_map()` was deprecated in favor of [cfp_run_map] for consistency.
#'
#' Create a cfp_run_map for model alteration in alternate()
#'
#' @inherit cfp_run_map
#'
#' @export
run_map <- function(x,
                    params = list(),
                    type = NULL,
                    method = NULL,
                    n_runs = NULL,
                    layers_different = FALSE,
                    layers_from = "layers_map",
                    layers_altmap = NULL,
                    topheight_adjust = FALSE

){

  lifecycle::deprecate_warn("1.0.7", "run_map()", "cfp_run_map()")

  cfp_run_map(x,
              params,
              type,
              method,
              n_runs,
              layers_different,
              layers_from,
              layers_altmap,
              topheight_adjust
              )

}

