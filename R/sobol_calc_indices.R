#' @title Calculate sobol indices
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' From any result parameter and its corresponding \code{cfp_run_map} calculate
#' first-order and total sobol indices using the Azzini (2021) method.
#'
#' @param Y A data.frame with the desired effect parameter(s) of the model
#' output, e.g. \code{efflux()}. The output should come from a list of model
#' results produced by a call to \code{\link{alternate}()} with a valid
#' \code{cfp_run_map} produced by \code{\link{sobol_run_map}()}.
#'
#' @param effect_cols character vector of the column names in \code{Y} for which
#' sobol indices should be calculated, e.g. \code{'efflux'}.
#'
#' @param id_cols character vector of column names in \code{Y} specifying
#' grouping variables. Indices are then calculated for each group individually.
#'
#' @param run_map The \code{cfp_run_map} used for the calculaton of Y
#' produced by a call to \code{\link{sobol_run_map}()}.
#'
#' @returns A \code{data.frame} with the following columns
#' \describe{
#' \item{\code{...}}{Any \code{id_cols} specified}
#' \item{\code{param_id, param, pmap}}{Parameter identificators from the
#' \code{cfp_run_map} used.}
#' \item{\code{effect_param}}{The parameter for which the effect was calulated.}
#' \item{\code{Vt, Vi, VY}}{Internal parameters for the indice calculation.}
#' \item{\code{Si}}{First order sobol indice.}
#' \item{\code{ST}}{Total order sobol indice.}
#' }
#'
#'
#' @details
#' This implements the approach outlined in Azzini et al (2021).
#'
#' @references Azzini, Ivano; Mara, Thierry A.; Rosati, Rossana: Comparison of
#' two sets of Monte Carlo estimators of Sobol’ indices, Environmental Modelling
#'  & Software, Volume 144, 2021, 105167, ISSN 1364-8152,
#'  https://doi.org/10.1016/j.envsoft.2021.105167
#'
#'
#' @family sobol
#'
#'
#' @importFrom rlang .data
#'
#'
#' @export

sobol_calc_indices <- function(Y,
                               effect_cols,
                               id_cols = character(),
                               run_map){

  stopifnot("Not all effect_cols are in present in Y." = all(effect_cols %in% names(Y)))
  stopifnot("Not all id_cols are in present in Y." = all(id_cols %in% names(Y)))
  stopifnot("Not all run_id in Y - did you use a different run_map?" =
              all(unique(run_map$run_id) %in% Y$run_id))

  param_ids <- unique(cfp_params_df(run_map)$param_id)

  run_map %>%
    dplyr::select("run_id",
                  "run_id_sobol",
                  "sobol_mat",
                  "param_id_main") %>%
    dplyr::distinct() %>%
    dplyr::left_join(Y, by = "run_id", relationship = "many-to-many") %>%
    #dplyr::mutate(param_count = ifelse(is.na(param_id), length(param_ids), 1)) %>%
    #tidyr::uncount(.data$param_count, .id = "param_id_new") %>%
    #dplyr::mutate(param_id_new = .data$param_ids[.data$param_id_new]) %>%
    #dplyr::mutate(param_id = ifelse(is.na(.data$param_id), .data$param_id_new, .data$param_id)) %>%
    #dplyr::select(!dplyr::any_of(c("param_id_new", "run_id"))) %>%
    dplyr::rename(param_id = "param_id_main") %>%
    dplyr::select(dplyr::any_of(c("param_id", id_cols, effect_cols,
                                "prof_id", "run_id_sobol", "sobol_mat"))) %>%
    tidyr::pivot_longer(cols = dplyr::any_of(effect_cols),
                        names_to = "effect_param",
                        values_to = "effect_value") %>%
    tidyr::pivot_wider(names_from = "sobol_mat",
                       values_from = "effect_value") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(id_cols, "param_id", "effect_param")))) %>%
    dplyr::summarise(Vt = sum((.data$B - .data$BA)^2 + (.data$A - .data$AB)^2),
              Vi = sum((.data$BA - .data$B) * (.data$A - .data$AB)),
              VY = sum((.data$A - .data$B)^2 + (.data$BA - .data$AB)^2)) %>%
    dplyr::mutate(Si = 2*.data$Vi / .data$VY,
           ST = .data$Vt / .data$VY) %>%
    dplyr::left_join(cfp_params_df(run_map), by = "param_id", relationship = "many-to-many")


}
