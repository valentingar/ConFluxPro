#' @title sobol_calc_indices
#'
#' @description From any result parameter and its corresponding run_map calculate
#' first-order and total sobol indices using the Azzini (2021) method.
#'
#' @param Y A data.frame with the desired effect parameter(s) of the model output. E.g.: efflux.
#'
#' @param effect_cols character vector of the column names in Y giving the target effects for which sobol indices
#' should be calculated
#'
#' @param id_cols character vector of column names specifying grouping columns. If the original model had
#' id_cols (e.g. different sites or gases) you may want to calculate indices per id_cols combination separately.
#'
#' @inheritParams alternate
#'
#' @export

sobol_calc_indices <- function(Y,
                               effect_cols,
                               id_cols = character(),
                               run_map){

  param_ids <- unique(cfp_params_df(run_map)$param_id)

  run_map %>%
    dplyr::select(run_id,
                  run_id_sobol,
                  sobol_mat,
                  param_id) %>%
    dplyr::distinct() %>%
    dplyr::left_join(Y, by = "run_id") %>%
    dplyr::mutate(param_count = ifelse(is.na(param_id), length(param_ids), 1)) %>%
    tidyr::uncount(param_count, .id = "param_id_new") %>%
    dplyr::mutate(param_id_new = param_ids[param_id_new]) %>%
    dplyr::mutate(param_id = ifelse(is.na(param_id), param_id_new, param_id)) %>%
    dplyr::select(!dplyr::any_of(c("param_id_new", "run_id"))) %>%
    dplyr::select(dplyr::any_of(c("param_id", id_cols, effect_cols,
                                "prof_id", "run_id_sobol", "sobol_mat"))) %>%
    tidyr::pivot_longer(cols = dplyr::any_of(effect_cols),
                        names_to = "effect_param",
                        values_to = "effect_value") %>%
    tidyr::pivot_wider(names_from = "sobol_mat",
                       values_from = "effect_value") %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(id_cols, "param_id", "effect_param")))) %>%
    summarise(Vt = sum((B - BA)^2+(A - AB)^2),
              Vi = sum((BA - B)*(A - AB)),
              VY = sum((A - B)^2+(BA - AB)^2)) %>%
    mutate(Si = 2*Vi/VY,
           ST = Vt/VY) %>%
    left_join(cfp_params_df(run_map), by = "param_id")


}
