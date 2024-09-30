#' @title evaluate_models
#'
#' @description
#' Evaluate the model runs produced by a call to [alternate()] with user-defined
#' error functions.
#'
#' @inheritParams error_funs
#'
#'
#' @export
evaluate_models <- function(x,
                            eval_funs = NULL,
                            param_cols,
                            scaling_fun = scale_min_median){
  UseMethod("evaluate_models")
}


#' @rdname evaluate_models
#' @exportS3Method
evaluate_models.cfp_altres <- function(x,
                                       eval_funs = NULL,
                                       param_cols = cfp_id_cols(cfp_layers_map(cfp_og_model(PF_alt))),
                                       scaling_fun = scale_min_median){

  stopifnot("Provide at least one eval_funs" = !is.null(eval_funs))

  stopifnot("eval_funs must be a list of functions!" = is.list(eval_funs) &
              (all(sapply(eval_funs,is.function))))

  stopifnot("all elements in eval_funs must be uniquely named!" =
              all(nchar(names(eval_funs))>0) &
              (length(unique(names(eval_funs))) == length(eval_funs)))

  #stopifnot("eval_args must be a list of the same length as eval_funs or NULL" =
  #            is.null(eval_args) | (is.list(eval_args) & (length(eval_args) == length(eval_funs))))


  x_eval_funs_applied <-
    lapply(FUN = apply_eval_fun,
           X = eval_funs,
           x = x) %>%
    dplyr::bind_rows(.id = "error_parameter") %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("error_parameter", param_cols)))) %>%
    dplyr::mutate(sNRMSE = scaling_fun(NRMSE))



  x_eval_funs_applied
}



apply_eval_fun <- function(x,
                           cur_eval_fun){

  # cur_eval_args <- c("x" = list(x),
  #                         cur_eval_args)
  #
  # stopifnot("formal arguments of eval_fun must match the names of eval_args!" =
  #             all(names((formals(cur_eval_fun))) %in% names(cur_eval_args)) &
  #             all(names(cur_eval_args) %in% names((formals(cur_eval_fun)))))

  x_eval <-
  do.call(cur_eval_fun, args = list(x=x))
  x_eval
}


scale_min_median <- function(x){
  x_min <- min(x, na.rm = TRUE)
  x_median <- median(x, na.rm = TRUE)

  (x - x_min) / (x_median - x_min)
}
