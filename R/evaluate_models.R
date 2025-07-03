#' @title Evaluate model runs for calibration
#'
#' @description Evaluate the model runs produced by a call to [alternate()] with
#' user-defined error functions.
#'
#' @param x A [cfp_altres] object, as returned by [alternate()].
#' @param eval_funs A named list of evaluation functions. Each function must
#'   accept the arguments `x` and `param_cols` that are passed from this
#'   function.
#' @param eval_weights A vector of weights the same length of `eval_funs` or
#'   one. Alternatively a `data.frame()` that specifies the weight for any
#'   wished `error_parameter` (names of `eval_funs`) and `param_cols`
#'   combinations.
#'    Provide the weights as a numeric in the `parameter_weight` column.
#' @param eval_cols A character vector of columns for which the model error
#'   should be returned separately. Must be a subset of `param_cols` and
#'   defaults to the complete set.
#' @param n_best An integer number of runs to select as the best runs.
#' @param f_best A numeric between 0 to 1 as the fraction of runs to select as
#'   the best. Defaults to 0.01.
#' @param scaling_fun A scaling function. Defaults to min-median scaling.
#' @param ... Any arguments that need to be passed to the `error_funs`. Note
#'   that all matching arguments will be applied to each function!
#'
#' @inherit error_concentration params
#'
#' @returns A list with components `best_runs` the runs with the lowest model
#'   error (ME), `model_error` the model error for all runs,
#'   `models_evaluated` the raw values returned by error_funs and
#'   `best_runs_runmap`, a [cfp_run_map()] which can be used to rerun the
#'   `best_runs` model configurations. Note, that for `best_runs_runmap` the
#'   value of `run_id` is remapped to values `1:n_best`.
#'
#' @examplesIf FALSE
#' PROFLUX <- pro_flux(base_dat |> filter(site == "site_a"))
#'
#' run_map <-
#'  cfp_run_map(
#'    PROFLUX,
#'    params = list(TPS = c(0.9, 1.1)),
#'    type = "factor",
#'    n_runs = 5)
#'
#' PF_alt <- alternate(
#'   PROFLUX,
#'   \(x) complete_soilphys(x, DSD0_formula = "a*AFPS^b", quiet = TRUE),
#'   run_map)
#'
#' evaluate_models(
#'   PF_alt,
#'   eval_funs = list("NRMSE_conc" = error_concentration)
#'     )
#'
#'
#'
#' @export
evaluate_models <- function(x,
                            eval_funs = NULL,
                            eval_weights = 1,
                            param_cols,
                            eval_cols,
                            n_best = NULL,
                            f_best = 0.01,
                            scaling_fun = scale_min_median,
                            ...){
  UseMethod("evaluate_models")
}


#' @rdname evaluate_models
#' @exportS3Method
evaluate_models.cfp_altres <-
  function(x,
           eval_funs = NULL,
           eval_weights = 1,
           param_cols = cfp_id_cols(cfp_layers_map(cfp_og_model(x))),
           eval_cols = NULL,
           n_best = NULL,
           f_best = 0.01,
           scaling_fun = scale_min_median,
           ...){

  stopifnot("Provide at least one eval_funs" = !is.null(eval_funs))

  stopifnot("eval_funs must be a list of functions!" = is.list(eval_funs) &
              (all(vapply(eval_funs, is.function, FUN.VALUE = logical(1)))))

  stopifnot("all elements in eval_funs must be uniquely named!" =
              all(nchar(names(eval_funs))>0) &
              (length(unique(names(eval_funs))) == length(eval_funs)))

  stopifnot("eval_weights must be of length 1 or of
            the same length as eval_funs" =
              (length(eval_funs) == length(eval_weights)) |
              (length(eval_weights) == 1))

  stopifnot("f_best must be a fraction between 0 and 1" =
              0 <= f_best & f_best <= 1 )

  stopifnot("eval_cols must be a subset of param_cols" =
              all(eval_cols %in% param_cols))

  additional_args <- list(...)

  if(is.numeric(eval_weights)){
  weights_df <- data.frame(error_parameter = names(eval_funs),
                           parameter_weight = eval_weights)
  } else if(is.data.frame(eval_weights)){
    #stopifnot("Column 'error_parameter' missing from eval_weights" =
    #            "error_parameter" %in% names(eval_weights))
    stopifnot("Column 'parameter_weight' missing from eval_weights" =
                "parameter_weight" %in% names(eval_weights))
    stopifnot("Invalid columns in eval_weights data.frame!" =
                names(eval_weights) %in%
                c("error_parameter", "parameter_weight",
                  names(eval_funs), param_cols))
    weights_df <- eval_weights
  } else {
    stop("eval_weights not a numeric vector or data.frame!")
  }

  n_runs <- length(x)

  if (is.null(n_best)){
    n_best <- ceiling(n_runs * f_best)
  }

  x_eval_funs_applied <-
    lapply(FUN = apply_eval_fun,
           X = eval_funs,
           x = x,
           param_cols = param_cols,
           additional_args = additional_args) %>%
    dplyr::bind_rows(.id = "error_parameter") %>%
    dplyr::group_by(
      dplyr::across(dplyr::all_of(c("error_parameter", param_cols)))) %>%
    dplyr::mutate(sNRMSE = scaling_fun(.data$NRMSE)) %>%
    dplyr::left_join(weights_df,
                     by = whats_in_both(list(names(.data), names(weights_df))))

  x_model_error <-
    x_eval_funs_applied %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c("run_id", eval_cols)))) %>%
    dplyr::summarise(ME = sum(.data$sNRMSE * .data$parameter_weight)) %>%
    dplyr::arrange(.data$ME)


  best_runs <- x_model_error %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(eval_cols)))) %>%
    dplyr::slice_min(.data$ME, n = n_best)

  best_runs_runmap <- best_runs %>%
    dplyr::select(!"ME") %>%
    dplyr::left_join(cfp_run_map(x), by = c("run_id", eval_cols)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(eval_cols))) %>%
    dplyr::arrange(.data$run_id) %>%
    dplyr::mutate(run_id = tapply(.data$run_id, .data$run_id)) %>%
    dplyr::ungroup() %>%
    data.frame()

  rmap_attrs <- attributes(cfp_run_map(x))

  attributes(best_runs_runmap) <-
  c(attributes(best_runs_runmap),
    rmap_attrs[!names(rmap_attrs) %in% c("names", "row.names")])
  attr(best_runs_runmap, "n_runs") <- n_best

  list(best_runs = best_runs,
       model_error = x_model_error,
       models_evaluated = x_eval_funs_applied,
       best_runs_runmap = best_runs_runmap)
}



apply_eval_fun <- function(x,
                           param_cols,
                           cur_eval_fun,
                           additional_args){

  additional_args <- additional_args[names(additional_args) %in%
                                       names(formals(cur_eval_fun))]

  x_eval <-
  do.call(cur_eval_fun,
          args = c(list(
            x = x,
            param_cols = param_cols),
            additional_args))
  x_eval
}

#' @title Scale a vector by its min and median
#' @description
#' Scale a vector between its minimum and median.
#' @param x a numeric vector
#'
#' @returns \code{x} scaled between min and median of \code{x}.
#'
#' @examples
#' scale_min_median(1:10)
#'
#' @export
scale_min_median <- function(x){
  x_min <- min(x, na.rm = TRUE)
  x_median <- stats::median(x, na.rm = TRUE)

  (x - x_min) / (x_median - x_min)
}
