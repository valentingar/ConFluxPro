#' @title alternate
#'
#' @description Alternate cfp_pfres / cfp_fgres models for sensitivity analysis and
#' more.
#'
#' @param x A cfp_pfres or cfp_fgres model result.
#'
#' @param f A function taking in a soilphys object and recalculates the relevant
#' columns. See \code{complete_soilphys()}.
#'
#' @param run_map A data.frame created by \code{run_map()} with the necessary information
#' how the data is to be changed with each distinct \code{run_id}.
#'
#' @param return_raw Should the models be returned as is, or after applying any
#' \code{error_funs}. Default is \code{TRUE} - exporting the models.
#'
#' @param error_funs A list of functions to be applied after flux calculation
#' if \code{return_raw == FALSE}. This can be used to output not the models
#' but quality parameters instead. Output must contain the column RMSE.
#'
#' @param error_args A list of additional function arguments to be passed to any
#' of the \code{error_funs}. Must match the length of \code{error_funs}
#'
#' @details \code{alternate_model()} is used internally to change and rerun one model,
#' but can also be used to update a model with a given unique run_map, e.g. by filtering
#' the best run_id from the original \code{run_map}.
#'
#' @aliases alternate_model
#'
#' @export


alternate <- function(x,
                      f,
                      run_map,
                      return_raw = TRUE,
                      error_funs = NULL,
                      error_args = NULL){

  stopifnot(inherits(x, "cfp_pfmod") | inherits(x, "cfp_fgmod"))


  alternate_res <-
    lapply(split(run_map,run_map$run_id),
           apply_one_run,
           x = x,
           f = f,
           return_raw = return_raw,
           error_funs = error_funs,
           error_args = error_args)

  alternate_res
}






#helpers --------------
apply_one_run <- function(run_map,
                           x,
                           f,
                           error_funs,
                           error_args,
                           return_raw){

  y <- alternate_model(run_map,
                       x,
                       f)

  # return either the complete dataset
  if (return_raw == TRUE){
    return(y)
  }

  # otherwise:
  # calculate error parameters and return results
  df_ret <-
    apply_error_funs(y,
                     error_funs,
                     error_args)

  df_ret$run_id <- r_id
  df_ret
}

#' @rdname alternate
#' @export
alternate_model <- function(run_map,
                            x,
                            f){


  topheight <- NULL

  if ("topheight" %in% run_map$param) {
    topheight <- run_map %>%
      filter(param == "topheight")
    run_map <-  run_map %>%
      filter(!param == "topheight")
  }

  ## update parameters
  x$soilphys <- update_soilphys(x$soilphys,
                                run_map,
                                f
                                )
  # update topheight
  if (is.null(topheight) == FALSE){
  x <- update_topheight(x,
                        topheight)
  }

  ## rerun model
  y <- flux(x)
  y
}

update_soilphys <- function(soilphys,
                            run_map,
                            f){

  id_cols <- cfp_id_cols(soilphys)
  merger <- names(run_map)[names(run_map) %in% id_cols]

  new_params <- lapply(split(run_map, run_map$param),
                       update_param,
                       soilphys,
                       id_cols
                     ) %>%
    #do.call(dplyr::left_join(), args = list(by = merger))
    dplyr::bind_cols()

  soilphys <- soilphys %>%
    dplyr::select(!dplyr::any_of(run_map$param)) %>%
    dplyr::bind_cols(new_params)

  soilphys <- f(soilphys)
  soilphys <- cfp_soilphys(soilphys, id_cols)
}

update_param <- function(run_param,
                         soilphys,
                         id_cols){

    param <- run_param$param[1]
    merger <- names(run_param)[names(run_param) %in% c(id_cols,"pmap")]

    soilphys %>%
      dplyr::select(dplyr::any_of(c(
        param,
        id_cols,
        "pmap"))) %>%
      dplyr::left_join(run_param, by = merger) %>%
      dplyr::mutate(dplyr::across({param},
                                  ~dplyr::case_when(type == "factor" ~ .x * value,
                                             type == "abs" ~ value,
                                             type == "addition" ~ .x + value)
      )) %>%
      dplyr::select({param})
  }


update_topheight <-
  function(x,
           topheight){

    id_lmap <- cfp_id_cols(x$layers_map)
    id_gd <-cfp_id_cols(x$gasdata)
    id_sp <- cfp_id_cols(x$soilphys)

    topheight <- topheight %>% dplyr::select(!dplyr::any_of("pmap"))
    topheight_gd <- topheight_sp <- topheight %>%
      dplyr::left_join(x$profiles,
                       by = {id_lmap})
    topheight_gd <- topheight_gd %>% dplyr::select(gd_id, value, type)
    topheight_sp <- topheight_sp %>% dplyr::select(sp_id, value, type)

    x$layers_map <-
      x$layers_map %>%
      dplyr::left_join(topheight, by = id_lmap ) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_lmap))) %>%
      dplyr::mutate(upper = ifelse(upper == max(upper),
                                   change_param(upper,
                                                value,
                                                type),
                                   upper)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!dplyr::any_of(c("param",
                                     "type",
                                     "value"))) %>%
      new_cfp_layers_map(id_cols = id_lmap)

    x$soilphys <-
      x$soilphys %>%
      dplyr::left_join(topheight_sp, by = "sp_id") %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_sp))) %>%
      dplyr::mutate(upper = ifelse(upper == max(upper),
                                   change_param(upper,
                                                value,
                                                type),
                                   upper)) %>%
      dplyr::ungroup()%>%
      dplyr::select(!dplyr::any_of(c("param",
                                     "type",
                                     "value")))  %>%
      dplyr::mutate(height = (upper-lower) / 100,
                    depth = (upper + lower) / 2) %>%
      new_cfp_soilphys(id_cols = id_sp)

    x$gasdata <-
      x$gasdata %>%
      dplyr::left_join(topheight_gd, by = "gd_id") %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_gd))) %>%
      dplyr::mutate(depth = ifelse(depth == max(depth),
                                   change_param(depth,
                                                value,
                                                type),
                                   depth)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!dplyr::any_of(c("param",
                                     "type",
                                     "value"))) %>%
      new_cfp_layers_map(id_cols = id_lmap)


    x
  }

change_param <- function(a,value, type){
  a <- dplyr::case_when(type == "factor" ~ a * value,
                        type == "abs" ~ value,
                        type == "addition" ~ a + value)
}


##
apply_error_funs <- function(x,
                             error_funs,
                             error_args){


  df_ret <-
    lapply(1:length(error_funs), function(f_id) {
      error_args_tmp <-
        error_args[[f_id]]
      error_args_tmp$PROFLUX <- x # add PROFLUX argument

      df <-
        do.call(
          error_funs[[f_id]],
          error_args_tmp
        )

      df$error_param <- names(error_funs[f_id])
      df
    }) %>%
    dplyr::bind_rows()

  df_ret
}
