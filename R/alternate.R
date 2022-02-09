#' @title alternate
#'
#' @description Alternate cfp_pfres / cfp_fgres models for sensitivit analysis and
#' more.
#'
#' @param x A cfp_pfres or cfp_fgres model result.
#'
#' @export

alternate <- function(x,
                      f,
                      run_map,
                      error_funs,
                      error_args,
                      return_raw){

  stopifnot(inherits(x, "cfp_pfmod") | inherits(x, "cfp_fgmod"))


  alternate_res <-
    lapply(split(run_map,run_map$run_id),
           alternate_model,
           x = x,
           f = f,
           return_raw = return_raw,
           error_funs = error_funs,
           error_args = error_args)

  alternate_res
}






#helpers --------------
# function to create the necessary run_map
create_runs <- function(x,
                        params = list(),
                        method = NULL,
                        type = NULL,
                        n_runs = NULL,
                        layers_different = FALSE
                        ){

  method <- match.arg(method, c("random", "permutation"))
  type <- match.arg(type, c("abs", "factor", "addition" ))

  stopifnot("type must be length 1 or the same as params" =
              (length(type) == 1) | (length(type) = length(params)))

  stopifnot("all params must be present in soilphys!" =
              all(names(params) %in% names(x$soilphys)))

  type_df <- data.frame(param = names(params),
                        type = ifelse(length(type == 1),
                                      rep(type, length(params)),
                                      type)
  )

  gases <- unique(x$profiles$gas)

  if(method == "permutation"){

    run_map <- expand.grid(params) %>%
      dplyr::mutate(run_id = dplyr::row_number()) %>%
      tidyr::pivot_longer(cols = !"run_id",
                          names_to = "param",
                          values_to = "value") %>%
      dplyr::left_join(type_df, by = "param")

  } else if (method == "random"){

    stopifnot("For method = 'random' give exactly two values per param as limits" =
                all(sapply(params,length) == 2))

    params <- lapply(params, sort)

    run_map <- data.frame(run_id = rep(1:n_runs, each = length(params)),
                          param = rep(names(params), times = n_runs)) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = runif(1, params[[param]][1], params[[param]][2])) %>%
      dplyr::left_join(type_df, by = "param") %>%
      dplyr::ungroup()

  }

  run_map <- lapply(gases,function(g){run_map$gas <- g; run_map}) %>%
    dplyr::bind_rows()

}


alternate_model <- function(run_map,
                            x,
                            f,
                            error_funs,
                            error_args,
                            return_raw){

  ## update parameters
  x$soilphys <- update_soilphys(x$soilphys,
                                run_map,
                                f)

  ## rerun model
  y <- flux(x)

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
    merger <- names(run_param)[names(run_param) %in% id_cols]

    soilphys %>%
      dplyr::select(dplyr::any_of(c(
        param,
        id_cols))) %>%
      dplyr::left_join(run_param, by = merger) %>%
      dplyr::mutate(dplyr::across({param},
                                  ~dplyr::case_when(type == "factor" ~ .x * value,
                                             type == "abs" ~ value,
                                             type == "addition" ~ .x + value)
      )) %>%
      dplyr::select({param})
  }
