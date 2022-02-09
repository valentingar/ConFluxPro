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
                      run_map){

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
