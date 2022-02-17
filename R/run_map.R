#' @title run_map
#'
#' @description Create a run_map for model alteration in \code{alternate()}.
#'
#' @param params A named list of numeric vectors. Names indicate column names
#' in soilphys, vectors either distinct values (method permutation) or limits (method random).
#'
#' @param type A vector of length param indicating what the values in params
#' represent. One of
#' \describe{
#' \item{abs}{Absolute values that are applied as-is.}
#' \item{factor}{Factors to be multiplied with the original values.}
#' \item{addition}{Factors to be added to the original values.}
#'}
#'
#' @param n_runs Integer value of the number of alterations to be done for
#' method = random.
#'
#' @param layers_different Should layers from layers_map be changed individually?
#' If \code{TRUE} this allows for different changes at different depths.


#' @export


# function to create the necessary run_map
run_map <- function(x,
                    params = list(),
                    method = NULL,
                    type = NULL,
                    n_runs = NULL,
                    layers_different = FALSE
){

  method <- match.arg(method, c("random", "permutation"))
  type <- match.arg(type, c("abs", "factor", "addition" ),several.ok = TRUE)

  stopifnot("type must be length 1 or the same as params" =
              (length(type) == 1) | (length(type) = length(params)))

  stopifnot("all params must be present in soilphys!" =
              all(names(params) %in% c(names(x$soilphys), "topheight")))


  if (length(type) == 1){
    type <- rep(type, length(params))

  }

  type_df <- data.frame(param = names(params),
                        type = type
  )

  gases <- unique(x$profiles$gas)

  if(method == "permutation"){

    stopifnot("layers_different is only yet supported for 'random' method" = layers_different == FALSE)
    stopifnot("topheight change not yet supported for method = permutation" =
                !"topheight" %in% names(params))

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
                          param = rep(names(params), times = n_runs))

    if (layers_different == TRUE){
      run_map <-
        x$layers_map %>%
        dplyr::select(pmap,
                      dplyr::any_of({cfp_id_cols(x)})) %>%
        dplyr::right_join(run_map %>%
                     dplyr::filter(!param == "topheight"),
                   by = character()) %>%
        dplyr::bind_rows(x$layers_map %>%
                           dplyr::select(dplyr::any_of({cfp_id_cols(x)})) %>%
                           dplyr::distinct() %>%
                           dplyr::right_join(run_map, by = character()) %>%
                           dplyr::filter(param == "topheight"))
    }

    run_map <-
      run_map %>%
      dplyr::rowwise() %>%
      dplyr::mutate(value = runif(1, params[[param]][1], params[[param]][2])) %>%
      dplyr::left_join(type_df, by = "param") %>%
      dplyr::ungroup()

  }

  run_map <- lapply(gases,function(g){run_map$gas <- g; run_map}) %>%
    dplyr::bind_rows()



}
