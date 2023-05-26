#' @title run_map
#'
#' @description Create a run_map for model alteration in \code{alternate()}.
#'
#' @param x Either a \link{cfp_pfres} or \link{cfp_fgres} model result.
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
#' @param method Either 'random', where a random value is chosen within
#' the bounds set in \code{params} or 'permutation', where every permutation of
#' the values in \code{params} is added.
#'
#' @param n_runs Integer value of the number of alterations to be done for
#' method = 'random'.
#'
#' @param layers_different Should layers from layers_map be changed individually?
#' If \code{TRUE} this allows for different changes at different depths.
#'
#' @param topheight_adjust (logical) If the proposed change in topheight is larger
#' than the highest layer in soilphys, should the limits be automatically
#' adjusted per id_cols indivdually? Default is FALSE, which leads to an error in that
#' case.


#' @export


# function to create the necessary run_map
run_map <- function(x,
                    params = list(),
                    type = NULL,
                    method = NULL,
                    n_runs = NULL,
                    layers_different = FALSE,
                    layers_from = "layers_map",
                    topheight_adjust = FALSE
){

  method <- match.arg(method, c("random", "permutation"))
  type <- match.arg(type, c("abs", "factor", "addition" ),several.ok = TRUE)

  stopifnot("type must be length 1 or the same as params" =
              (length(type) == 1) | (length(type) = length(params)))

  stopifnot("all params must be present in soilphys!" =
              all(names(params) %in% c(names(x$soilphys), "topheight")))

  stopifnot("please give an integer number of runs" =
              ((!(method == "random")) || (is.numeric(n_runs) && (abs(round(n_runs) - n_runs) < 1E-10))))

  layers_from = match.arg(layers_from, c("layers_map", "soilphys"))


  if (length(type) == 1){
    type <- rep(type, length(params))

  }

  type_df <- data.frame(param = names(params),
                        type = type
  )

  gases <- unique(x$profiles$gas)


  # get second highest depth per ids in layers_map
  if ("topheight" %in% names(params)){

    second_depth <-
    x$soilphys %>%
      dplyr::select(dplyr::any_of(c(cfp_id_cols(x$layers_map), "upper"))) %>%
      dplyr::distinct() %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(cfp_id_cols(x$layers_map)))) %>%
      dplyr::slice_max(upper, n = 2) %>%
      dplyr::summarise(top = max(upper),
                       bottom = min(upper))

    if (any((second_depth$top-second_depth$bottom) <= -min(params$topheight))){
      if (topheight_adjust == TRUE){
        message("adjusting topheight limits to fit data.")

        #something to do just that.

      } else {
        stop("topheight change too large for (some) profiles! \n
             Choose topheight_adjust = TRUE for autmatic, individual limits.")
      }

    }

  }

  if (method == "permutation"){
    run_map <- run_map_permutation(x,
                                   params,
                                   n_runs,
                                   layers_different,
                                   layers_from,
                                   topheight_adjust,
                                   type_df,
                                   second_depth)
  } else if (method == "random"){
      run_map <- run_map_random(x,
                                params,
                                n_runs,
                                layers_different,
                                layers_from,
                                topheight_adjust,
                                type_df,
                                second_depth)
    }

  id_cols_all <- cfp_id_cols(x)
  id_cols_runmap <- id_cols_all[id_cols_all %in% names(run_map)]

  if (!"gas" %in% id_cols_runmap){
  # add at least basic id_cols (gas)
  run_map <- lapply(gases, function(g){run_map$gas <- g; run_map}) %>%
    dplyr::bind_rows()

  id_cols_runmap <- c(id_cols_runmap, "gas")
  }

  params_df <- run_map %>%
    dplyr::select(dplyr::any_of(c("pmap", "param", "upper", "lower"))) %>%
    dplyr::distinct() %>%
    dplyr::mutate(param_id = row_number())


  run_map <- new_cfp_run_map(x = run_map,
                            id_cols = id_cols_runmap,
                            params = params,
                            method = method,
                            type = type,
                            n_runs = n_runs,
                            layers_different = layers_different,
                            runmap_type = "base",
                            params_df = params_df)
}


run_map_permutation <- function(x,
                                params,
                                n_runs,
                                layers_different,
                                layers_from,
                                topheight_adjust,
                                type_df,
                                second_depth){

      run_map <- expand.grid(params) %>%
        dplyr::mutate(run_id = dplyr::row_number()) %>%
        tidyr::pivot_longer(cols = !"run_id",
                            names_to = "param",
                            values_to = "value") %>%
        dplyr::left_join(type_df, by = "param")

      if (layers_different == TRUE) {

      params_notop <- params[!names(params) == "topheight"]

      run_map_raw <- expand.grid(params_notop) %>%
        dplyr::mutate(perm_id = dplyr::row_number())
      n_perms <- nrow(run_map_raw)


      if (layers_from == "layers_map"){

        run_map <-
          x$layers_map %>%
          dplyr::select(pmap,
                        dplyr::any_of({cfp_id_cols(x)})) %>%
          dplyr::group_by(dplyr::across(dplyr::any_of({cfp_id_cols(x)}))) %>%
          dplyr::group_modify(~{
            n_layers <- nrow(.x)
            expand.grid(lapply(1:n_layers, function(x) 1:n_perms)) %>%
              setNames(.x$pmap) %>%
              dplyr::mutate(run_id = row_number()) %>%
              tidyr::pivot_longer(!any_of("run_id"),
                                  names_to = "pmap",
                                  values_to = "perm_id")
          }) %>%
          dplyr::left_join(run_map_raw) %>%
          dplyr::select(!perm_id) %>%
          dplyr::mutate(pmap = as.numeric(pmap))

      } else if (layers_from == "soilphys"){
        run_map <-
          x$soilphys %>%
          dplyr::select(upper,
                        lower,
                        dplyr::any_of({cfp_id_cols(x)})) %>%
          dplyr::distinct()
      }


      if("topheight" %in% names(params)){

        # new permutation with topheight as well
        run_map_compl <-
          expand.grid(topheight = params$topheight,
                      run_id = unique(run_map$run_id)) %>%
          dplyr::mutate(run_id_new = dplyr::row_number())

        # run_map without topheight
        run_map_notop <-
          run_map_compl%>%
          dplyr::select(run_id, run_id_new) %>%
          dplyr::left_join(run_map, by = "run_id") %>%
          dplyr::select(!run_id) %>%
          tidyr::pivot_longer(cols = dplyr::any_of(names(params)),
                              names_to = "param",
                              values_to = "value")

        # run_map topheight only
        run_map_top  <-
          run_map %>%
          dplyr::select(dplyr::any_of(c(cfp_id_cols(x),"run_id"))) %>%
          dplyr::distinct() %>%
          dplyr::left_join(run_map_compl %>%
                             dplyr::select("topheight","run_id_new","run_id"),
                           by = "run_id") %>%
          dplyr::select(!run_id) %>%
          dplyr::rename(value = topheight) %>%
          dplyr::mutate(param = "topheight")


        #combine and finalize
        run_map <-
          run_map_notop %>%
          dplyr::bind_rows(run_map_top) %>%
          dplyr::rename(run_id = run_id_new)  %>%
          dplyr::left_join(type_df, by = "param")



      } else {

        run_map <-
          run_map %>%
          tidyr::pivot_longer(cols = dplyr::any_of(names(params)),
                              names_to = "param",
                              values_to = "value") %>%
          dplyr::left_join(type_df, by = "param")

      }

    }

    if (topheight_adjust == TRUE){
      # filter out runs with not possible topheight change

      merger <- cfp_id_cols(x$layers_map)[cfp_id_cols(x$layers_map) %in% names(run_map)]

      run_map <-
        run_map %>%
        dplyr::left_join(second_depth, by = merger) %>%
        dplyr::filter(param == "topheight" &
                        -value < top-bottom) %>%
        dplyr::select(dplyr::any_of(c("run_id", cfp_id_cols(x$layers_map)))) %>%
        dplyr::left_join(run_map,
                         by = c(merger, "run_id"),
                         relationship = "many-to-many")
    }

  run_map
}

run_map_random <- function(x,
                           params,
                           n_runs,
                           layers_different,
                           layers_from,
                           topheight_adjust,
                           type_df,
                           second_depth){


  stopifnot("For method = 'random' give exactly two values per param as limits" =
              all(sapply(params,length) == 2))

  params <- lapply(params, sort)

  params_notop <- params[!names(params) == "topheight"]

  run_map <- data.frame(run_id = rep(1:n_runs,
                                     each = length(params_notop)),
                        param = rep(names(params_notop), times = n_runs))

  if (layers_different == TRUE){
    if (layers_from == "layers_map"){
      run_map <-
        x$layers_map %>%
        dplyr::select(pmap,
                      dplyr::any_of({cfp_id_cols(x)}))%>%
        dplyr::cross_join(run_map)

    } else if (layers_from == "soilphys"){
      run_map <-
        x$soilphys %>%
        dplyr::select(dplyr::any_of(c(cfp_id_cols(x$layers_map),
                                      "upper",
                                      "lower"))) %>%
        dplyr::distinct() %>%
        dplyr::cross_join(run_map)

    }
  }


  if ("topheight" %in% names(params)){
    run_map <-
      run_map %>%
      dplyr::select(dplyr::any_of(c(cfp_id_cols(x),
                                    "run_id"))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(param = "topheight") %>%
      dplyr::bind_rows(run_map)
  }

  params_limits <-
  params %>%
    data.frame() %>%
    t() %>%
    data.frame() %>%
    setNames(c("param_min", "param_max")) %>%
    dplyr::mutate(param = rownames(.))

  run_map <-
    run_map %>%
    dplyr::left_join(params_limits, by = "param")

  if (topheight_adjust == TRUE){
    # only topheight_adjust limits within range of topmost layer!
    merger <- cfp_id_cols(x$layers_map)[cfp_id_cols(x$layers_map) %in% names(run_map)]

    run_map <-
      run_map %>%
      dplyr::left_join(second_depth, by = merger) %>%
      dplyr::mutate(param_min = ifelse(param == "topheight" &
                                         -param_min >= top-bottom,
                                       (bottom-top)+0.0001,
                                       param_min)) %>%
      dplyr::select(!dplyr::any_of(c("top", "bottom")))

  }

  run_map <-
    run_map %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = runif(1, param_min, param_max)) %>%
    dplyr::select(!dplyr::any_of(c("param_min", "param_max"))) %>%
    dplyr::left_join(type_df, by = "param") %>%
    dplyr::ungroup()



  run_map
}

