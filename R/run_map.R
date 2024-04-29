#'@title run_map
#'
#'@description Create a run_map for model alteration in \code{alternate()}.
#'
#'@param x Either a \link{cfp_pfres} or \link{cfp_fgres} model result.
#'
#'@param params A named list of numeric vectors. Names indicate column names in
#'  soilphys, vectors either distinct values (method permutation) or limits
#'  (method random).
#'
#'@param type A vector of length param indicating what the values in params
#'  represent. One of
#' \describe{
#' \item{abs}{Absolute values that are applied as-is.}
#' \item{factor}{Factors to be multiplied with the original values.}
#' \item{addition}{Factors to be added to the original values.}
#'}
#'
#'@param method Either 'random', where a random value is chosen within the
#'  bounds set in \code{params} or 'permutation', where every permutation of the
#'  values in \code{params} is added.
#'
#'@param n_runs Integer value of the number of alterations to be done for method
#'  = 'random'.
#'
#'@param layers_different Should layers from layers_map be changed individually?
#'  If \code{TRUE} this allows for different changes at different depths.
#'
#'@param layers_from (character) If layers_different is TRUE, from which source
#'  should the layers be created? One of:
#'\describe{
#'\item{layers_map}{(default) Use the layers that are defined in layers_map.}
#'\item{soilphys}{Use the layers as defined in soilphys}
#'\item{layers_altmap}{Use the layers as defined in the provided layers_altmap object.}
#'}
#'
#'@param layers_altmap An optional layers_map created using layers_map() that
#'  defines the layers to be used if layers_different = TRUE.
#'
#'@param topheight_adjust (logical) If the proposed change in topheight is
#'  larger than the highest layer in soilphys, should the limits be
#'  automatically adjusted per id_cols indivdually? Default is FALSE, which
#'  leads to an error in that case.


#' @export


# function to create the necessary run_map

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

  method <- match.arg(method, c("random", "permutation"))
  type <- match.arg(type, c("abs", "factor", "addition" ),several.ok = TRUE)

  stopifnot("type must be length 1 or the same as params" =
              (length(type) == 1) | (length(type) = length(params)))

  stopifnot("all params must be present in soilphys!" =
              all(names(params) %in% c(names(x$soilphys), "topheight")))

  stopifnot("please give an integer number of runs" =
              ((!(method == "random")) || (is.numeric(n_runs) && (abs(round(n_runs) - n_runs) < 1E-10))))

  layers_from = match.arg(layers_from, c("layers_map", "soilphys", "layers_altmap"))


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
                                   layers_altmap,
                                   topheight_adjust,
                                   type_df,
                                   second_depth)
  } else if (method == "random"){
      run_map <- run_map_random(x,
                                params,
                                n_runs,
                                layers_different,
                                layers_from,
                                layers_altmap,
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

  run_map <-
    run_map %>%
    dplyr::left_join(params_df, by = whats_in_both(list(names(params_df),
                                                        names(run_map))))

  if (layers_different && layers_from == "layers_altmap"){
    run_map <-
      x$soilphys %>%
      dplyr::select(!"pmap") %>%
      sp_add_pmap(layers_altmap) %>%
      dplyr::select(dplyr::any_of(c("pmap", "step_id", cfp_id_cols(layers_altmap)))) %>%
      dplyr::distinct() %>%
      dplyr::right_join(layers_altmap[, c("pmap", "upper", "lower", cfp_id_cols(layers_altmap))],
                        by = c("pmap" ,cfp_id_cols(layers_altmap))) %>%
      dplyr::right_join(run_map, by = c("upper", "lower", cfp_id_cols(layers_altmap))) %>%
      dplyr::select(!dplyr::any_of(c("pmap", "upper", "lower")))
  }


  run_map <- new_cfp_run_map(x = data.frame(run_map),
                            id_cols = id_cols_runmap,
                            params = params,
                            method = method,
                            type = type,
                            n_runs = n_runs,
                            layers_different = layers_different,
                            layers_from = layers_from,
                            layers_altmap = layers_altmap,
                            runmap_type = "base",
                            params_df = data.frame(params_df))
}


run_map_permutation <- function(x,
                                params,
                                n_runs,
                                layers_different,
                                layers_from,
                                layers_altmap,
                                topheight_adjust,
                                type_df,
                                second_depth){

   params_notop <- params[!names(params) == "topheight"]

   stopifnot("all limits in params be numeric for method = 'permutation' !!!" = all(sapply(params, is.numeric)))

   if (length(params_notop) > 0){

     run_map <- expand.grid(params_notop) %>%
       dplyr::mutate(perm_id = dplyr::row_number()) %>%
       tidyr::pivot_longer(cols = !"perm_id",
                           names_to = "param",
                           values_to = "value")
   } else {

     run_map <- data.frame(param = "topheight",
                           value = params$topheight,
                           run_id = seq_along(params$topheight))

   }

      if (layers_different == TRUE) {
        n_perms <- max(run_map$perm_id)

        if (layers_from == "layers_map"){
          layers_for_run_map <-
            x$layers_map %>%
            dplyr::select(pmap,
                          dplyr::any_of({cfp_id_cols(x)}))

        } else if (layers_from == "soilphys"){
          layers_for_run_map <-
            x$soilphys %>%
            dplyr::select(dplyr::any_of(c(cfp_id_cols(x$layers_map),
                                          "upper",
                                          "lower"))) %>%
            dplyr::distinct()
        } else if (layers_from == "layers_altmap"){
          layers_for_run_map <-
            layers_altmap %>%
            dplyr::select(dplyr::any_of(c(cfp_id_cols(layers_altmap),
                                          "upper",
                                          "lower"))) %>%
            dplyr::distinct()
        }

        run_map <-
          layers_for_run_map %>%
          dplyr::group_by(dplyr::across(dplyr::any_of(cfp_id_cols(x)))) %>%
          dplyr::mutate(layer_id = row_number()) %>%
          dplyr::group_modify(~{
            expand.grid(lapply(.x$layer_id, function(x) 1:n_perms)) %>%
              setNames(.x$layer_id) %>%
              dplyr::mutate(run_id = row_number()) %>%
              tidyr::pivot_longer(!any_of("run_id"),
                                  names_to = "layer_id",
                                  values_to = "perm_id") %>%
              dplyr::mutate(layer_id = as.numeric(layer_id)) %>%
              dplyr::left_join(.x)

          }) %>%
          dplyr::left_join(run_map, relationship = "many-to-many") %>%
          dplyr::select(!perm_id) %>%
          dplyr::select(!layer_id)
      }

      if (!"run_id" %in% names(run_map)){
        run_map <- run_map %>%
          dplyr::rename(run_id = perm_id)
      }

      if("topheight" %in% names(params) && length(params_notop) > 0){

        # new permutation with topheight as well
        run_map_compl <-
          expand.grid(topheight = params$topheight,
                      run_id = unique(run_map$run_id)) %>%
          dplyr::mutate(run_id_new = dplyr::row_number())

        # run_map without topheight
        run_map_notop <-
          run_map_compl %>%
          dplyr::select(run_id, run_id_new) %>%
          dplyr::left_join(run_map, by = "run_id") %>%
          dplyr::select(!run_id)

        # run_map topheight only
        run_map_top  <-
          run_map %>%
          dplyr::select(!param) %>%
          dplyr::select(!value) %>%
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
          dplyr::rename(run_id = run_id_new)

      }

    if ("topheight" %in% names(params) && topheight_adjust == TRUE){
      # filter out runs with not possible topheight change

      merger <- cfp_id_cols(x$layers_map)[cfp_id_cols(x$layers_map) %in% names(run_map)]

      run_map <-
        run_map %>%
        {function(x,y,merger){
          if (length(merger) == 0){
            dplyr::cross_join(x,y)
          } else {
            dplyr::left_join(x,y, by = merger)
          }
        }
          }(., second_depth, merger) %>%
        dplyr::filter(param == "topheight" &
                        -value < top-bottom) %>%
        dplyr::select(dplyr::any_of(c("run_id", cfp_id_cols(x$layers_map)))) %>%
        dplyr::left_join(run_map,
                         by = c(merger, "run_id"),
                         relationship = "many-to-many")
    }

      run_map <- run_map  %>%
        dplyr::left_join(type_df, by = "param")

  run_map
}

run_map_random <- function(x,
                           params,
                           n_runs,
                           layers_different,
                           layers_from,
                           layers_altmap,
                           topheight_adjust,
                           type_df,
                           second_depth){


  stopifnot("For method = 'random' give exactly two values per param as limits" =
              all(sapply(params,length) == 2))

  params <- lapply(params, function(x) {
    if(is.numeric(x[1])){
      x <- sort(x)
    }
    x
    })

  # get column names in params, if given as characters
  columns_for_limits <- unlist(params[sapply(params, is.character)], use.names = FALSE)

  stopifnot(  "if limits in params are privided as characters,
            these columns must be present in layers_altmap!"  =
                all(columns_for_limits %in% names(layers_altmap))
            )


  params_notop <- params[!names(params) == "topheight"]

  if (length(params_notop) > 0){
    run_map <- data.frame(run_id = rep(1:n_runs,
                                       each = length(params_notop)),
                          param = rep(names(params_notop), times = n_runs))
  } else {
    run_map <- data.frame(param = "topheight",
                          run_id = 1:n_runs)
  }

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

    } else if (layers_from == "layers_altmap"){
      run_map <-
        layers_altmap %>%
        dplyr::select(dplyr::any_of(c(cfp_id_cols(layers_altmap),
                                      columns_for_limits,
                                      "upper",
                                      "lower"))) %>%
        dplyr::distinct() %>%
        dplyr::cross_join(run_map)
    }
  }


  if ("topheight" %in% names(params) && length(params_notop) > 0){
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

  run_map <-
  run_map %>%
    rowwise() %>%
    mutate(param_min = as.numeric(get(ifelse(is.na(suppressWarnings(as.numeric(param_min))),param_min,"param_min"))),
           param_max = as.numeric(get(ifelse(is.na(suppressWarnings(as.numeric(param_max))),param_max,"param_max"))))

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

