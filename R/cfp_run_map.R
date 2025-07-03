#'@title Create a run plan for parameter variation
#'
#'@description An S3 class cfp_run_map to be used in alternate(). Either create
#'  a new run map from a cfp_pfres or cfp_fgres model or extract an existing
#'  run_map from an cfp_altres object.
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
#'\item{layers_altmap}{Use the layers as defined in the provided layers_altmap
#'object.}
#'}
#'
#'@param layers_altmap An optional layers_map created using layers_map() that
#'  defines the layers to be used if layers_different = TRUE.
#'
#'@param topheight_adjust (logical) If the proposed change in topheight is
#'  larger than the highest layer in soilphys, should the limits be
#'  automatically adjusted per id_cols individually? Default is FALSE, which
#'  leads to an error in that case.
#'
#' @returns An object of type \code{cfp_run_map} that can be used within
#' \link{alternate}.
#'
#' @examples
#' PROFLUX <- ConFluxPro::base_dat |> pro_flux()
#'# Create a cfp_run_map where TPS is changed between 90 % and 110 %
#'# of the original value for 50 runs.
#' cfp_run_map(
#'   PROFLUX,
#'   list("TPS" = c(0.9, 1.1)),
#'   "factor",
#'   n_runs = 50)
#'
#'@importFrom rlang .data
#'
#'@export
cfp_run_map <- function(x,
                        params = list(),
                        type = NULL,
                        method = NULL,
                        n_runs = NULL,
                        layers_different = FALSE,
                        layers_from = "layers_map",
                        layers_altmap = NULL,
                        topheight_adjust = FALSE){
  UseMethod("cfp_run_map")
}

# extract only

#' @exportS3Method
cfp_run_map.cfp_altres <- function(x,
                                   params = NULL,
                                   type = NULL,
                                   method = NULL,
                                   n_runs = NULL,
                                   layers_different = NULL,
                                   layers_from = NULL,
                                   layers_altmap = NULL,
                                   topheight_adjust = NULL){
  out <- attr(x, "run_map")
  out
}

# helper ----
#' @exportS3Method
cfp_run_map.cfp_pfres <-
  function(x,
           params = list(),
           type = NULL,
           method = NULL,
           n_runs = NULL,
           layers_different = FALSE,
           layers_from = "layers_map",
           layers_altmap = NULL,
           topheight_adjust = FALSE){

    method <- match.arg(method, c("random", "permutation"))
    type <- match.arg(type, c("abs", "factor", "addition" ),several.ok = TRUE)

    stopifnot("type must be length 1 or the same as params" =
                (length(type) == 1) | (length(type) == length(params)))

    stopifnot("all params must be present in soilphys!" =
                all(names(params) %in% c(names(x$soilphys), "topheight")))

    stopifnot("please give an integer number of runs" =
                ((!(method == "random")) ||
                   (is.numeric(n_runs) &&
                      (abs(round(n_runs) - n_runs) < 1E-10))))

    layers_from <- match.arg(layers_from, c("layers_map",
                                            "soilphys",
                                            "layers_altmap"))


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
        dplyr::group_by(
          dplyr::across(dplyr::any_of(cfp_id_cols(x$layers_map)))) %>%
        dplyr::slice_max(upper, n = 2) %>%
        dplyr::summarise(top = max(upper),
                         bottom = min(upper))

      if (any(
        (second_depth$top-second_depth$bottom) <= -min(params$topheight))){
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
      dplyr::mutate(param_id = dplyr::row_number())

    run_map <-
      run_map %>%
      dplyr::left_join(params_df, by = whats_in_both(list(names(params_df),
                                                          names(run_map))))

    if (layers_different && layers_from == "layers_altmap"){
      run_map <-
        x$soilphys %>%
        dplyr::select(!"pmap") %>%
        sp_add_pmap(layers_altmap) %>%
        dplyr::select(
          dplyr::any_of(c("pmap",
                          "step_id", cfp_id_cols(layers_altmap)))) %>%
        dplyr::distinct() %>%
        dplyr::right_join(
          layers_altmap[, c("pmap", "upper", "lower",
                            cfp_id_cols(layers_altmap))],
          by = c("pmap" ,cfp_id_cols(layers_altmap)),
                          relationship = "many-to-many") %>%
        dplyr::right_join(
          run_map, by = c("upper", "lower", cfp_id_cols(layers_altmap)),
                          relationship = "many-to-many") %>%
        dplyr::select(!dplyr::any_of(c("pmap", "upper", "lower")))
    }

    # get correct new n_runs
    n_runs_new <- length(unique(run_map$run_id))

    run_map <- new_cfp_run_map(x = data.frame(run_map),
                               id_cols = id_cols_runmap,
                               params = params,
                               method = method,
                               type = type,
                               n_runs = n_runs_new,
                               layers_different = layers_different,
                               layers_from = layers_from,
                               layers_altmap = layers_altmap,
                               runmap_type = "base",
                               params_df = data.frame(params_df))

  run_map <- validate_cfp_run_map(run_map)
  run_map
}

#' @exportS3Method
cfp_run_map.cfp_fgres <- cfp_run_map.cfp_pfres


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

  stopifnot("all limits in params be numeric for method = 'permutation' !!!" =
              all(vapply(params, is.numeric, FUN.VALUE = logical(1))))

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
    if (length(params_notop) == 0){
      stop("layers_different does not make sense if only topheight is changed.")
    }

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
      dplyr::mutate(layer_id = dplyr::row_number()) %>%
      dplyr::group_modify(~{
        expand.grid(lapply(.x$layer_id, function(x) 1:n_perms)) %>%
          stats::setNames(.x$layer_id) %>%
          dplyr::mutate(run_id = dplyr::row_number()) %>%
          tidyr::pivot_longer(!any_of("run_id"),
                              names_to = "layer_id",
                              values_to = "perm_id") %>%
          dplyr::mutate(layer_id = as.numeric(layer_id)) %>%
          dplyr::left_join(.x, by = "layer_id")

      }) %>%
      dplyr::left_join(run_map,
                       relationship = "many-to-many", by = "perm_id") %>%
      dplyr::select(!"perm_id") %>%
      dplyr::select(!"layer_id")
  }

  if (!"run_id" %in% names(run_map)){
    run_map <- run_map %>%
      dplyr::rename(run_id = "perm_id")
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
      dplyr::select("run_id", "run_id_new") %>%
      dplyr::left_join(run_map, by = "run_id") %>%
      dplyr::select(!"run_id")

    # run_map topheight only
    run_map_top  <-
      run_map %>%
      dplyr::select(!"param") %>%
      dplyr::select(!"value") %>%
      dplyr::distinct() %>%
      dplyr::left_join(run_map_compl %>%
                         dplyr::select("topheight","run_id_new","run_id"),
                       by = "run_id") %>%
      dplyr::select(!"run_id") %>%
      dplyr::rename(value = "topheight") %>%
      dplyr::mutate(param = "topheight")

    #combine and finalize
    run_map <-
      run_map_notop %>%
      dplyr::bind_rows(run_map_top) %>%
      dplyr::rename(run_id = "run_id_new")

  }

  if ("topheight" %in% names(params) && topheight_adjust == TRUE){
    # filter out runs with not possible topheight change

    merger <- cfp_id_cols(x$layers_map)[cfp_id_cols(x$layers_map) %in%
                                          names(run_map)]

    run_map <-
      run_map %>%
      {function(x,y,merger){
        if (length(merger) == 0){
          dplyr::cross_join(x,y)
        } else {
          dplyr::left_join(x,y, by = merger)
        }
      }
      }(second_depth, merger) %>%
      dplyr::filter(.data$param == "topheight" &
                      -.data$value < .data$top-.data$bottom) %>%
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


  stopifnot("For method = 'random' give exactly two values per
            param as limits" =
              all(vapply(params, length, FUN.VALUE = integer(1)) == 2))

  params <- lapply(params, function(x) {
    if(is.numeric(x[1])){
      x <- sort(x)
    }
    x
  })

  # get column names in params, if given as characters
  columns_for_limits <- unlist(params[vapply(params,
                                             is.character,
                                             FUN.VALUE = logical(1))],
                               use.names = FALSE)

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
    if (length(params_notop) == 0){
      stop("layers_different does not make sense if only topheight is changed.")
    }

    if (layers_from == "layers_map"){
      run_map <-
        x$layers_map %>%
        dplyr::select("pmap",
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
    stats::setNames(c("param_min", "param_max")) %>%
    tibble::rownames_to_column("param")

  run_map <-
    run_map %>%
    dplyr::left_join(params_limits, by = "param")

  run_map <-
    run_map %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      param_min = as.numeric(
        get(ifelse(is.na(
          suppressWarnings(as.numeric(param_min))),param_min,"param_min"))),
      param_max = as.numeric(
        get(ifelse(is.na(
          suppressWarnings(as.numeric(param_max))),param_max,"param_max"))))

  if (topheight_adjust == TRUE){
    # only topheight_adjust limits within range of topmost layer!
    merger <- cfp_id_cols(x$layers_map)[cfp_id_cols(x$layers_map) %in%
                                          names(run_map)]

    run_map <-
      run_map %>%
      dplyr::left_join(second_depth, by = merger) %>%
      dplyr::mutate(
        param_min = ifelse(.data$param == "topheight" &
                             -.data$param_min >= .data$top-.data$bottom,
                           (.data$bottom-.data$top)+0.0001,
                           .data$param_min)) %>%
      dplyr::select(!dplyr::any_of(c("top", "bottom")))

  }

  run_map <-
    run_map %>%
    dplyr::rowwise() %>%
    dplyr::mutate(value = stats::runif(1, .data$param_min, .data$param_max)) %>%
    dplyr::select(!dplyr::any_of(c("param_min", "param_max"))) %>%
    dplyr::left_join(type_df, by = "param") %>%
    dplyr::ungroup()



  run_map
}


# creator -------------

new_cfp_run_map <- function(x,
                            id_cols,
                            params,
                            method,
                            type,
                            n_runs,
                            layers_different,
                            layers_from,
                            layers_altmap,
                            runmap_type,
                            params_df
){

  stopifnot(is.data.frame(x))

  x <- structure(x,
                 class = c("cfp_run_map",
                           class(x)[!grepl("cfp_run_map",class(x))]),
                 params = params,
                 method = method,
                 type = type,
                 n_runs = n_runs,
                 layers_different = layers_different,
                 layers_from = layers_from,
                 layers_altmap = layers_altmap,
                 runmap_type = runmap_type,
                 params_df = params_df)
 x
}


# validator -----------

validate_cfp_run_map <- function(x){

  stopifnot(inherits(x, "cfp_run_map"))
  stopifnot(is.data.frame(x))

  stopifnot("run_id" %in% names(x))
  x
}



###############
### HELPERS ### -----------------
###############

# extractors ---------
#' @rdname extractors
#' @export
cfp_runmap_type <- function(x){
  UseMethod("cfp_runmap_type")
}

#' @exportS3Method
cfp_runmap_type.cfp_run_map <- function(x){
  attr(x, "runmap_type")
}


#' @rdname extractors
#' @export
cfp_params_df <- function(x){
  UseMethod("cfp_params_df")
}

#' @exportS3Method
cfp_params_df.cfp_run_map <- function(x){
  attr(x, "params_df")
}

#' @exportS3Method
cfp_params_df.cfp_altres <- function(x){
  x <- cfp_run_map(x)
  .Class <- class(x)
  NextMethod()
}



#' @rdname extractors
#' @export
cfp_n_runs <- function(x){
  UseMethod("cfp_n_runs")
}

#' @exportS3Method
cfp_n_runs.cfp_run_map <- function(x){
  attr(x, "n_runs")
}

#' @rdname extractors
#' @export
cfp_layers_different <- function(x){
  UseMethod("cfp_layers_different")
}

#' @exportS3Method
cfp_layers_different.cfp_run_map <- function(x){
  attr(x, "layers_different")
}

#' @rdname extractors
#' @export
cfp_layers_from <- function(x){
  UseMethod("cfp_layers_from")
}

#' @exportS3Method
cfp_layers_from.cfp_run_map <- function(x){
  attr(x, "layers_from")
}
#' @rdname extractors
#' @export
cfp_layers_altmap <- function(x){
  UseMethod("cfp_layers_altmap")
}

#' @exportS3Method
cfp_layers_altmap.cfp_run_map <- function(x){
  attr(x, "layers_altmap")
}




###### PRINTING #######
#' @exportS3Method
print.cfp_run_map <- function(x, ...){
  n <- cfp_n_runs(x)

  cat("\nA cfp_run_map to be used in alternate(). \n")
  cat("number of runs: ", n,"\n")
  cat("parameters to alternate:\n")
  print(cfp_params_df(x))
  NextMethod()
}






