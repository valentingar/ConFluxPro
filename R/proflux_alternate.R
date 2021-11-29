#' @title proflux_alternate
#'
#' @description Functions to extract datasets from the result of a
#'   \code{\link{pro_flux}} model, a PFres object, to modify parameters and
#'   rerun the model.
#'
#' @param PROFLUX An object of class PFres, the result of a call to
#'   \code{\link{pro_flux}}
#'
#' @param params A vector of characters which parameters to alternate. Can be
#'   the character of any column in \code{soilphys} or "topheight". If
#'   "topheight" is called, the height of the soil/atmosphere interface will be
#'   altered. This means, that the corresponding values in \code{gasdata},
#'   \code{soilphys} and \code{layers_map} will be adjusted.
#'
#'   The parameter(s) provided will be varied along the \code{facs} provided.
#'   Then, \code{\link{complete_soilphys}} is run. This means, that it does not
#'   make sense to alter, e.g. "AFPS" as it is re-calculated in that function
#'   call.
#'
#' @param facs A vector of factors each of params will be multiplied by. This
#'   may be something like \code{seq(0.9,1.1,0.05)} if values should be varied
#'   between 90 \% and 110 \% of their original value.
#'
#' @param params_map A \code{data.frame} containing any of \code{id_cols} of
#'   PROFLUX, as well as \code{upper} and \code{lower}. For each layer enclosed
#'   by "upper" and "lower", the parameters in "params" will be varied
#'   independently. This means, that the more layer per combination of id_cols
#'   are provided, the more variations will be tested out.
#'
#' @param topheight_var Either FALSE (default) or a vector of numerics that define
#'   how much the topheight should be varied in cm. Usually this corresponds to
#'   the humus layer of a soil. The values are added to the original topheight.
#'   Negative values are subtracted. If this results in a layer of height 0
#'   (or negative height), it is removed from the model and a warning message displayed.
#'
#' @param sensitivity_analysis How should the different values of \code{params}
#'   and \code{facs} be combined? Can be one of:
#'   \describe{
#'   \item{FALSE}{(default). Here, all possible combinations of \code{params},
#'                \code{facs} and the respective layers of \code{params_map}
#'                are run.}
#'   \item{OAT}{One At a Time. This means, that only one of \code{params} is
#'              changed at a time for all layers, all the others will have
#'              a \code{facs} of 1.}
#'   \item{OATL}{One At a Time Layer. Here, \code{OAT} is expanded to change
#'               only one parameter \emph{and} one layer at a time.}
#'   }
#'
#' @param no_confirm If the function is used in a script, set this to TRUE to
#'   confirm the number of profiles without separate user input. Please ensure
#'   separately if the function will evaluate in a reasonable amount of time.
#'
#' @param return_raw If TRUE, the complete model results are returned as a list
#'   instead of the error summary.
#'
#' @param write_db If \code{TRUE}, the results (either the error parameters from error_funs
#'  or the raw PFres results) are written in table in the database defined in con.
#'  Runs are run together at 25 runs each to improve performance while preventing memory crashes.
#'
#' @param con The database connection to be used when write_db is \code{TRUE}.
#'
#' @param error_funs A named list of functions that take PROFLUX (+ further arguments)
#'   as input and return a \code{data.frame} of at least one column: NRMSE, as
#'   well as further columns that may represent different parameters calculated
#'   in parallel (e.g. NRMSE for different gases or id_cols).
#'
#' @param error_args A list of one named list of function arguments per function
#'  provided in error_funs. Note, that the arguments must be named. The names of the
#'  wrapping list must follow the names and order of \code{error_funs}. Additional
#'  data needed in these functions (e.g. reference \code{EFFLUX} values) must be
#'  provided here.
#'
#' @inheritParams complete_soilphys
#'
#'
#' @export

proflux_alternate <- function(PROFLUX,
                              params,
                              facs,
                              params_map,
                              topheight_var = FALSE,
                              sensitivity_analysis = FALSE,
                              no_confirm = FALSE,
                              DSD0_formula,
                              return_raw = FALSE,
                              write_db = FALSE,
                              con = NULL,
                              error_funs,
                              error_args) {

  # validity test of PROFLUX (is PFres?)

  # check sensitvity_analysis
  if (!(sensitivity_analysis %in% c("OAT", "OATL") |
    sensitivity_analysis == FALSE)) {
    stop("sensitivity_analysis must be on of c('OAT','OATL') or FALSE.")
  }

  # check error_args - is there PROFLUX ANYWHERE
  arg_names <-
    sapply(error_args, names) %>% unlist()

  if ("PROFLUX" %in% arg_names) {
    stop("error_args cannot contain PROFLUX!")
  }

  # check error_funs - must be functions!
  if (!all(sapply(error_funs, is.function))) {
    stop("error_funs must be functions only.")
  }

  # extract all necessary data from the PFres object
  id_cols <- PF_id_cols(PROFLUX)
  profiles <- PF_profiles(PROFLUX)
  layers_map <- PF_layers_map(PROFLUX)

  ## check if params are in soilphys and numeric
  p_in_sp <- params[params %in% names(PROFLUX)]
  p_numeric <- sapply(p_in_sp, function(col) {
    ans <- PROFLUX %>%
      dplyr::pull(col) %>%
      is.numeric()
  })

  if (!length(params) == length(p_numeric)) {
    stop("Not all parameters provided in params are columns in soilphysor are not numeric.")
  }


  # check if topheight_var leads to 0 or negative layers
  if (!topheight_var[1] == FALSE){
    is_topheight_invalid <-
      layers_map %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::slice_max(upper) %>%
      dplyr::mutate(upper = upper + min(!!topheight_var)) %>%
      dplyr::mutate(is_invalid = upper <= lower) %>%
      dplyr::pull(is_invalid)

    if (any(is_topheight_invalid)){
      warning("topheight_var leads to layers of height 0 or less.
              There, the top layer is removed.
              This may lead to duplicate runs.")
    }
  }

  #----- manipulate the data set -----#

  # params map should have a layer_alt variable to join with soilphys
  # this is also added to PROFLUX, from which soilphys is taken.
  params_map <-
    params_map %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(gr_id = dplyr::cur_group_id()) %>%
    dplyr::mutate(layer = LETTERS[dplyr::row_number()])

  pf_layer_alt <-
    PROFLUX %>%
    dplyr::mutate(row_id_lmap = dplyr::row_number()) %>%
    dplyr::mutate(depth = (upper + lower) / 2) %>%
    dplyr::select(!dplyr::any_of("layer")) %>%
    set_layer(
      layers_map = params_map,
      id_cols = id_cols
    ) %>%
      select(layer,row_id_lmap)

   PROFLUX$layer_alt <-
      pf_layer_alt$layer[match(1:nrow(PROFLUX),pf_layer_alt$row_id_lmap)]


  params_map <-
    params_map %>%
    dplyr::rename(layer_alt = layer)



  # get permutations in dependence of sensitivity analysis type
  if (sensitivity_analysis == FALSE) {

    # 'fac_' will be the identifier for recalculation later
    facs_map <-
      lapply(params, function(i) facs)

    names(facs_map) <- paste0("fac_", params)

    facs_map <- facs_map %>%
      as.data.frame()

    facs_map <- facs_map %>%
      expand.grid()

  } else {
    # for sensitivity analysis, only one parameter is changed
    # at a time, so that not all permuations are needed.

    # Because 1 is added later anyways
    facs <- facs[!facs == 1]

    facs_map <-
      lapply(params, function(i) rep(1, length(facs) + 1))

    names(facs_map) <- paste0("fac_", params)
    facs_map <- as.data.frame(facs_map)

    facs_map <-
      lapply(1:ncol(facs_map), function(i) {
        df <- facs_map
        df[-1, i] <- facs
        df
      }) %>%
      dplyr::bind_rows()

  }

  # this makes it easier to assign different permutations to different layers.
  facs_map <- facs_map %>%
    dplyr::mutate(perm_id = dplyr::row_number())

  n_perms <- nrow(facs_map)

  if (sensitivity_analysis == FALSE) {
    run_map <- make_map_allvar(
      params_map,
      facs_map,
      n_perms
    )
  } else if (sensitivity_analysis == "OAT") {
    run_map <- make_map_oat(
      params_map,
      facs_map
    )
  } else if (sensitivity_analysis == "OATL") {
    run_map <- make_map_oatl(
      params_map,
      facs_map
    )
  }

  #initialise value
  run_map$fac_topheight <- 0

  if (!topheight_var[1] == FALSE){

    if (sensitivity_analysis == FALSE){

      n_toph <- length(topheight_var)

      run_map <-
        run_map %>%
        dplyr::select(run_id,gr_id) %>%
        dplyr::distinct() %>%
        dplyr::rowwise() %>%
        dplyr::summarise(dplyr::across(dplyr::everything(),~rep(.x,n_toph)),
                         fac_topheight = !!topheight_var,
                         run_modifier = seq(1:n_toph)) %>%
        dplyr::group_by(run_id,run_modifier) %>%
        dplyr::mutate(run_id = dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::select(!run_modifier) %>%
        dplyr::left_join(run_map %>%
                           dplyr::select(
                             !dplyr::any_of(c("run_id",
                                              "fac_topheight"))
                             )
        )

    } else if(sensitivity_analysis %in% c("OAT","OATL")){

      # is added anyway as default
      topheight_var <- topheight_var[!topheight_var == 0]
      n_toph <- length(topheight_var)

      run_map <- run_map %>%
        dplyr::select(
          !dplyr::any_of(c("fac_topheight"))
        )

      run_ones <-
      run_map %>%
        dplyr::filter(run_id == 1) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("fac_"),
                                    ~1)) %>%
        dplyr::select(!run_id)

      run_topheight <-
      data.frame(fac_topheight = topheight_var,
                 run_id = LETTERS[1:n_toph]) %>%
        left_join(run_ones,by = character())

      run_map <-
      run_map %>%
        dplyr::mutate(fac_topheight = 0) %>%
        dplyr::mutate(run_id = as.character(run_id)) %>%
        dplyr::bind_rows(run_topheight) %>%
        dplyr::group_by(run_id) %>%
        dplyr::mutate(run_id = cur_group_id())

    }

  }

  #repair fac_topheight
  run_map <- run_map %>%
    dplyr::mutate(fac_topheight = ifelse(is.na(fac_topheight),
                                         0,
                                         fac_topheight))

  runs <- unique(run_map$run_id)


  run_map <- run_map %>%
    dplyr::select(!dplyr::any_of(c(
      "upper",
      "lower",
      "gr_id"
    )))

  # find number of profiles that will be calculated
  # confirm by user if many profiles - wrong input?
  total_profiles <- run_map %>%
    dplyr::left_join(profiles) %>%
    nrow()

  message(paste0("The total number of single profiles is: ", total_profiles))

  if (total_profiles > 1e5 & sensitivity_analysis == FALSE) {
    message("Wow, thats a lot of profiles! Are you sure? (~30s / 1000 profiles)")
    t_estim <- total_profiles / 1000 * c(10, 50) / 60
    t_estim <- paste0(t_estim[1], " to ", t_estim[2])
    message(paste0("time estimate: ", t_estim, "minutes"))

    if (!no_confirm) {
      confirmer <- readline("please enter one of to continue / stop: (y/N)")
      if (!confirmer == "y") {
        stop("Evaluation stopped by user. If you wanted to continue, press 'y' next time.")
      }
    }
  }




  #------ calculate ---------#

  #initialise function for actual running.
  fun_run <- function(run_id) {
    run_and_summarise(r_id = run_id,
                      run_map = run_map,
                      PROFLUX = PROFLUX,
                      params = params,
                      DSD0_formula = DSD0_formula,
                      return_raw = return_raw,
                      error_funs = error_funs,
                      error_args = error_args)
    }


  if (write_db == TRUE){

    #####################
    ### WITH DATABASE ###
    #####################

    tab_name <- ifelse(return_raw == TRUE,
                       "PFres",
                       "PFalt")

    # have there been previous attempts?
    # increase alt_id by one if yes
    if (DBI::dbExistsTable(con,"alt_map")){
      alt_id <-
        con %>% dplyr::tbl("alt_map") %>%
        dplyr::collect() %>%
        dplyr::pull(alt_id) %>%
        max()

      alt_id <- alt_id + 1

      if(!is.finite(alt_id)){
        warning("corrupted alt_map in con. starting at 100 now")
        alt_id <- 100
      }

    } else {
      alt_id <- 1

      DBI::dbCreateTable(con,
                         "alt_map",
                         data.frame(alt_id = alt_id,
                                    Date = Sys.time())
                         )

    }

    DBI::dbAppendTable(con,
                       "alt_map",
                       data.frame(alt_id = alt_id,
                                  Date = format(Sys.time()))
    )

    # initialise database tables and choose correct functions
    tbl_temp <- fun_run(1)
    tbl_temp$alt_id <- alt_id
    tbl_temp <- tbl_temp[0, ]

    if (return_raw == TRUE){
      tab_name <- paste0("PFres_",alt_id)
    } else {
      tab_name <- paste0("PFalt_",alt_id)

    }

    DBI::dbCreateTable(con,
                       tab_name,
                       tbl_temp)


    fun_proc <- function(df_ret){
      write_database(df_ret,
                     con,
                     alt_id,
                     tab_name)
    }


    # run alternation
    chunk_lapply(X = runs,
                 FUN = fun_run,
                 fun_process = fun_proc,
                 n_per_chunk = 25)

    DBI::dbCreateTable(con,
                       paste0("runmap_",alt_id),
                       run_map)
    DBI::dbAppendTable(con,
                       paste0("runmap_",alt_id),
                       run_map)


    DBI::dbDisconnect(con)

  } else {

  ###################
  ### NO DATABASE ###
  ###################


  df_ret <-
    lapply(runs,
           fun_run
           )

  # user can chose to export not the
  # error summary but the raw model results instead
  if (return_raw == TRUE) {
    return(df_ret)
  }


  df_ret <-
    df_ret %>%
    dplyr::bind_rows()

  l <- list(
    results = df_ret,
    run_map = run_map
  )

  return(l)
  }
}



##################################
##### HELPERS ####################
##################################

PFres2env <- function(PROFLUX,
                      env) {
  obj_names <- names(attributes(new_PFres()))
  obj_names <- obj_names[-which(obj_names == "class")]

  l <- sapply(obj_names, function(n) {
    attr(PROFLUX, n)
  }, USE.NAMES = T)

  list2env(l, environment())

  gasdata <-
    gasdata %>%
    left_join(profiles)

  l_env <- as.list.environment(environment())
  list2env(l_env, env)
}


proflux_rerun <- function(PROFLUX,
                          run,
                          params,
                          DSD0_formula) {
  env <- environment()
  PFres2env(
    PROFLUX,
    env
  )


  cols_sp <- c(
    id_cols,
    "DS",
    "D0",
    "TPS",
    "AFPS",
    "SWC",
    "upper",
    "lower",
    "depth",
    "Temp",
    "p",
    "rho_air",
    "layer_alt",
    "a",
    "b",
    "c"
  )

  soilphys <-
    PROFLUX %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(cols_sp))

  topheight_change <- run$fac_topheight[1]

  run <- run %>%
    dplyr::select(!fac_topheight)

  soilphys <-
    soilphys %>%
    dplyr::left_join(run) %>%
    #dplyr::select(!layer_alt) %>%
    dplyr::mutate(dplyr::across(params, ~ .x * {
      get(paste0("fac_", cur_column()))
    })) %>%
    complete_soilphys(
      DSD0_formula = DSD0_formula,
      gases = unique(PROFLUX$gas),
      overwrite = TRUE
    )

  if (!topheight_change == 0){

    layers_map <-
      layers_map %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(upper = ifelse(upper == max(upper),
                                   upper + topheight_change,
                                   upper)) %>%

      # if this leads to invalid (or empty) layers, these are
      # removed.
      dplyr::filter(lower < upper)

    soilphys <-
      soilphys %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(upper = ifelse(upper == max(upper),
                                   upper + topheight_change,
                                   upper)) %>%

      # if this leads to invalid (or empty) layers, these are
      # removed.
      dplyr::filter(lower < upper)

    gasdata <-
      gasdata %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(depth = ifelse(depth == max(depth),
                                   depth + topheight_change,
                                   depth))

    # get highest value in layers_map per group and remove
    # any entries in gasdata that are higher than that
    # after correction (same reason as for the other datasets)
    maxtop <-
      layers_map %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::summarise(max_upper = max(upper))

    gasdata <-
      gasdata %>%
      dplyr::left_join(maxtop) %>%
      filter(depth <= max_upper) %>%
      select(!max_upper)


  }

  PROFLUX_new <-
    pro_flux(
      gasdata,
      soilphys,
      layers_map,
      id_cols,
      storage_term,
      zero_flux,
      zero_limits,
      known_flux,
      known_flux_factor,
      DSD0_optim,
      evenness_factor
    )
  ### optional:: calculate NRMSEs
}


run_and_summarise <- function(r_id,
                        run_map,
                        PROFLUX,
                        params,
                        DSD0_formula,
                        return_raw,
                        error_funs,
                        error_args){
  run <- run_map[run_map$run_id == r_id, ]

  PROFLUX_new <-
    proflux_rerun(
      PROFLUX,
      run,
      params,
      DSD0_formula = DSD0_formula
    )

  # user can chose to export not the
  # error summary but the raw model results instead
  if (return_raw == TRUE) {

    PROFLUX_new$run_id <- r_id
    return(PROFLUX_new)
  }

  # otherwise: error parameter are calculated
  # and returned instead.
  df_ret <-
    apply_error_funs(PROFLUX_new,
                     error_funs,
                     error_args)

  df_ret$run_id <- r_id
  df_ret
}

#### run map design ####
########################

make_map_allvar <- function(params_map,
                            facs_map,
                            n_perms) {
  run_map <-
    params_map %>%
    dplyr::group_by(gr_id) %>%
    dplyr::group_modify(
      ~ {
        l <- length(.x$layer_alt)

        df <-
          gtools::combinations(n_perms,l,repeats.allowed = TRUE) %>%
          t()

        df <-
        as.data.frame(df) %>%
          dplyr::mutate(layer_alt = .x$layer_alt) %>%
          tidyr::pivot_longer(
            cols = starts_with("V"),
            names_to = "run_id",
            values_to = "perm_id",
            names_prefix = "V") %>%
          dplyr::mutate(run_id = as.numeric(run_id))

      }
    ) %>%
    dplyr::left_join(facs_map) %>%
    dplyr::left_join(params_map)
}


make_map_oat <- function(params_map,
                         facs_map) {
  run_map <-
    facs_map %>%
    dplyr::mutate(run_id = dplyr::row_number()) %>%
    dplyr::full_join(params_map,
      by = character()
    )
}


make_map_oatl <- function(params_map,
                          facs_map) {
  run_map <-
    params_map %>%
    group_by(gr_id) %>%
    group_modify(~ {
      l <- nrow(.x)

      facs_map <-
        facs_map %>%
        dplyr::filter(!dplyr::if_all(dplyr::starts_with("fac"), ~ .x == TRUE))

      df_l <- lapply(1:l, function(i) {
        df <- facs_map
        df[T] <- 1
        df$perm_id <- facs_map$perm_id
        df
      })

      df_ret <-
        lapply(1:l, function(i) {
          df_l[[i]] <- facs_map

          df_ret <- dplyr::bind_rows(df_l)
          df_ret$l_id <- i
          df_ret$layer_alt <- rep(.x$layer_alt,
            each = nrow(facs_map)
          )

          df_ret
        }) %>%
        dplyr::bind_rows()

      df_ret <-
        df_ret %>%
        dplyr::group_by(
          l_id,
          perm_id
        ) %>%
        dplyr::mutate(run_id = dplyr::cur_group_id()) %>%
        dplyr::ungroup() %>%
        dplyr::select(!dplyr::any_of(c("perm_id", "l_id")))

      df_one <- df_ret %>%
        dplyr::filter(run_id == 1) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("fac"), ~1)) %>%
        dplyr::mutate(run_id = 0)

      df_ret <- bind_rows(df_one, df_ret) %>%
        dplyr::mutate(run_id = run_id + 1)


      df_ret
    }) %>%
    dplyr::left_join(params_map)
}

apply_error_funs <- function(PROFLUX_new,
                             error_funs,
                             error_args){


  df_ret <-
  lapply(1:length(error_funs), function(f_id) {
    error_args_tmp <-
      error_args[[f_id]]
    error_args_tmp$PROFLUX <- PROFLUX_new

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

# functions to write the results  to database connection con
write_database <- function(df_ret,
                               con,
                               alt_id,
                           tab_name){

  df_ret <-
    df_ret %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(alt_id = !!alt_id)

  DBI::dbAppendTable(conn = con,
                     name = tab_name,
                     value = df_ret)

}


