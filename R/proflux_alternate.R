#' @title proflux_alternate
#'
#' @description Functions to extract datasets from the result of
#' a \code{\link{pro_flux}} model, a PFres object, to modify
#' parameters and rerun the model.
#'
#' @param PROFLUX An object of class PFres, the result
#' of a call to \code{\link{pro_flux}}
#'
#' @param params A vector of characters which parameters to alternate.
#' Can be the character of any column in \code{soilphys} or "topheight".
#' If "topheight" is called, the height of the soil/atmosphere interface will
#' be altered. This means, that the corresponding values in \code{gasdata},
#' \code{soilphys} and \code{layers_map} will be adjusted.
#'
#' The parameter(s) provided will be varied along the \code{facs} provided.
#' Then, \code{\link{complete_soilphys}} is run. This means, that it does not make
#' sense to alter, e.g. "AFPS" as it is re-calculated in that function call.
#'
#' @param facs A vector of factors each of params will be multiplied by.
#' This may be something like \code{seq(0.9,1.1,0.05)} if values should be varied between
#' 90 \% and 110 \% of their original value.
#'
#' @param params_map A \code{data.frame} containing any of \code{id_cols} of
#' PROFLUX, as well as \code{upper} and \code{lower}. For each layer enclosed by
#' "upper" and "lower", the parameters in "params" will be varied independently.
#' This means, that the more layer per combination of id_cols are provided, the
#' more variations will be tested out.
#'
#' @param no_confirm If the function is used in a script, set this to TRUE to confirm
#' the number of profiles without separate user input. Please ensure separately if
#' the function will evaluate in a reasonable amount of time.
#'
#' @export

proflux_alternate <- function(PROFLUX,
                              params,
                              facs,
                              params_map,
                              no_confirm = F,
                              DSD0_formula
                              ){

  # validity test of PROFLUX (is PFres?)

  # extract all necessary data from the PFres object
  id_cols <- PF_id_cols(PROFLUX)
  profiles <- PF_profiles(PROFLUX)

  # find number of profiles that will be calculated
  # confirm by user if many profiles - wrong input?
  ids_params_map <- id_cols[id_cols %in% names(params_map)]

  n_profs <-
    profiles %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(ids_params_map))) %>%
    dplyr::summarise(n_profs = dplyr::n())

  n_runs <-
    params_map %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::summarise(n_layers = dplyr::n()) %>%
    dplyr::left_join(n_profs) %>%
    dplyr::mutate(n_runs =
                    n_layers *
                    n_profs *
                    (length(facs)*
                    length(params))^
                    n_layers)

  total_profiles <- sum(n_runs$n_runs)
  message(paste0("The total number of single profiles is: ", total_profiles))

  if (total_profiles > 1e5){
    message("Wow, thats a lot of profiles! Are you sure? (~30s / 1000 profiles)")
    t_estim <- total_profiles/1000 * c(10,50) / 60
    t_estim <- paste0(t_estim[1]," to ",t_estim[2])
    message(paste0("time estimate: ",t_estim, "minutes"))

    if (!no_confirm){
      confirmer <- readline("please enter one of to continue / stop: (y/N)")
      if(!confirmer == "y"){
        stop("Evaluation stopped by user. If u wanted to continue, press 'y' next time.")
      }
    }
  }

  ## check if params are in soilphys and numeric
  p_in_sp <- params[params %in% names(PROFLUX)]
  p_numeric <- sapply(p_in_sp, function(col){
  ans <- PROFLUX %>% dplyr::pull(col) %>% is.numeric()
  })
  if (!length(params) == length(p_numeric)){
    stop("Not all parameters provided in params are columns in soilphysor are not numeric.")
  }

  #----- manipulate the data set -----#

  params_map <-
    params_map %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(gr_id = dplyr::cur_group_id()) %>%
    dplyr::mutate(layer = LETTERS[dplyr::row_number()])

  print(params_map)

  attr(PROFLUX,"tbl") <-
    PROFLUX %>%
    dplyr::mutate(depth = (upper+lower) / 2) %>%
    dplyr::select(!dplyr::any_of("layer")) %>%
    set_layer(layers_map = params_map,
              id_cols = id_cols) %>%
    as.data.frame()


  ## get variables for each run
  facs_map <-
    lapply(params, function(i) facs)
  names(facs_map) <- paste0("fac_",params)

  facs_map <- as.data.frame(facs_map) %>%
    expand.grid() %>%
    dplyr::mutate(perm_id = dplyr::row_number())

  n_perms <- nrow(facs_map)


  run_map <-
    params_map %>%
    dplyr::group_by(gr_id) %>%
    dplyr::group_modify(
      ~{
        l <- length(.x$layer)

        lapply(1:l,function(i)
          c(1:n_perms)
          ) %>%
          expand.grid() %>%
          tidyr::pivot_longer(cols = tidyr::starts_with("Var"),
                              names_to = "layer",
                              values_to = "perm_id",
                              names_prefix = "Var") %>%
          dplyr::mutate(run_id = rep(seq_len(n()/l),each = l))
      }
    ) %>%
    dplyr::mutate(layer = LETTERS[as.numeric(layer)]) %>%
    dplyr::left_join(facs_map) %>%
    dplyr::left_join(params_map) %>%
    dplyr::select(!dplyr::any_of(c("upper",
                          "lower",
                          "gr_id")))


  runs <- unique(run_map$run_id)

  lapply(runs,function(r_id){
    run <- run_map[run_map$run_id == r_id,]

    PROFLUX_new <-
      proflux_rerun(PROFLUX,
                    run,
                    params,
                    DSD0_formula = DSD0_formula)



    ##}



  })



}



##################################
##### HELPERS ####################
##################################

PFres2env <- function(PROFLUX,
                      env){

obj_names <- names(attributes(new_PFres()))
obj_names <- obj_names[- which(obj_names == "class")]

l <- sapply(obj_names,function(n){
  attr(PROFLUX,n)
},USE.NAMES = T)

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
              DSD0_formula){

  env <- environment()
  PFres2env(PROFLUX,
            env)


  cols_sp <- c(id_cols,
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
               "layer",
               "a",
               "b")

  soilphys <-
    PROFLUX %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(cols_sp))

  print(run)

  soilphys <-
    soilphys %>%
    dplyr::left_join(run) %>%
    dplyr::select(!layer) %>%
    dplyr::mutate(dplyr::across(params,~.x*{get(paste0("fac_",cur_column()))})) %>%
    complete_soilphys(DSD0_formula = DSD0_formula,
                      gases = unique(PROFLUX$gas),
                      overwrite = TRUE)


  ## function(param_variation)
  ##{
  ### initialise soilphys
  ### something for "topheight"
  ### recalculate soilphys
  ### calculate pro_flux()
  PROFLUX_new <-
    pro_flux(gasdata,
             soilphys,
             layers_map,
             id_cols,
             storage_term,
             zero_flux,
             zero_limits,
             known_flux,
             known_flux_factor,
             DSD0_optim,
             evenness_factor)
  ### optional:: calculate NRMSEs

}
