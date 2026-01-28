#'@title Inverse model of production profiles
#'
#'@description This implements an inverse modeling approach which optimizes
#'  vertically resolved production (or consumption) of the gases in question to
#'  fit a modeled concentration profile to observed data.
#'
#'  One boundary condition of this model is, that there is no incoming or
#'  outgoing flux at the bottom of the lowest layer of the profile. If this
#'  boundary condition is not met, the flux must be optimised as well. This can
#'  be set in \code{zero_flux}.
#'
#'@param x A `cfp_dat` object with all the necessary input datasets.
#'
#'@inheritDotParams cfp_pfmod zero_flux zero_limits DSD0_optim evenness_factor
#'  known_flux_factor
#'
#' @returns A [cfp_pfres()] model result.
#'
#' @examples
#'
#'soilphys <-
#'  cfp_soilphys(
#'    ConFluxPro::soilphys,
#'    id_cols = c("site", "Date")
#'  )
#'
#'gasdata <-
#'  cfp_gasdata(
#'    ConFluxPro::gasdata,
#'    id_cols = c("site", "Date")
#'  )
#'
#'
#'lmap <-
#'  cfp_layers_map(
#'    ConFluxPro::layers_map,
#'    gas = "CO2",
#'    lowlim = 0,
#'    highlim = 1000,
#'    id_cols = "site"
#'  )
#'
#'PROFLUX <-
#'  cfp_dat(gasdata,
#'          soilphys,
#'          lmap ) |>
#'  pro_flux()
#'
#'
#'@family flux models
#'
#'
#'@export

pro_flux <- function(x,
                     ...){
  # for future expansion, remove if implemented
  named_dots <- names(list(...))
  stopifnot(
    "'...' contains unused arguments or that are not yet implemented fully" =
      all(named_dots %in%
            c("zero_flux", "zero_limits", "evenness_factor", "fit_to")))

UseMethod("pro_flux")
}

#' @rdname pro_flux
#'@exportS3Method
pro_flux.cfp_dat <- function(x,
                             ...){
  #rlang::check_dots_empty()

  x <- cfp_pfmod(x,...)
  .Class <- "cfp_pfmod"
  NextMethod()
}

#' @rdname pro_flux
#'@exportS3Method
pro_flux.cfp_pfres <- function(x,
                               ...){
  x <- as_cfp_pfmod(x)
  NextMethod()
}

#' @rdname pro_flux
#'@exportS3Method
pro_flux.cfp_pfmod <- function(x,
                                ...){

  stopifnot(inherits(x,"cfp_pfmod"))

  #apply function to all grouped cfp_pfmods #24x
  p <- progressr::progressor(steps = nrow(x$profiles)/53)

  y <- pro_flux_by_profiles(x, p)

  #combine PROFLUX result
  y <- dplyr::bind_rows(y)

  # add some columns
  y <- x$soilphys %>%
    as.data.frame() %>%
    dplyr::left_join(x$profiles,
                     by = c(names(x$soilphys)[names(x$soilphys) %in%
                                                names(x$profiles)]),
                     relationship = "many-to-many") %>%
    dplyr::select(upper, lower, step_id, prof_id, sp_id, pmap) %>%
    dplyr::right_join(y, by = c("step_id", "prof_id")) %>%
    cfp_layered_profile(id_cols ="prof_id")

  #create cfp_pfres object
  y <- cfp_pfres(x,y)
  y
}


#################################################
### ------------- HELPERS -----------------------
#################################################

##########################################
## Function to perform preparation for each
## group and then run prof_optim on all.
pro_flux_by_profiles <-  function(
    x,
    p){

  # initialize parameters for each group
  # and general parameters
  parameters <- init_proflux_parameters(x)

  # split profiles to loop over
  profs_split <- split(
    data.frame(
      x$profiles[,names(x$profiles) %in%
                   c("gd_id", "sp_id", "group_id")]),
    x$profiles$prof_id)

  # split soilphys and gasdata per ids and
  # store in environment for copyless retrieval
  x <- split_by_prof_env(x)


  # calculate production rates per profile
  df_ret <-furrr::future_imap(
    profs_split,
    env = x,
    parameters = parameters,
    p = p,
    prof_optim
  )

  df_ret <- df_ret %>%
    dplyr::bind_rows()

  return(df_ret)
}


#########################################-
### Function for per profile optimisation
prof_optim <- function(
    current_profile,
    prof_id,
    env,
    parameters,
    p){
  gasdata <- get(as.character(current_profile$gd_id),envir = env$gasdata)
  soilphys <- get(as.character(current_profile$sp_id),envir = env$soilphys)

  group_parameters <- get(as.character(current_profile$group_id), parameters$group_parameters)

  #mapping productions to soilphys_tmp
  pmap <- soilphys$pmap

  #calculating height of each step in m
  height <- soilphys$height

  #mapping measured concentrations to soilphys_tmp
  cmap <- soilphys$step_id[match(gasdata$depth,
                                 soilphys$upper)]

  x_ppm <- gasdata$x_ppm
  conc <- x_ppm * soilphys$c_air[cmap]

  #shortening to valid cmaps
  x_ppm <- x_ppm[is.finite(cmap)]
  conc <- conc[is.finite(cmap)]
  cmap <- cmap[is.finite(cmap)]

  profile_ref <- switch(
    parameters$fit_to,
    "concentration" = conc,
    "molar_fraction" = x_ppm)

  #weigh the observations based on the degrees of freedom
  deg_free_obs <- pmap[cmap]
  n_obs_deg_free <- tabulate(deg_free_obs,max(pmap))
  deg_free_ids <- sort(as.numeric(unique(pmap)))
  weights <- deg_free_ids^2/n_obs_deg_free
  wmap <- weights[deg_free_obs]

  #C0 at lower end of production model
  x0 <- stats::median(
    gasdata$x_ppm[gasdata$depth == group_parameters$dmin])
  C0 <- x0 * soilphys$c_air[soilphys$lower == group_parameters$dmin]

  # init F0
  F0 <- group_parameters$F0

  lower_boundry_ref <- switch(
    parameters$fit_to,
    "concentration" = C0,
    "molar_fraction" = x0
  )

  #DS and D0
  DS <- soilphys$DS

  #optimisation with error handling returning NA
  prod_optimised <- tryCatch({
    stats::optim(par=group_parameters$prod_start,
                 fn = prod_optim,
                 lower = group_parameters$lowlim_tmp,
                 upper = group_parameters$highlim_tmp,
                 method = "L-BFGS-B",
                 height = height,
                 DS = DS,
                 c_air = soilphys$c_air,
                 lower_boundry_ref = lower_boundry_ref,
                 pmap = pmap,
                 cmap = cmap,
                 profile_ref = profile_ref,
                 zero_flux = parameters$zero_flux,
                 F0 = group_parameters$F0,
                 layer_couple = group_parameters$layer_couple_tmp,
                 wmap = wmap,
                 evenness_factor = parameters$evenness_factor,
                 fit_to = parameters$fit_to
    )},
    error = function(cond) NA)


  if (is.na(prod_optimised[1])){
    pars <- rep(NA, length(group_parameters$prod_start))
    RMSE <- NA
  } else {
    pars <- prod_optimised$par
    RMSE <-prod_optimised$value
  }

  if(parameters$zero_flux == TRUE){
    prods <- pars
  } else {
    F0 <- pars[1]
    prods <- pars[-1]
  }

  #mapping production to correct steps in soilphys
  prod <-prods[pmap]

  #calculating flux
  fluxs <- prod_mod_flux(prod, height, F0)

  # calculating concentration and molar fraction
  if (parameters$fit_to == "concentration"){
    conc_mod <- prod_mod_conc(
      prod,
      height,
      soilphys$DS,
      F0,
      C0)
    x_ppm_mod <- conc_mod / soilphys$c_air
  } else if(parameters$fit_to == "molar_fraction"){
    x_ppm_mod <- prod_mod_x(
      prod,
      height,
      soilphys$DS,
      soilphys$c_air,
      F0,
      x0)
    conc_mod <- x_ppm_mod * soilphys$c_air
  }

  # do not allow negative concentrations!
  if (any_negative_values(conc_mod)){
    fluxs <- NA
    conc_mod <- NA
    x_ppm_mod <- NA
    prod <- NA
    RMSE <- NA
  }


  #toggle progress bar

  if(as.numeric(prof_id) %% 53 == 0){
    p()
  }

  #generating return data_frame
  df <- data.frame(
    prof_id = as.numeric(prof_id),
    step_id = soilphys$step_id,
    flux = fluxs,
    F0 = F0,
    prod = prod,
    conc = conc_mod,
    x_ppm = x_ppm_mod,
    RMSE = RMSE)
  df
}


### parameter initialization ---------------
init_proflux_groups <- function(
    layers_map,
    zero_flux,
    zero_limits){
  #make absolutely sure layers_map is sorted correctly
  layers_map <- layers_map %>%
    dplyr::arrange(upper)

  #this represents the production model depths
  #(including upper and lower bound) per group
  prod_depth_v <-
    c(layers_map$upper,layers_map$lower) %>%
    unique() %>%
    sort()

  #starting values
  prod_start <- rep(0,length(prod_depth_v)-1)

  #initialising boundary conditions
  F0 <- 0
  lowlim_tmp <- layers_map$lowlim
  highlim_tmp <- layers_map$highlim

  layer_couple_tmp <- layers_map$layer_couple[-1]

  # If F0 are optimised as well,
  # more starting parameters need to be set!
  if (zero_flux == FALSE){
    prod_start <- c(0,prod_start)
    lowlim_tmp <- c(min(zero_limits), lowlim_tmp)
    highlim_tmp <- c(max(zero_limits), highlim_tmp)
  }

  dmin <- min(layers_map$lower)

  list(prod_start = prod_start,
       F0 = F0,
       layer_couple_tmp = layer_couple_tmp,
       lowlim_tmp = lowlim_tmp,
       highlim_tmp = highlim_tmp,
       dmin = dmin)
}

init_proflux_parameters <- function(x){

  group_parameters <-
    split(x$layers_map, x$layers_map$group_id) %>%
    purrr::map(init_proflux_groups,
               zero_flux = cfp_zero_flux(x),
               zero_limits = cfp_zero_limits(x))

  list(
    group_parameters = group_parameters,
    evenness_factor = cfp_evenness_factor(x),
    zero_flux = cfp_zero_flux(x),
    fit_to = cfp_fit_to(x)
  )
}


extracols_pf <- function(){
  c("layer",
    "pmap",
    "height",
    "flux",
    "F0",
    "prod",
    "conc",
    "DSD0_fit")
}







