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


  # first separate groups
  x_split <- split_by_group_efficient(x)

  # mean profiles per group
  number_of_groups <- length(x_split)
  mean_profiles <- n_profiles(x) / number_of_groups
  max_profiles <- lapply(x_split, n_profiles) %>%
    unlist() %>% max(na.rm = TRUE)

  parallel_over_groups <- (number_of_groups > 24) &
    (max_profiles/mean_profiles < 3)

  #apply function to all grouped cfp_pfmods
  p <- progressr::progressor(steps = nrow(x$profiles)/53)
  if(parallel_over_groups){
    y <- furrr::future_map(
      x_split,
      pro_flux_group,
      p = p
    )
  } else {
    y <- purrr::map(x_split,
                    pro_flux_group,
                    p = p
    )
  }

  #y <- purrr::map(x_split,pro_flux_group)

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
pro_flux_group <-  function(x, p){

  group <- x$layers_map$group_id[1]
  p(amount = 0,
    message = paste0("calculating group_id: ", group),
    class = "sticky")

  #make absolutely sure layers_map is sorted correctly
  layers_map <- x$layers_map %>%
    dplyr::arrange(upper)

    #this represents the production model depths
    #(including upper and lower bound) per group
    prod_depth_v <-
      c(layers_map$upper,layers_map$lower) %>%
      unique() %>%
      sort()


    #getting lower end of model
    lower_depth <- prod_depth_v[1]

    #starting values
    prod_start <- rep(0,length(prod_depth_v)-1)

    #initialising boundary conditions
    F0 <- 0
    lowlim_tmp <- layers_map$lowlim
    highlim_tmp <- layers_map$highlim

    layer_couple_tmp <- layers_map$layer_couple[-1]

    # If F0 are optimised as well,
    # more starting parameters need to be set!
    if (cfp_zero_flux(x) == FALSE){
      prod_start <- c(0,prod_start)
      lowlim_tmp <- c(min(cfp_zero_limits(x)), lowlim_tmp)
      highlim_tmp <- c(max(cfp_zero_limits(x)), highlim_tmp)
    }


    evenness_factor <- cfp_evenness_factor(x)
    zero_flux <- cfp_zero_flux(x)
    fit_to <- cfp_fit_to(x)

    dmin <- min(x$layers_map$lower)

    #x <- split_by_prof_barebones(x)
    #env <- rlang::new_environment(trim_cfp_dat(x))

    profs_split <- split(data.frame(x$profiles[,names(x$profiles) %in%
                                                 c("gd_id", "sp_id")]),
                         x$profiles$prof_id)
    x <- split_by_prof_env(x)


    df_ret <-furrr::future_imap(
      profs_split,
      env = x,
      prod_start = prod_start,
      F0 = F0,
      layer_couple_tmp = layer_couple_tmp,
      lowlim_tmp = lowlim_tmp,
      highlim_tmp = highlim_tmp,
      evenness_factor = evenness_factor,
      zero_flux = zero_flux,
      dmin = dmin,
      p = p,
      fit_to = fit_to,
      prof_optim
    )

    df_ret <- df_ret %>%
      dplyr::bind_rows()

    return(df_ret)
  }

#########################################-
### Function for per profile optimisation
prof_optim <- function(y,
                       prof_id,
                       env,
                       prod_start,
                       F0,
                       layer_couple_tmp,
                       lowlim_tmp,
                       highlim_tmp,
                       evenness_factor,
                       zero_flux,
                       dmin,
                       p,
                       fit_to){
  gasdata <- get(as.character(y$gd_id),envir = env$gasdata)
  soilphys <- get(as.character(y$sp_id),envir = env$soilphys)
  #gasdata <- env$gasdata[env$gasdata$gd_id == y$gd_id, ]
  #soilphys <- env$soilphys[env$soilphys$sp_id == y$sp_id, ]


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

  profile_ref <- switch(fit_to,
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
    gasdata$x_ppm[gasdata$depth == dmin])
  C0 <- x0 * soilphys$c_air[soilphys$lower == dmin]

  lower_boundry_ref <- switch(
    fit_to,
    "concentration" = C0,
    "molar_fraction" = x0
  )

  #DS and D0
  DS <- soilphys$DS

  #optimisation with error handling returning NA
  prod_optimised <- tryCatch({
    stats::optim(par=prod_start,
                 fn = prod_optim,
                 lower = lowlim_tmp,
                 upper = highlim_tmp,
                 method = "L-BFGS-B",
                 height = height,
                 DS = DS,
                 c_air = soilphys$c_air,
                 lower_boundry_ref = lower_boundry_ref,
                 pmap = pmap,
                 cmap = cmap,
                 profile_ref = profile_ref,
                 zero_flux = zero_flux,
                 F0 = F0,
                 layer_couple = layer_couple_tmp,
                 wmap = wmap,
                 evenness_factor = evenness_factor,
                 fit_to = fit_to
  )},
  error = function(cond) NA)


  if (is.na(prod_optimised[1])){
    pars <- rep(NA, length(prod_start))
    RMSE <- NA
  } else {
    pars <- prod_optimised$par
    RMSE <-prod_optimised$value
  }

  if(zero_flux == TRUE){
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
  if (fit_to == "concentration"){
    conc_mod <- prod_mod_conc(
      prod,
      height,
      soilphys$DS,
      F0,
      C0)
    x_ppm_mod <- conc_mod / soilphys$c_air
  } else if(fit_to == "molar_fraction"){
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
    #x_ppm = x_ppm_mod,
    RMSE = RMSE)
  df
}


###

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

