#' @title pro_flux
#'
#' @description This implements an inverse modeling approach which optimizes
#'   vertically resolved production (or consumption) of the gases in question to
#'   fit a modeled concentration profile to observed data.
#'
#'   One boundary condition of this model is, that there is no incoming or
#'   outgoing flux at the bottom of the lowest layer of the profile. If this
#'   boundary condition is not met, the flux must be optimised as well. This can
#'   be set in \code{zero_flux}.
#'
#' @inheritParams cfp_pfmod
#'
#' @inheritDotParams cfp_pfmod zero_flux zero_limits DSD0_optim evenness_factor known_flux_factor
#'
#'
#' @examples {
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
#' }
#'
#' @family proflux
#'
#'
#' @export

pro_flux <- function(x,
                     ...){
  # for future expansion, remove if implemented
  named_dots <- names(list(...))
  stopifnot("'...' contains unused arguments or that are not yet implemented fully" =
              all(named_dots %in% c("zero_flux", "zero_limits", "evenness_factor")))

UseMethod("pro_flux")
}

#' @rdname pro_flux
#'@exportS3Method
pro_flux.cfp_dat <- function(x,
                             ...,
                             zero_flux = TRUE,
                             zero_limits = c(-Inf,Inf),
                             DSD0_optim = FALSE,
                             evenness_factor = 0,
                             known_flux_factor = 0){
  rlang::check_dots_empty()

  x <- cfp_pfmod(x,
                 zero_flux = zero_flux,
                 zero_limits = zero_limits,
                 DSD0_optim = DSD0_optim,
                 evenness_factor = evenness_factor,
                 known_flux_factor = known_flux_factor)
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
  x_split <- split_by_group(x)

  #apply function to all grouped cfp_pfmods
  p <- progressr::progressor(steps = nrow(x$profiles)/50)
  y <- furrr::future_map(x_split,
                         pro_flux_group,
                         p = p
                         )
  #y <- purrr::map(x_split,pro_flux_group)

  #combine PROFLUX result
  y <- dplyr::bind_rows(y)

  # add some columns
  y <- x$soilphys %>%
    as.data.frame() %>%
    dplyr::left_join(x$profiles, by = "sp_id") %>%
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

    # If either the DS or the F0 are optimised as well,
    # more starting parameters need to be set!
    if(cfp_DSD0_optim(x) == TRUE){
      prod_start <- c(prod_start,rep(0.5,length(prod_start)))
      lowlim_tmp <- c(lowlim_tmp,rep(0,length(lowlim_tmp)))
      highlim_tmp <- c(highlim_tmp,rep(1,length(highlim_tmp)))
    }
    if (cfp_zero_flux(x) == FALSE){
      prod_start <- c(0,prod_start)
      lowlim_tmp <- c(min(cfp_zero_limits(x)), lowlim_tmp)
      highlim_tmp <- c(max(cfp_zero_limits(x)), highlim_tmp)
    }

    x <- split_by_prof(x)
    df_ret <-furrr::future_map(x,
                         prod_start = prod_start,
                         F0 = F0,
                         layer_couple_tmp = layer_couple_tmp,
                         lowlim_tmp = lowlim_tmp,
                         highlim_tmp = highlim_tmp,
                         p = p,
                         prof_optim)

    df_ret <- df_ret %>%
      dplyr::bind_rows()

    return(df_ret)
  }

#########################################-
### Function for per profile optimisation
prof_optim <- function(x,
                       prod_start,
                       F0,
                       layer_couple_tmp,
                       lowlim_tmp,
                       highlim_tmp,
                       p){

  DSD0_optim <- cfp_DSD0_optim(x)
  evenness_factor <- cfp_evenness_factor(x)
  zero_flux <- cfp_zero_flux(x)
  known_flux_factor <- cfp_known_flux_factor(x)


  #mapping productions to soilphys_tmp
  pmap <- x$soilphys$pmap

  #calculating height of each step in m
  height <- x$soilphys$height

  #mapping measured concentrations to soilphys_tmp
  cmap <- x$soilphys$step_id[match(x$gasdata$depth,
                                     x$soilphys$upper)]

  #from ppm to mumol/m^3
  conc <- x$gasdata$x_ppm * x$soilphys$c_air[cmap]

  #shortening to valid cmaps
  conc <- conc[is.finite(cmap)]
  cmap <- cmap[is.finite(cmap)]

  #weigh the observations based on the degrees of freedom
  deg_free_obs <- pmap[cmap]
  n_obs_deg_free <- tabulate(deg_free_obs,max(pmap))
  deg_free_ids <- sort(as.numeric(unique(pmap)))
  weights <- deg_free_ids^2/n_obs_deg_free
  wmap <- weights[deg_free_obs]

  #C0 at lower end of production model
  dmin <- min(x$layers_map$lower)
  C0 <- stats::median(x$gasdata$x_ppm[x$gasdata$depth == dmin]*x$soilphys$c_air[x$soilphys$lower == dmin])

  #DS and D0
  DS <- x$soilphys$DS

  #temporary until implementation
  # D0 <- DS
  # dstor <- 0
  # known_flux <- NA

  #optimisation with error handling returning NA
  prod_optimised <- tryCatch({stats::optim(par=prod_start,
                                           fn = prod_optim,
                                           lower = lowlim_tmp,
                                           upper = highlim_tmp,
                                           method = "L-BFGS-B",
                                           height = height,
                                           DS = DS,
                                           #D0 = D0,
                                           C0 = C0,
                                           pmap = pmap,
                                           cmap = cmap,
                                           conc = conc,
                                           #dstor = dstor,
                                           zero_flux = zero_flux,
                                           F0 = F0,
                                           #known_flux = known_flux,
                                           known_flux_factor = known_flux_factor,
                                           DSD0_optim = DSD0_optim,
                                           layer_couple = layer_couple_tmp,
                                           wmap = wmap,
                                           evenness_factor = evenness_factor
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
  if(DSD0_optim == TRUE){
    DSD0_fit <- prods[-c(1:length(prods)/2)]
    prods <- prods[1:(length(prods)/2)]
  }


  #mapping production to correct steps in soilphys
  prod <-prods[pmap]

  #calculating flux
  fluxs <- prod_mod_flux(prod,height,F0)
  conc_mod <- prod_mod_conc(prod,height,x$soilphys$DS,F0,C0)

  # do not allow negative concentrations!
  if (any_negative_values(conc_mod)){
    fluxs <- NA
    conc_mod <- NA
    prod <- NA
    RMSE <- NA
  }

  #toggle progress bar
  if(x$profiles$prof_id %% 50 == 0){
  p()
  }

  #generating return data_frame
  df <- data.frame(
    prof_id = x$profiles$prof_id[1],
    step_id = x$soilphys$step_id,
    flux = fluxs,
    F0 = F0,
    prod = prod,
    conc = conc_mod,
    RMSE = RMSE)
  if(DSD0_optim == TRUE){
    df$DSD0_fit <- DSD0_fit[pmap]
  }
  return(df)
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

