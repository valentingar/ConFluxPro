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
#' @param x A valid \code{cfp_dat} object. This contains all the necessary input
#' data.
#'
#' @param storage_term (logical) Should changes in storage be accounted for?
#'   Default is F. Only works if data is present in a temporal dimension as well
#'   and is probably only representative for a high temporal resolution (hours).
#'
#' @param zero_flux (logical) Applies the zero-flux boundary condition? If
#'   FALSE, the first value in X represents the incoming flux to the lowest
#'   layer.
#'
#' @param zero_limits (numeric vector) a vector of length 2 defining the lower
#'   and upper limit of the lowest flux if zero_flux = F.
#'
#' @param known_flux_factor (numeric) a numeric value > 0 that represents a
#'   weight for the error calculation with the known flux. A higher value means
#'   that the optimisation will weigh the error to the efflux more than in
#'   regard to the concentration measurements. Must be determined manually by
#'   trying out!
#'
#' @param DSD0_optim (logical) If True, the diffusion coefficient (DSD0) values are
#'   also object to optimisation together with the production. DSD0 is varied between
#'   values 0 and 1, DS is then recalculated from D0 to be used in the model. The fit values
#'   are given as DSD0_fit in the return table. Only makes sense to use in
#'   combination with known_flux.
#'
#' @param evenness_factor (numeric) A user defined factor used to penalise strong
#' differences between the optimised production rates. This must be identified by
#' trial-and-error and can help prevent that production rates are simply set to zero
#' basically the lower a production is relative to the the maximum of the absolute of
#' all productions, the higher it is penalised. The \code{evenness_factor} then
#' defines the weight of this penalty in the optimisation algorithm \code{\link{prod_optim}}.
#'
#' @examples {
#' data("gasdata")
#' data("soilphys")
#'
#' library(dplyr)
#'
#' lmap <- soilphys %>%
#'   select(upper,site) %>%
#'   distinct() %>%
#'   group_by(site) %>%
#'   slice_max(upper) %>%
#'   summarise(upper = c(upper,0),
#'             lower = c(0,-100),
#'             lowlim = 0,
#'             highlim = 1000,
#'             layer_couple = 0)
#' PROFLUX <-
#'   pro_flux(gasdata,
#'            soilphys,
#'            lmap,
#'            c("site","Date"))
#' }
#'
#' @family proflux
#'
#'
#' @import dplyr
#' @importFrom  ddpcr quiet
#'
#' @export

pro_flux <- function(x,
                     ...){
UseMethod("pro_flux")
}

#'@exportS3Method
pro_flux.cfp_dat <- function(x,
                             ...){
  x <- cfp_pfmod(x,
                 ...)
  .Class <- "cfp_pfmod"
  NextMethod()
}

#'@exportS3Method
pro_flux.cfp_pfres <- function(x,
                               ...){
  x <- as_cfp_pfmod(x)
  NextMethod()
}

#'@exportS3Method
pro_flux.cfp_pfmod <- function(x,
                             ...){

  stopifnot(inherits(x,"cfp_pfmod"))

  #make sure soilphys is in ascending order
  x$soilphys <- dplyr::arrange(x$soilphys,upper)

  # first separate groups
  x_split <- split_by_group(x)

  #apply function to all grouped cfp_pfmods
  future::plan(future::multisession, workers = 1)
  y <- furrr::future_map(x_split,pro_flux_group)
  future::plan(future::sequential)
  #y <- purrr::map(x_split,pro_flux_group)

  #combine PROFLUX result
  y <- dplyr::bind_rows(y)

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
pro_flux_group <-  function(x){

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
    if(cfp_DSD0_optim(x) == T){
      prod_start <- c(prod_start,rep(0.5,length(prod_start)))
      lowlim_tmp <- c(lowlim_tmp,rep(0,length(lowlim_tmp)))
      highlim_tmp <- c(highlim_tmp,rep(1,length(highlim_tmp)))
    }
    if (cfp_zero_flux(x) == F){
      prod_start <- c(0,prod_start)
      lowlim_tmp <- c(min(x$zero_limits),lowlim_tmp)
      highlim_tmp <- c(max(x$zero_limits),highlim_tmp)
    }

    x <- split_by_prof(x)
    df_ret <-purrr::map(x,
                         prod_start = prod_start,
                         F0 = F0,
                         layer_couple_tmp = layer_couple_tmp,
                         lowlim_tmp = lowlim_tmp,
                         highlim_tmp = highlim_tmp,
                         prof_optim) %>%
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
                       highlim_tmp){

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
  conc <- x$gasdata$NRESULT_ppm * x$soilphys$rho_air[cmap]

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
  dmin <- min(x$gasdata$depth)
  C0 <- stats::median(x$gasdata$NRESULT_ppm[x$gasdata$depth == dmin]*x$soilphys$rho_air[x$soilphys$lower == dmin])

  #DS and D0
  DS <- x$soilphys$DS

  #temporary until implementation
  D0 <- DS
  dstor <- 0
  known_flux <- NA

  #optimisation with error handling returning NA
  prod_optimised <- tryCatch({prod_optimised<-stats::optim(par=prod_start,
                                                           fn = prod_optim,
                                                           lower = lowlim_tmp,
                                                           upper = highlim_tmp,
                                                           method = "L-BFGS-B",
                                                           height = height,
                                                           DS = DS,
                                                           D0 = D0,
                                                           C0 = C0,
                                                           pmap = pmap,
                                                           cmap = cmap,
                                                           conc = conc,
                                                           dstor = dstor,
                                                           zero_flux = zero_flux,
                                                           F0 = F0,
                                                           known_flux = known_flux,
                                                           known_flux_factor = known_flux_factor,
                                                           DSD0_optim = DSD0_optim,
                                                           layer_couple = layer_couple_tmp,
                                                           wmap = wmap,
                                                           evenness_factor = evenness_factor
  )},
  error = NA)


  if (is.na(prod_optimised[1])){
    pars <- rep(NA, length(prod_start))
    RMSE <- NA
  } else {
    pars <- prod_optimised$par
    RMSE <-prod_optimised$value
  }

  if(zero_flux == T){
    prods <- pars
  } else {
    F0 <- pars[1]
    prods <- pars[-1]
  }
  if(DSD0_optim == T){
    DSD0_fit <- prods[-c(1:length(prods)/2)]
    prods <- prods[1:(length(prods)/2)]
  }


  #mapping production to correct steps in soilphys
  prod <-prods[pmap]

  #calculating flux
  fluxs <- prod_mod_flux(prod,height,F0)
  conc_mod <- prod_mod_conc(prod,height,x$soilphys$DS,F0,C0)

  #generating return data_frame
  df <- data.frame(
    prof_id = x$profiles$prof_id[1],
    step_id = x$soilphys$step_id,
    flux = fluxs,
    F0 = F0,
    prod = prod,
    conc = conc_mod,
    RMSE = RMSE)
  if(DSD0_optim ==T){
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

