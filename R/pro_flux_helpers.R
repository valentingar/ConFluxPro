#' @title helper functions for \code{pro_flux()}
#'
#' @description These functions are internal
#' helper functions that are needed for the pro_flux method
#'
#' @keywords internal
#' @noRd


# splitting soilphys so that the each slice is homogenous
# and the gas measurements are at the intersections
depth_filler <- function(.x,.y){

  #getting depths of gasdata at that plot
  g_depths <-
    gasdata %>%
    dplyr::right_join(.y) %>%
    dplyr::pull(depth) %>%
    unique()

  l_depths <-
    layers_map %>%
    dplyr::right_join(.y) %>%
    dplyr::pull(upper) %>%
    unique()

  #getting interfaces of soilphys at that plot
  s_highs <- .x%>%
    dplyr::pull(upper) %>%
    unique()
  s_lows <- .x %>%
    dplyr::pull(lower) %>%
    unique()
  s_depths <- c(s_highs,s_lows)

  #creating union of the two + sorting
  depths <- sort(unique(c(g_depths,s_depths,l_depths)))

  #find which depths need to be inserted
  to_int <- sort(depths[depths %in% s_depths ==F])

  #generate a map for the different layers with id
  k_map <-
    .x %>%
    dplyr::select(upper,lower) %>%
    dplyr::distinct() %>%
    dplyr::mutate(k_id = row_number())

  #find layer id for depths to be inserted
  k_ind <- unlist(lapply(to_int,function(i){
    k_id <- k_map$k_id[k_map$upper>i & k_map$lower<i]
    if(length(k_id) == 0) k_id <- NA
    return(k_id)
  }))

  #resizing depth to only include those that work
  depths <- depths [!depths %in% to_int[is.na(k_ind)]]

  #counting how often each layer needs to be in the final product
  k_map <- k_map %>%
    dplyr::mutate(k_count = unlist(lapply(k_id,function(i) length(which(i %in% !!k_ind == T))))+1)

  #expanding to final map and adding the correct boundaries
  k_map_n <- k_map %>%
    tidyr::uncount(k_count) %>%
    dplyr::mutate(lower = !!depths[-length(!!depths)],
                  upper = !!depths[-1])

  #final product
  .x <- .x %>%
    dplyr::left_join(k_map) %>%
    dplyr::select(!dplyr::any_of(c("upper","lower")))%>%
    dplyr::left_join(k_map_n) %>%
    dplyr::select(!dplyr::any_of({c("k_id","k_count")}))

  return(.x)
}



## Function to perform preparation for each
## group and then run prof_optim on all.
profile_stack <-
  function(
    group_id,
    gr,
    n_gr,
    gasdata,
    soilphys,
    layers_map,
    Ds_optim,
    zero_flux,
    known_flux,
    prod_depth,
    profiles,
    groups_map
  ){

  #choose correct prod_depth!
  prod_depth_df <- prod_depth[prod_depth$group_id == group_id, ]
  prod_depth_v <- prod_depth_df$depth

  #filtering only relevant profiles for the group
  profiles_tmp <-
    groups_map %>%
    dplyr::filter(group_id == !!group_id) %>%
    dplyr::left_join(profiles)

  #sorting prod_depth and getting lower end of model
  prod_depth_v <- sort(prod_depth_v)
  lower_depth <- prod_depth_v[1]

  #subset soilphys and gasdata accordingly
  gasdata_gr <- gasdata[gasdata$prof_id %in% profiles_tmp$prof_id]
  soilphys_gr <- soilphys[soilphys$prof_id %in% profiles_tmp$prof_id]

  soilphys_gr <- soilphys_gr  %>%
    dplyr::mutate(pmap =  findInterval(depth,!!prod_depth_v))


  #starting values
  prod_start <- rep(0,length(prod_depth_v)-1)

  lowlim_tmp <- layers_map$lowlim[layers_map$group_id== group_id]
  highlim_tmp <- layers_map$highlim[layers_map$group_id== group_id]

  layer_couple_tmp <- layers_map$layer_couple[layers_map$group_id== group_id]
  layer_couple_tmp <- layer_couple_tmp[-1]
  #print(paste("layer_couple:",paste(layer_couple_tmp,collapse = " ")))

  if(Ds_optim == T){
    prod_start <- c(prod_start,rep(1e-8,length(prod_start)))
    lowlim_tmp <- c(lowlim_tmp,rep(1e-13,length(lowlim_tmp)))
    highlim_tmp <- c(highlim_tmp,rep(1e-4,length(highlim_tmp)))
    #prevent Ds_optim to put out negative values
    #lowlim_tmp <- lowlim_tmp<-c(lowlim_tmp[1:ceiling(length(lowlim_tmp)/2)],rep(1e-13,floor(length(lowlim_tmp)/2)))
    #prod_start <- prod_start<-c(prod_start[1:ceiling(length(prod_start)/2)],rep(1e-8,floor(length(prod_start)/2)))
    #highlim_tmp <- highlim_tmp<-c(lowlim_tmp[1:ceiling(length(lowlim_tmp)/2)],rep(1e-4,floor(length(lowlim_tmp)/2)))
  }
  if (zero_flux == F){
    prod_start <- c(0,prod_start)
    lowlim_tmp <- c(min(zero_limits),lowlim_tmp)
    highlim_tmp <- c(max(zero_limits),highlim_tmp)
  }
  #print(prod_start)
  #print(lowlim_tmp)
  #print(highlim_tmp)

  n_profs <- nrow(profiles_tmp)
  printers <-floor(seq(1,nrow(profiles_tmp),length.out = 11))
  printers<-profiles_tmp$prof_id[printers]
  print_percent <- seq(0,100,10)

  #initialising boundary conditions
  F0 <- 0

  #print(is.null(known_flux))

  #for users
  print(paste0("group ",gr,"/",n_gr))
  print(paste(n_profs,"profiles"))

  #getting good initial guesses from 1 profile
  #prod_start <- prof_optim(1,
  #                         prod_start,
  #                         return_pars = T)
  #
  #if (any(is.na(prod_start))){
  #  prod_start <- rep(0,length(prod_start))
  #}

  #the above is not helpful. for now:
  prod_start <-rep(0,length(prod_start))

  #per-profile calculation
  #proflux<- lapply(profiles_tmp$prof_id,
  #                 function(i) prof_optim(i,prod_start,return_pars = F))

  #df_ret <- dplyr::bind_rows(proflux)
  df_ret <-
    purrr::map2(split(gasdata_gr %>% dplyr::arrange(prof_id),
                      gasdata_gr$prof_id),
                split(soilphys_gr%>% dplyr::arrange(prof_id),
                      soilphys_gr$prof_id),
                prod_start = prod_start,
                printers = printers,
                print_percent = print_percent,
                known_flux = known_flux,
                zero_flux = zero_flux,
                Ds_optim = Ds_optim,
                F0 = F0,
                known_flux_factor = known_flux_factor,
                layer_couple_tmp = layer_couple_tmp,
                lowlim_tmp = lowlim_tmp,
                highlim_tmp = highlim_tmp,
                prof_optim) %>%
    dplyr::bind_rows()
  return(df_ret)
}

#########################################-
### Function for per profile optimisation
prof_optim <- function(gasdata_tmp,
                       soilphys_tmp,
                       prod_start,
                       return_pars = F,
                       printers,
                       print_percent,
                       known_flux,
                       zero_flux,
                       Ds_optim,
                       F0,
                       known_flux_factor,
                       layer_couple_tmp,
                       lowlim_tmp,
                       highlim_tmp){

  i <- gasdata_tmp$prof_id[1]

  if (i %in% printers){
    print(paste0(print_percent[printers == i]," %"))
  }
  #for known_flux b.c.
  if(is.null(known_flux)==F){
    known_flux_df <-known_flux[list(i)]
    known_flux_tmp <- known_flux_df$flux
  } else {
    known_flux_tmp <- NA
  }

  #gasdata_tmp <- gasdata_gr[list(i)]
  #soilphys_tmp <- soilphys_gr[list(i)]


  #mapping productions to soilphys_tmp
  pmap <- soilphys_tmp$pmap

  #calculating height of each step in m
  height <- soilphys_tmp$height

  #mapping measured concentrations to soilphys_tmp
  cmap <- soilphys_tmp$step_id[match(gasdata_tmp$depth,
                                     soilphys_tmp$upper)]
  #shortening to valid cmaps
  conc <- conc[is.finite(cmap)]
  cmap <- cmap[is.finite(cmap)]

  #weigh the observations based on the degrees of freedom
  deg_free_obs <- pmap[cmap]
  n_obs_deg_free <- tabulate(deg_free_obs )
  deg_free_ids <- sort(as.numeric(unique(pmap)))
  weights <- deg_free_ids^2/n_obs_deg_free
  wmap <- weights[deg_free_obs]

  #C0 at lower end of production model
  dmin <- min(gasdata_tmp$depth)
  C0 <- median(gasdata_tmp$NRESULT_ppm[gasdata_tmp$depth == dmin]*soilphys_tmp$rho_air[soilphys_tmp$lower == dmin])

  #from ppm to mumol/m^3
  conc <- gasdata_tmp$NRESULT_ppm * soilphys_tmp$rho_air[cmap]

  #storage term
  dstor <-0


  #optimisation with error handling returning NA
  pars <- tryCatch({
    prod_optimised<-optim(par=prod_start,
                          fn = prod_optim,
                          lower = lowlim_tmp,
                          upper = highlim_tmp,
                          method = "L-BFGS-B",
                          height = height,
                          DS = soilphys_tmp$DS,
                          C0 = C0,
                          pmap = pmap,
                          cmap = cmap,
                          conc = conc,
                          dstor = dstor,
                          zero_flux=zero_flux,
                          F0 = F0,
                          known_flux = known_flux_tmp,
                          known_flux_factor = known_flux_factor,
                          Ds_optim = Ds_optim,
                          layer_couple = layer_couple_tmp,
                          wmap = wmap
    )
    pars <-(prod_optimised$par)
  },error = function(e) {return(rep(NA, length(prod_start)))})


  if(return_pars == T){
    return(pars)
  }

  if(zero_flux == T){
    prods <- pars
  } else {
    F0 <- pars[1]
    prods <- pars[-1]
  }
  if(Ds_optim == T){
    Ds_fit <- prods[-c(1:length(prods)/2)]
    prods <- prods[1:(length(prods)/2)]
  }


  #mapping production to correct steps in soilphys
  prod <-prods[pmap]

  #calculating flux
  fluxs <- prod_mod_flux(prod,height,F0)
  conc_mod <- prod_mod_conc(prod,height,soilphys_tmp$DS,F0,C0)

  #generating return data_frame
  soilphys_tmp$flux <- fluxs
  soilphys_tmp$F0 <- F0
  soilphys_tmp$prod <-prod
  soilphys_tmp$conc <- conc_mod
  if(Ds_optim ==T){
    soilphys_tmp$Ds_fit <- Ds_fit[pmap]
  }
  return(soilphys_tmp)
}




