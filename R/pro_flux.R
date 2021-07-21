#' @title pro_flux
#'
#' @description This implements an inverse modeling approach which optimizes
#' vertically resolved production (or consumption) of the gases in question to fit
#' a modeled concentration profile to observed data.
#'
#' One boundary condition of this model is, that there is no incoming or outgoing flux at the bottom of the lowest layer
#' of the profile. If this boundary condition is not met, the flux must be optimised as well. This can be set in "zero_fluix"
#'
#' @param gasdata (dataframe)
#' @param soilphys (dataframe)
#'
#' @param layers_map (dataframe) That defines the layers of homogenous production as well as the upper and lower limits of production rate.
#'
#' @param id_cols (character vector) The names of the columns that together uniquely identify one profile. Must be present in gasdata.
#'
#' @param storage_term (logical) Should changes in storage be accounted for? Default is F.
#' Only works if data is present in a temporal dimension as well and is probably only
#' representative for a high temporal resolution (hours).
#'
#' @param zero_flux (logical) Applies the zero-flux boundary condition? If FALSE, the first value in X
#' represents the incoming flux to the lowest layer.
#'
#' @param zero_limits (numeric vector) a vector of length 2 defining the lower and upper limit of the lowest flux if zero_flux = F.
#'
#' @param known_flux (dataframe) a dataframe that gives a known efflux for each profile defined by id_cols.
#' If this is provided, the productions are optimised to meet this flux as well as the concentration measurements provided.
#'
#' @param known_flux_factor (numeric)
#' a numeric value > 0 that represents a weight for the error calculation with the known flux. A higher value means that the optimisation will weigh the error to the efflux more than in regard to the concentration measurements.
#' Must be determined manually by trying out!
#'
#' @param Ds_optim (logical) If True, the diffusion coefficient (DS) values are also object to optimisation together with the production. The fit values are given as Ds_fit in the return table. Only makes sense to use in combination with known_flux.
#'
#' @examples pro_flux(gasdata,
#' soilphys,
#' target_depth,
#' storage_term = F,
#' zero_flux = T)
#'
#' @family proflux
#'
#'
#' @import dplyr
#' @import data.table
#' @import ddpcr
#' @import purrr

#'
#' @export

pro_flux <- function(gasdata,
                     soilphys,
                     layers_map,
                     id_cols,
                     storage_term = F,
                     zero_flux = T,
                     zero_limits = c(-Inf,Inf),
                     known_flux = NULL,
                     known_flux_factor = 0,
                     Ds_optim = F){

  #filtering out problematic measurements
  gasdata <-
  gasdata %>%
  dplyr::filter(!dplyr::across(dplyr::any_of({c("gas","depth","NRESULT_ppm","id_cols")}),~is.na(.x)))

  #adapting layers_map to only contain target columns
  id_tmp <- c(id_cols,"upper","lower","layer","highlim","lowlim","layer_couple")

  #filtering unnecessary columns, making unique and adding grouping id
  layers_map <- layers_map %>%
    dplyr::select(dplyr::any_of({id_tmp})) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({id_cols}))) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    dplyr::mutate(join_help = 1) %>%
    dplyr::arrange(lower)

  #this maps the groups to any relevant id_cols
  groups_map <-
    layers_map %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of({c("group_id",id_cols)})) %>%
    dplyr::distinct()

  #this is the subset of id_cols that is relevant for the groups
  id_cols_groups <- id_cols[id_cols %in% names(groups_map)]

  #this represents the production model depths
  #(including upper and lower bound) per group
  prod_depth <- layers_map %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of({c("upper","lower","group_id","join_help")})) %>%
    tidyr::pivot_longer(cols = c("upper","lower"),
                        names_to = "type",
                        values_to = "depth") %>%
    dplyr::select(-type) %>%
    dplyr::distinct()


  # getting depths for soilphys adaption
  # this is to make sure that each "slice" of the soil
  # is homogenous and that the measured concentrations are
  # at a border between slices.
  target_depths <- gasdata %>%
    dplyr::ungroup() %>%
    dplyr::select(any_of({c(id_cols_groups,"depth")})) %>%
    dplyr::distinct() %>%
    dplyr::full_join(soilphys %>%
                       dplyr::ungroup() %>%
                       dplyr::select(any_of({c("upper","lower",id_cols)})) %>%
                       tidyr::pivot_longer(cols = c("upper","lower"),values_to = "depth") %>%
                       dplyr::select(any_of({c(id_cols_groups,"depth")})) %>%
                       dplyr::distinct()

    ) %>%
    dplyr::bind_rows(prod_depth %>%
                left_join(groups_map) %>%
                select(-c("join_help","group_id"))) %>%
    dplyr::distinct()

  sp_names <- names(soilphys)
  sp_names <- sp_names[!sp_names %in% c(id_cols,"gas","depth","upper","lower")]

  # here the present profiles are identified based on the
  # id_cols provided
  profiles <- gasdata %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of({id_cols})) %>%
    dplyr::distinct() %>%
    dplyr::mutate(prof_id = row_number())

  #select relevant profiles from soilphys
  soilphys <- profiles %>%
    dplyr::inner_join(soilphys)



  # splitting soilphys so that the each slice is homogenous
  # and the gas measurements are at the intersections
  id_cols_fill <- id_cols[id_cols %in% names(gasdata) &
                            id_cols %in% names(soilphys) &
                            id_cols %in% names(layers_map) ]


  soilphys <-
    soilphys %>%
    dplyr::mutate(r_id = dplyr::row_number())

  soilphys<-
    soilphys %>%
    dplyr::select(dplyr::any_of(c(id_cols,"upper","lower","r_id"))) %>%
    dplyr::group_by(dplyr::across(dplyr::any_of({id_cols_fill}))) %>%
    dplyr::group_modify(~{
      ddpcr::quiet(.x <-  depth_filler(.x,.y))
      return(.x)
    }) %>%
    dplyr::left_join(soilphys %>%
                       dplyr::select(!any_of(c("upper","lower")))
    )


  #adding prof_id
  soilphys <- profiles %>% dplyr::left_join(soilphys)

  #arranging soilphys and creating id of steps
  soilphys <- soilphys  %>%
    dplyr::arrange(upper) %>%
    dplyr::group_by(prof_id) %>%
    dplyr::mutate(step_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(na_flag = ifelse(is.na(DS) | is.na(rho_air) | is.na(upper) | is.na(lower),T,F)) %>%
    dplyr::mutate(height = (upper-lower)/100)

  #only work with functioning profiles
  profiles <-
    soilphys %>%
    dplyr::filter(na_flag == F) %>%
    dplyr::select(prof_id) %>%
    dplyr::distinct() %>%
    dplyr::left_join(profiles) %>%
    dplyr::mutate(join_help = 1)

  #shrinking gasdata to relevant profiles
  gasdata <- profiles %>%
    dplyr::left_join(gasdata)

  #making copy of unaltered soilphys
  soilphys_backup <- soilphys %>%
    dplyr::select(-dplyr::any_of(c("flux","prod","F0"))) %>%
    dplyr::select(-dplyr::any_of(c("depth","height","DS","rho_air","upper","lower","step_id")))

  #turning data into data.table for fast subsetting
  # this radically improves performance
  soilphys <- data.table::as.data.table(soilphys %>%
                              dplyr::ungroup() %>%
                              dplyr::select(prof_id,depth,height,DS, rho_air,upper,lower,step_id))
  gasdata <- data.table::as.data.table(gasdata %>%
                             dplyr::select(prof_id,depth,NRESULT_ppm))


  #if known flux b.c. applies:
  if(is.null(known_flux)==F){
    known_flux <- known_flux %>%
      dplyr::filter(is.na(flux)==F) %>%
      dplyr::select(dplyr::any_of({c(id_cols, "flux")})) %>%
      dplyr::right_join(profiles) %>%
      data.table::as.data.table()
    data.table::setkey(known_flux, prof_id)
  }

  #Initialising prof_id column for fast subsetting
  data.table::setkey(soilphys,prof_id)
  data.table::setkey(gasdata,prof_id)


  id_tmp <- id_cols[id_cols %in% names(prod_depth)]

  prod_depth <-
  profiles %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of({c(id_tmp,"join_help")})) %>%
    dplyr::distinct() %>%
    dplyr::left_join(prod_depth)

  groups <- unique(prod_depth$group_id)
  n_gr <- length(groups)


  #for users patience
  message("started profile fitting. This may take very long. ~30 s/1000 profiles!")
  message(paste(nrow(profiles),"profiles total"))

  df_ret <- lapply(1:n_gr,function(gr){
    group <- groups[gr]
    df<-profile_stack(group,
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
                      groups_map)
    return(df)
  }) %>%
    dplyr::bind_rows()

  ## adding back the rest of the variables
  join_names <- names(df_ret)[names(df_ret) %in% names(soilphys_backup)]

  df_ret <- df_ret %>%
    dplyr::left_join(soilphys_backup,
                     by = join_names)

  #removing all unecessary data
  rm(soilphys_backup)
  rm(gasdata)
  rm(soilphys)

  message("Done :)")
  return(df_ret)
}

#################################################
### ------------- HELPERS -----------------------
#################################################



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

  #from ppm to mumol/m^3
  conc <- gasdata_tmp$NRESULT_ppm * soilphys_tmp$rho_air[cmap]

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




