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
#' @param target_depth (numeric vector) This vector determines the depths of the interfaces between
#' different production values to be fitted. This allows for an adaptive model with an individual
#' number variables and hence degrees-of-freedom. So for three different production values,
#' two depths that mark the intersection must be given.
#' @param storage_term (logical) Should changes in storage be accounted for? Default is F.
#' Only works if data is present in a temporal dimension as well and is probably only
#' representative for a high temporal resolution (hours).
#' @param zero_flux (logical) Applies the zero-flux boundary condition? If FALSE, the first value in X
#' represents the incoming flux to the lowest layer.
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
#' @export

pro_flux <- function(gasdata,
                      soilphys,
                      prod_depth,
                      storage_term = F,
                      zero_flux = T,
                      known_flux = NULL,
                      Ds_optim = F,
                      known_flux_factor = 0,
                      highlim,
                      lowlim,
                      id_cols){

  #filtering out problematic measurements
  gasdata <- gasdata %>% filter(!is.na(depth),
                                !is.na(gas),
                                !is.na(Plot),
                                !is.na(NRESULT_ppm))


  #checking if prod_depth is a numeric vector or dataframe
  #creating dataframe if it isnt.
  if(is.vector(prod_depth)){
    prod_depth<- cbind.data.frame(depth = prod_depth)
  } else if (is.data.frame(prod_depth)==F){
    stop("Input prod_depth is wrong. Must be numeric vector or data.frame")
  }

    #adapting prod_depth to only contain target columns
    id_tmp <- c(id_cols,"depth")

    prod_depth <- prod_depth %>%
    dplyr::select(dplyr::any_of({id_tmp})) %>%
    dplyr::distinct() %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
      dplyr::mutate(join_help = 1)


  #getting depths for soilphys adaption
  target_depths <- gasdata %>%
    dplyr::ungroup() %>%
    dplyr::select(Plot,depth) %>%
    dplyr::distinct() %>%
    dplyr::full_join(soilphys %>%
                       dplyr::ungroup() %>%
                       dplyr::select(c("upper","lower","Plot")) %>%
                       tidyr::pivot_longer(cols = c("upper","lower"),values_to = "depth") %>%
                       dplyr::select(Plot,depth) %>%
                       dplyr::distinct()

    ) %>%
    dplyr::distinct()

  sp_names <- names(soilphys)
  sp_names <- sp_names[!sp_names %in% c("Plot","gas","Date","depth","upper","lower")]

  profiles <- gasdata %>%
    dplyr::ungroup() %>%
    dplyr::select({{id_cols}}) %>%
    dplyr::distinct() %>%
    dplyr::mutate(prof_id = row_number())

  #select relevant profiles from soilphys
  soilphys <- profiles %>% dplyr::left_join(soilphys)


  #something to split soilphys to have well depths at edges.
  soilphys <-discretize_depth(soilphys,
                              param = sp_names,
                              method = "boundary",
                              depth_target = target_depths,
                              boundary_nearest =F,
                              id_cols = c("Plot","gas","Date"))

  #select relevant profiles from soilphys
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

  gasdata <- profiles %>%
    dplyr::left_join(gasdata)

  soilphys_backup <- soilphys
  soilphys <- as.data.table(soilphys %>%
                              dplyr::ungroup() %>%
                              dplyr::select(prof_id,depth,height,DS, rho_air,upper,lower,step_id,pmap))
  gasdata <- as.data.table(gasdata %>%
                             dplyr::select(prof_id,depth,NRESULT_ppm))


  #if known flux b.c. applies:
  if(is.null(known_flux)==F){
    known_flux <- known_flux %>%
      dplyr::filter(is.na(flux)==F) %>%
      dplyr::select(dplyr::any_of({c(id_cols,"flux")})) %>%
      dplyr::right_join(profiles) %>%
      as.data.table()
    setkey(known_flux,prof_id)
  }

  #Initialising prof_id column for fast subsetting
  setkey(soilphys,prof_id)
  setkey(gasdata,prof_id)



  profile_stack <- function(group_id,gr){

    #choose correct prod_depth!
    prod_depth_df <- prod_depth[prod_depth$group_id == group_id,]
    prod_depth_v <- prod_depth_df$depth

    profiles_tmp <-profiles %>% dplyr::right_join(prod_depth_df %>%
                                                    dplyr::select(dplyr::any_of({c(id_cols,"join_help")})) %>%
                                                    dplyr::distinct())


  #sorting prod_depth and getting lower end of model
    prod_depth_v <- sort(prod_depth_v)
    lower_depth <- prod_depth_v[1]

    #subset soilphys and gasdata accordingly
    gasdata_gr <- gasdata %>% dplyr::right_join(profiles_tmp)
    soilphys_gr <- soilphys %>% dplyr::right_join(profiles_tmp)

    soilphys_gr <- soilphys_gr  %>%
    dplyr::mutate(pmap =  findInterval(depth,!!prod_depth_v))


  #starting values
  prod_start <- rep(0,length(prod_depth_v)-1)
  if(Ds_optim == T){
    prod_start <- c(prod_start,prod_start)
  }
  if (zero_flux == F){
    prod_start <- c(0,prod_start)
  }
  lowlim_tmp <- rep(lowlim,length(prod_start))
  highlim_tmp <- rep(highlim,length(prod_start))
  if(Ds_optim == T){
    #prevent Ds_optim to put out negative values
    lowlim_tmp <- lowlim_tmp<-c(lowlim_tmp[1:ceiling(length(lowlim_tmp)/2)],rep(0.0001,floor(length(lowlim_tmp)/2)))
    prod_start <- prod_start<-c(prod_start[1:ceiling(length(prod_start)/2)],rep(1,floor(length(prod_start)/2)))

  }
  print(prod_start)
  print(lowlim_tmp)
  print(highlim_tmp)

  n_profs <- nrow(profiles_tmp)
  printers <-floor(seq(1,nrow(profiles_tmp),length.out = 11))
  printers<-profiles_tmp$prof_id[printers]
  print_percent <- seq(0,100,10)

  #initialising boundary conditions
  F0 <- 0
  known_flux_tmp <- NA

  print(is.null(known_flux))

  #for users
  print(paste0("group ",gr,"/",n_gr))
  print(paste(n_profs,"profiles"))

  #per-profile calculation
  proflux<- lapply(profiles_tmp$prof_id,function(i){

    if (i %in% printers){
      print(paste0(print_percent[printers == i]," %"))
    }

    #for known_flux b.c.
    if(is.null(known_flux)==F){
      known_flux_df <-known_flux[J(i),]
      known_flux_tmp <- known_flux_df$flux
    }

    gasdata_tmp <- gasdata_gr[J(i),]
    soilphys_tmp <- soilphys_gr[J(i),]

    #mapping productions to soilphys_tmp
    pmap <- soilphys_tmp$pmap

    #calculating height of each step in m
    height <- soilphys_tmp$height

    #mapping measured concentrations to soilphys_tmp
    cmap <- unlist(lapply(gasdata_tmp$depth,function(g) ifelse(g %in% soilphys_tmp$upper,soilphys_tmp$step_id[soilphys_tmp$upper==g],NA)))

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
                            known_flux = known_flux_tmp,
                            known_flux_factor = known_flux_factor,
                            Ds_optim = Ds_optim
      )
      pars <-(prod_optimised$par)
    },error = function(e) {return(rep(NA, length(prod_start)))})



    if(zero_flux == T){
      prods <- pars
    } else {
      F0 <- pars[1]
      prods <- pars[-1]
    }
    if(Ds_optim == T){
      Ds_f <- prods[-c(1:length(prods)/2)]
      prods <- prods[1:(length(prods)/2)]
    }


    #mapping production to correct steps in soilphys
    prod <-prods[pmap]

    #calculating flux
    fluxs <- prod_mod_flux(prod,height,F0)

    #generating return data_frame
    soilphys_tmp$flux <- fluxs
    soilphys_tmp$F0 <- F0
    soilphys_tmp$prod <-prod
    if(Ds_optim ==T){
    soilphys_tmp$Ds_f <- Ds_f[pmap]
    }
    return(soilphys_tmp)
  })

  df_ret <- dplyr::bind_rows(proflux)
  return(df_ret)
  }


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
  print("started profile fitting. This may take very long. ~30 s/1000 profiles!")
  print(paste(nrow(profiles),"profiles total"))

  df_ret <- lapply(1:n_gr,function(gr){
    group <- groups[gr]
    df<-profile_stack(group,gr)
    return(df)
  }) %>% bind_rows()


  df_ret <- df_ret %>% dplyr::left_join(soilphys_backup %>%
                                          dplyr::select(-dplyr::any_of(c("flux","prod","F0"))),)
  return(df_ret)
}
