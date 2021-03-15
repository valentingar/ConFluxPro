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
                      highlim,
                      lowlim,
                      id_cols){

  #filtering out problematic measurements
  gasdata <- gasdata %>% filter(!is.na(depth),
                                !is.na(gas),
                                !is.na(Plot),
                                !is.na(NRESULT_ppm))


  target_depths <- gasdata %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Plot) %>%
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
    dplyr::filter(depth == min(prod_depth)) %>%
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

  #sorting production depths
  prod_depth <- sort(prod_depth)

  #arranging soilphys_tmp and creating id of steps
  soilphys <- soilphys  %>%
    dplyr::arrange(upper) %>%
    dplyr::group_by(prof_id) %>%
    dplyr::mutate(step_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(na_flag = ifelse(is.na(DS) | is.na(rho_air) | is.na(upper) | is.na(lower),T,F)) %>%
    dplyr::mutate(height = (upper-lower)/100) %>%
    dplyr::mutate(pmap =  findInterval(depth,!!prod_depth))

  #only work with functioning profiles
  profiles <-
    soilphys %>%
    dplyr::filter(na_flag == F) %>%
    dplyr::select(prof_id) %>%
    dplyr::distinct() %>%
    dplyr::left_join(profiles)

  gasdata <- profiles %>%
    dplyr::left_join(gasdata)

  soilphys_backup <- soilphys
  soilphys <- as.data.table(soilphys %>%
                              dplyr::select(prof_id,height,DS, rho_air,upper,lower,step_id,pmap))
  gasdata <- as.data.table(gasdata %>%
                             dplyr::select(prof_id,depth,NRESULT_ppm))

  setkey(soilphys,prof_id)
  setkey(gasdata,prof_id)

  #sorting prod_depth and getting lower end of model
  prod_depth <- sort(prod_depth)
  lower_depth <- prod_depth[1]

  #starting values
  prod_start <- rep(0,length(prod_depth)-1)
  if (zero_flux == F){
    prod_start <- c(0,prod_start)
  }
  lowlim_tmp <- rep(lowlim,length(prod_start))
  highlim_tmp <- rep(highlim,length(prod_start))


  n_profs <- nrow(profiles)
  printers <-floor(seq(1,nrow(profiles),length.out = 11))
  printers<-profiles$prof_id[printers]
  print_percent <- seq(0,100,10)

  F0 <- 0


  #for users
  print("started profile fitting. This may take very long. ~30 s/1000 profiles!")
  print(paste(n_profs,"profiles"))
  #per-profile calculation
  proflux<- lapply(profiles$prof_id,function(i){

    if (i %in% printers){
      print(paste0(print_percent[printers == i]," %"))
    }
    #gasdata_tmp <- gasdata[gasdata$prof_id == i,]
    #soilphys_tmp <- soilphys[soilphys$prof_id == i,]

    gasdata_tmp <- gasdata[J(i),]
    soilphys_tmp <- soilphys[J(i),]

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
                            zero_flux=zero_flux
      )
      pars <-(prod_optimised$par)
    },error = function(e) {return(rep(NA, length(prod_start)))})



    if(zero_flux == T){
      prods <- pars
    } else {
      F0 <- pars[1]
      prods <- pars[-1]
    }


    #mapping production to correct steps in soilphys
    prod <-prods[pmap]

    #calculating flux
    fluxs <- prod_mod_flux(prod,height,F0)

    #generating return data_frame
    soilphys_tmp$flux <- fluxs
    soilphys_tmp$F0 <- F0
    soilphys_tmp$prod <-prod

    return(soilphys_tmp)
  })

  df_ret <- dplyr::bind_rows(proflux)
  df_ret <- df_ret %>% dplyr::left_join(soilphys_backup %>%
                                          dplyr::select(-dplyr::any_of(c("flux","prod","F0"))),)
  return(df_ret)
}
