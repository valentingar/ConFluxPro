#' @title pro_flux
#'
#' @description This implements an inverse modeling approach which optimizes vertically resolved production (or consumption)
#' of the gases in question to fit a modeled concentration profile to observed data.
#'
#' @param gasdata (dataframe)
#' @param soilphys (dataframe)
#'
#' @param target_depth (numeric vector) This vector determines the depths of the interfaces between
#' different production values to be fitted. This allows for an adaptive model with an individual number variables. So for three different production values, two depths that mark the intersection must be given.
#' @param storage_term (logical) Should changes in storage be accounted for? Default is F.
#' Only works if data is present in a temporal dimension as well and is probably only representative for a high temporal resolution (hours).
#' @param zero_flux (logical) Applies the zero-flux boundary condition (T)? If FALSE, the first value in X
#' represents the incoming flux to the lowest layer.
#'
#' @examples pro_flux(gasdata,
#' soilphys,
#' target_depth,
#' storage_term = F,
#' zero_flux = T)
#'
#' @import dplyr
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
    dplyr::distinct()

  #select relevant profiles from soilphys

  soilphys <- profiles %>% dplyr::left_join(soilphys)


  #something to split soilphys to have well depths at edges.
  soilphys <-discretize_depth(soilphys,
                              param = sp_names,
                              method = "boundary",
                              depth_target = target_depths,
                              boundary_nearest =F,
                              id_cols = c("Plot","gas","Date"))





  #per-profile calculation
  proflux<- lapply(1:nrow(profiles),function(i){

    prof <- profiles[i,]

    print(paste(prof,collapse = " | "))


  gasdata_tmp <-  suppressMessages(prof %>% dplyr::left_join(gasdata))
  soilphys_tmp <- suppressMessages(prof %>% dplyr::left_join(soilphys))

  #sorting prod_depth and getting lower end of model
  prod_depth <- sort(prod_depth)
  lower_depth <- prod_depth[1]

  #arranging soilphys_tmp and creating id of steps
  soilphys_tmp <- soilphys_tmp %>%
    dplyr::arrange(upper) %>%
    dplyr::mutate(step_id = dplyr::row_number())

  if (anyNA(soilphys_tmp %>% ungroup() %>%select(upper,lower,DS,rho_air))){
    return(NULL)
  }


  #mapping productions to soilphys_tmp
  pmap <- findInterval(soilphys_tmp$depth,prod_depth)

  #calculating height of each step in m
  height <- (soilphys_tmp$upper-soilphys_tmp$lower)/100

  #mapping measured concentrations to soilphys_tmp
  cmap <- unlist(lapply(gasdata_tmp$depth,function(g) ifelse(g %in% soilphys_tmp$upper,soilphys_tmp$step_id[soilphys_tmp$upper==g],NA)))

  #C0 at lower end of production model
  C0 <- median(gasdata_tmp %>%
                 dplyr::filter(depth == lower_depth) %>%
                 dplyr::pull(NRESULT_ppm))*soilphys_tmp$rho_air[soilphys_tmp$lower==lower_depth]

  #from ppm to mumol/m^3
  conc <- gasdata_tmp$NRESULT_ppm * soilphys_tmp$rho_air[cmap]

  #storage term
  dstor <-0

  #starting values
  prod_start <- rep(0,length(prod_depth)-1)
  if (zero_flux == F){
    prod_start <- c(0,prod_start)
  }
  lowlim_tmp <- rep(lowlim,length(prod_start))
  highlim_tmp <- rep(highlim,length(prod_start))


  F0 <- 0
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

df_ret <- bind_rows(proflux)
return(df_ret)
}
