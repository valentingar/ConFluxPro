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
                     target_depth,
                     storage_term = F,
                     zero_flux = T){

  #filtering out problematic measurements
  gasdata <- gasdata %>% filter(!is.na(depth),
                                !is.na(gas),
                                !is.na(Plot))


  well_depths <- gasdata %>% dplyr::group_by(Plot,gas) %>%
    dplyr::select(Plot,depth) %>%
    dplyr::distinct() %>%
    dplyr::rename(well_depth = depth)

  #something to split soilphys to have well depths at edges.
  discretize_depth(soilphys_complete %>%
                     dplyr::mutate(rho_air = p*100 / (8.314 * (273.15+Temp))),
                   param = c("DS","rho_air"), #vielleicht versuchen, dass alle mitgenommen werden?? nötig?
                   method = "boundary",
                   depth_target = #dataframe:combined interfaces soilphys+gasdata,
                   control=list(boundary_nearest =F),
                   id_cols = c("Plot","gas","Date"))


  #group by Plot Date and gas
  soilphys %>%
    dplyr::mutate(rho_air = p*100 / (8.314 * (273.15+Temp))) %>%
    group_by(Plot,Date,gas)



  gdf <- gasdata_o2offset %>%
    filter(Plot == "ES_Fi",gas == "CO2",is.na(NRESULT_ppm)==F) %>%
    group_by(Date) %>% mutate(n = cur_group_id()) %>% filter(n ==200)

  ggplot(gdf,aes(y=depth,x=NRESULT_ppm))+geom_point()

  d <- gdf$Date[1]

  spdf <- soilphys_complete %>% filter(Plot == "ES_Fi",
                                       Date == d,
                                       gas == "CO2") %>% arrange(depth)
  spdf <- spdf %>%
    tibble::rowid_to_column("n") %>%
    dplyr::mutate(rho_air = p*100 / (8.314 * (273.15+Temp)))

  pmap <- c(1,1,2,2,3,3)
  height <- (spdf$upper-spdf$lower)/100
  cmap <- unlist(lapply(gdf$depth,function(g) ifelse(g %in% spdf$upper,spdf$n[spdf$upper==g],NA)))

  C0 <- median(gdf %>% filter(depth == -10) %>% pull(NRESULT_ppm))*spdf$rho_air[spdf$lower==-10]
  conc <- gdf$NRESULT_ppm * spdf$rho_air[cmap]
  dstor <-0
  prod_optimised<-optim(par=c(0,0,0,0),
                        fn = prod_optim,
                        lower = rep(0,4),
                        upper = rep(20000,4),
                        method = "L-BFGS-B",
                        pmap = pmap,
                        cmap = cmap,
                        conc = conc,
                        dstor = dstor,
                        C0 = C0,
                        zero_flux=F
  )

  prod_optimised$par

  prod_mod_flux(prod_optimised$par[-1],height,prod_optimised$par[1])

}
