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
