#' @title cfp_dat
#'
#' @description This is the essential object class that binds all necessary
#' input data to run a ConFluxPro model. It automatically combines the different
#' datasets and checks them for validity. It may split soilphys layers to correspond
#' with layers_map and gasdata depths.
#'
#' @param gasdata A cfp_gasdata object created by running \code{cfp_gasdata()}.
#' @param soilphys A cfp_soilphys object created by running \code{cfp_soilphys()}.
#' @param layers_map A cfp_layers_map object created by running \code{cfp_layers_map}.
#'
#' @return A cfp_dat object with the following parameters:
#' \describe{
#' \item{gasdata}{The gasdata object with added column "gd_id" that is unique for each profile.}
#' \item{soilphys}{The soilphys object with added columns "sp_id" that is unique for each profile,
#' "step_id" indicating the position of each step from the bottom up, "height" in m of each layer,
#' "pmap" indicating which layer it belongs to from the bottom up. Potentially, some original
#' steps were split to account for the depths within gasdata or layers_map.}
#' \item{layers_map}{The layers_map object with added column "group_id" indicating each
#' unique group of the same layer parameterization set by layers_map.}
#' \item{profiles}{A \code{data.frame} where each row indicates one unique profile that
#' is characterised by all \cdoe{id_cols} present in the original input as well as the correspongin
#' "gd_id", "sp_id", and "group_id". Each row has a unique identifier "prof_id".}
#' \item{id_cols}{A character vector of all columns that identify a profile uniquely.}
#'}


#' @export
# helper
cfp_dat <- function(gasdata,
                    soilphys,
                    layers_map){

  stopifnot("gasdata must be created with cfp_gasdata() first" = inherits(gasdata,"cfp_gasdata"),
            "soilphys must be created with cfp_soilphys() first" = inherits(soilphys,"cfp_soilphys"),
            "layers_map must be created with cfp_layers_map() first" = inherits(layers_map,"cfp_layers_map"))

  message("validating datasets")
  gasdata <- validate_cfp_gasdata(gasdata)
  soilphys <- validate_cfp_soilphys(soilphys)
  layers_map <- validate_cfp_layers_map(layers_map)

  # get id_cols
  id_cols_list <- lapply(list(gasdata,soilphys,layers_map), cfp_id_cols)
  id_cols <- unlist(id_cols_list) %>% unique()
  message(paste0("id_cols: ", paste0(id_cols, collapse = ", "), collapse = ""))

  merger_1 <- whats_in_both(id_cols_list[c(1,2)])
  merger_2 <- list(unlist(id_cols_list[c(1,2)][[1]]),
                   id_cols_list[[3]])%>%
    whats_in_both()


  # create group_id in layers_map, gd_id in gasdata and sp_id in soilphys
  layers_map <-
  layers_map %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(group_id = dplyr::cur_group_id()) %>%
    cfp_layers_map(id_cols = id_cols_list[[3]])

  gasdata <-
    gasdata %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(gd_id = dplyr::cur_group_id()) %>%
    cfp_gasdata(id_cols = id_cols_list[[1]])

  soilphys <-
    soilphys %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(sp_id = dplyr::cur_group_id()) %>%
    dplyr::arrange(upper) %>%
    dplyr::mutate(pmap = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    cfp_soilphys(id_cols = id_cols_list[[2]])


  # calculating profiles
  profiles <-
    gasdata %>%
    select_id_cols(c(id_cols,"gd_id")) %>%
    dplyr::left_join(soilphys %>% select_id_cols(c(id_cols,"sp_id")), by = merger_1) %>%
    dplyr::left_join(layers_map %>% select_id_cols(c(id_cols,"group_id")), by = merger_2) %>%
    dplyr::mutate(prof_id = dplyr::row_number()) %>%
    dplyr::distinct() %>%
    as.data.frame()


  message(paste0(nrow(profiles)," unique profiles"))

  # checking if layersmap range = soilphys range
  stopifnot("layers_map and soilphys must have the same max-min upper/lower bounds per group!" = same_range(soilphys,layers_map))


  # splitting soilphys layers to match layers_map and gasdata
  soilphys <- split_soilphys(soilphys,
                             gasdata,
                             layers_map)



  x <- new_cfp_dat(gasdata,
                   soilphys,
                   layers_map,
                   profiles,
                   id_cols)

  x <- validate_cfp_dat(x)
  x
}

#constructor
new_cfp_dat <- function(gasdata,
                        soilphys,
                        layers_map,
                        profiles,
                        id_cols){

  x <- structure(list(profiles = profiles,
                      gasdata = gasdata,
                      soilphys = soilphys,
                      layers_map = layers_map),
                 id_cols = id_cols,
                 class = c("cfp_dat","list"))

}

#validator
validate_cfp_dat <- function(x){

  stopifnot(inherits(x,"cfp_dat"))

  x
}




# helpers -------

select_id_cols <- function(df,id_cols){
  df <- df %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::any_of(id_cols)) %>%
    dplyr::distinct()

  df

}

whats_in_both <- function(l){
  stopifnot(length(l) == 2)
  l[[1]][l[[1]] %in% l[[2]]]
}

same_range <- function(soilphys,
                       layers_map){

  sp_summ <- get_upper_lower_range(soilphys) %>%
    rename(umax_x = umax,
           lmin_x = lmin) %>%
    left_join(get_upper_lower_range(layers_map),
              by = whats_in_both(list(cfp_id_cols(layers_map),cfp_id_cols(soilphys)))
    )

  all((sp_summ$umax_x == sp_summ$umax) & (sp_summ$lmin_x == sp_summ$lmin))
}

get_upper_lower_range <- function(x){
  id_cols <- cfp_id_cols(x)

  x_sum <-
  x %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::summarise(umax = max(upper),
                     lmin = min(lower))
  x_sum
}


split_soilphys <- function(soilphys,
                           gasdata,
                           layers_map){

  #get id_cols for joining
  sp_id_cols <- cfp_id_cols(soilphys)
  gd_id_cols <- cfp_id_cols(gasdata)
  lmap_id_cols <- cfp_id_cols(layers_map)

  merger_1 <- whats_in_both(list(lmap_id_cols,gd_id_cols)) #to join gd to sp
  merger_2 <- whats_in_both(list(unique(c(lmap_id_cols,gd_id_cols)), # to join lmap to both
                                 sp_id_cols))


  sp_bare <-
  soilphys %>%
    dplyr::arrange(lower) %>%
    dplyr::mutate(row_id = dplyr::row_number())

  gd_bare <-
    gasdata %>%
    dplyr::select(dplyr::any_of(c(gd_id_cols,"depth","prof_id"))) %>%
    dplyr::distinct()

  lmap_bare <-
    layers_map %>%
    dplyr::select(dplyr::any_of(c(lmap_id_cols, "upper","lower"))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(lmap_id_cols))) %>%
    dplyr::summarise(depth = c(upper,lower))


  all_depths <- gd_bare %>%
    dplyr::bind_rows(lmap_bare) %>%
    dplyr::select(dplyr::any_of(c(sp_id_cols,"depth"))) %>%
    dplyr::distinct()


  sp_out <-
    sp_bare %>%
      dplyr::select(dplyr::any_of(c(sp_id_cols, "upper","lower","row_id"))) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(sp_id_cols))) %>%
      dplyr::group_modify(~add_depths(.x,
                                      .y,
                                      all_depths,
                                      id_cols = merger_2
                                      )) %>%
    dplyr::ungroup() %>%
      dplyr::left_join(sp_bare %>% dplyr::select(!dplyr::any_of(c("upper","lower"))),
                       by = c(sp_id_cols,"row_id")) %>%
      dplyr::select(!row_id) %>%
    dplyr::mutate(height = (upper-lower)/100) %>%
    dplyr::group_by(sp_id) %>%
    dplyr::arrange(upper) %>%
    dplyr::mutate(step_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    cfp_soilphys(id_cols = c(sp_id_cols))

    sp_out
}


# splitting soilphys so that the each slice is homogenous
# and the gas measurements are at the intersections
add_depths <- function(.x,
                       .y,
                       all_depths,
                       id_cols){

  #getting depths of gasdata at that plot
  more_depths <-
    all_depths %>%
    dplyr::right_join(.y,by = id_cols) %>%
    dplyr::pull(depth) %>%
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
  depths <- sort(unique(c(s_depths, more_depths)))

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
    dplyr::mutate(k_count = unlist(lapply(k_id,function(i) length(which(!!k_ind %in% i) == T)))+1)

  #expanding to final map and adding the correct boundaries
  k_map_n <- k_map %>%
    tidyr::uncount(k_count) %>%
    dplyr::mutate(lower = !!depths[-length(!!depths)],
                  upper = !!depths[-1])

  #final product
  .x <- .x %>%
    dplyr::left_join(k_map, by = c("upper","lower")) %>%
    dplyr::select(!dplyr::any_of(c("upper","lower")))%>%
    dplyr::left_join(k_map_n, by = "k_id") %>%
    dplyr::select(!dplyr::any_of({c("k_id","k_count")}))

  return(.x)
}




# methods  -------------------

# PRINTING
#' @exportS3Method
print.cfp_dat <- function(x){
  cat("\nA cfp_dat object to be used as input in ConFluxPro models \n \n")
  id_cols <- cfp_id_cols(x)
  cat("id_cols:", id_cols, "\n")
  n_profs <- cfp_get_profiles(x) %>% nrow()
  n_groups <- cfp_get_layers_map(x) %>% dplyr::pull(group_id) %>% unique() %>% length()
  cat("number of profiles: ", n_profs, "\n")
  cat("number of groups: ", n_groups, "\n")
  cat("\n")
}


# SPLITTING
#' @export
split_by_group <- function(x){
  UseMethod("split_by_group")
}

#' @exportS3Method
split_by_group.cfp_dat <- function(x){

  profiles <- x$profiles
  groups <- unique(profiles$group_id)

  out <-
  lapply(groups, function(group_tmp){

    profs_tmp <- profiles[profiles$group_id == group_tmp,]

    sp <-
    x$soilphys[x$soilphys$sp_id %in% profs_tmp$sp_id,] %>%
      cfp_soilphys(id_cols = cfp_id_cols(x$soilphys))

    gd <-
      x$gasdata[x$gasdata$gd_id %in% profs_tmp$gd_id,] %>%
      cfp_gasdata(id_cols = cfp_id_cols(x$gasdata))

    lmap <-
      x$layers_map[x$layers_map$group_id %in% profs_tmp$group_id,] %>%
      cfp_layers_map(id_cols = cfp_id_cols(x$layers_map))

    cfp_dat_group <- new_cfp_dat(gd,sp,lmap,profs_tmp,id_cols = cfp_id_cols(x))
  })

}








