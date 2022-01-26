#' @title input_classes
#'
#' @description This provides the framework for multiple input data classes that
#' make it easier to set up a ConFluxPro model by checking for data integrity first.
#'
#' @param x sosso
#'
#' @export
# helper
cfp_gasdata <- function(gasdata,
                        id_cols){


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("added 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <- new_gdat(gasdata,
                id_cols)

  validate_cfp_gasdata(x)
}

#'
# constructor
new_gdat <- function(gasdata,
                     id_cols){

  structure(gasdata,
            class = c("cfp_gasdata","data.frame"),
            id_cols = id_cols)
}


#'
# validator
validate_cfp_gasdata <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_gasdata"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("NRESULT_ppm","gas","depth")
  id_cols <- cfp_id_cols(x)

  stopifnot("data.frame lacks obligatory coluns" = base_cols %in% names(x),
            "id_cols must be present in the data.frame" = id_cols %in% names(x)
  )

  #check for NAs in id_cols
  stopifnot("id_cols cannot contain NAs" =
    anyNA(x[id_cols]) == FALSE)

  #check that at least two depths per group are present
  problem_groups <-
  x %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::summarise(n_depths = length(unique(depth[!is.na(NRESULT_ppm)]))) %>%
    dplyr::filter(n_depths < 2)

  stopifnot("There are combinations of id_cols with less than 2 non-NA depths" =
              nrow(problem_groups) == 0 )

  x
}



#' @export
#helper
cfp_soilphys <- function(soilphys,
                         id_cols){


  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("added 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  x <- new_cfp_soilphys(soilphys,
                        id_cols
                        )

  x <- validate_cfp_soilphys(x)
}

#constructor
new_cfp_soilphys <- function(soilphys,
                      id_cols){
  x <- structure(soilphys,
                 class = c("cfp_soilphys","data.frame"),
                 id_cols = id_cols)
  x
}

#validator
validate_cfp_soilphys <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_soilphys"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("upper","lower","DS","rho_air","gas")
  id_cols <- cfp_id_cols(x)

  stopifnot("data.frame lacks obligatory coluns" = base_cols %in% names(x),
            "id_cols must be present in the data.frame" = id_cols %in% names(x)
  )

  # is the data frame upper/lower consistent?
  stopifnot("The data is not unique and upper/lower consistent!" = is_ul_consistent(x,id_cols))

  x
}



#' @export
cfp_layers_map <- function(layers_map,
                           id_cols,
                           gas = NULL,
                           lowlim = NULL,
                           highlim = NULL,
                           layer_couple = NULL
                           ){

  stopifnot("layers_map must be a data frame!" = is.data.frame(layers_map))
  stopifnot("id_cols must be provided!" = !missing(id_cols))

  if (!"gas" %in% id_cols){
    message("added 'gas' to id_cols")
    id_cols <- c(id_cols,"gas")
  }

  #convenient way of adding multiple gases to layers_map
  if (!is.null(gas)){

    stopifnot("gas must not be present in layers_map already!" = !"gas" %in% names(layers_map))
    stopifnot("gas must be a character (-vector)!" = is.character(gas))
    stopifnot("gas must contain unique values only!" = length(gas) == length(unique(gas)))

    layers_map <-
      lapply(gas, function(i){
        layers_map$gas <- i
        layers_map
      }) %>%
      dplyr::bind_rows()
  }

  layers_map <- add_if_missing(layers_map,
                               gas,
                               lowlim = lowlim)

  layers_map <- add_if_missing(layers_map,
                               gas,
                               highlim = highlim)
  layers_map <- add_if_missing(layers_map,
                               gas,
                               layer_couple = layer_couple)


  #automated adding of "layer" column
  if(!"layer" %in% names(layers_map)){
    layers_map <-
      layers_map %>%
      dplyr::arrange(desc(upper)) %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
      dplyr::mutate(layer = 1:n())

    message("automatically added 'layer' column")
  }

  x <- new_cfp_layers_map(layers_map,
                          id_cols)

  x <- validate_cfp_layers_map(x)

  x
}


#constructor
new_cfp_layers_map <- function(layers_map,
                               id_cols){

  x <- structure(layers_map,
                 id_cols = id_cols,
                 class = c("cfp_layers_map","data.frame"))
  x
}

#validator
validate_cfp_layers_map <- function(x){

  # are the classes correct?
  stopifnot(inherits(x,"cfp_layers_map"))
  stopifnot(inherits(x,"data.frame"))

  # are the necessary columns present?
  base_cols <- c("upper","lower","layer","lowlim","highlim","layer_couple","gas")
  id_cols <- cfp_id_cols(x)

  error_if_missing(x, c(base_cols,id_cols))

  # is the data.frame upper/lower consistent?
  stopifnot("layers_map must be unique and upper/lower consitent" =
              is_ul_consistent(x,id_cols = cfp_id_cols(x)))

  x

}


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
    dplyr::mutate(group_id = cur_group_id()) %>%
    cfp_layers_map(id_cols = id_cols_list[[3]])

  gasdata <-
    gasdata %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(gd_id = cur_group_id()) %>%
    cfp_gasdata(id_cols = id_cols_list[[1]])

  soilphys <-
    soilphys %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(sp_id = cur_group_id()) %>%
    cfp_soilphys(id_cols = id_cols_list[[2]])


  # calculating profiles
  profiles <-
    gasdata %>%
    select_id_cols(id_cols) %>%
    dplyr::left_join(soilphys %>% select_id_cols(id_cols), by = merger_1) %>%
    dplyr::left_join(layers_map %>% select_id_cols(id_cols), by = merger_2) %>%
    dplyr::mutate(prof_id = dplyr::row_number()) %>%
    dplyr::left_join(layers_map[,c(id_cols_list[[3]],"group_id")],
                     by = merger_2) %>%
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

add_if_missing  <- function(df,
                            gas,
                            ...){


  obj_list <- list(...)
  stopifnot(length(obj_list) == 1)
  obj_name <- names(obj_list)[1]
  obj <- obj_list[[1]]

  if (!is.null(obj)){
    if(obj_name %in% names(df)){
      stop(paste0(obj_name," must not be present in layers_map already!"))
    }
    if(!(length(obj) == length(gas)  |  length(obj) == 1)){
      stop(paste0(obj_name," must be length 1 or the same as gas"))
    }
    stopifnot(is.numeric(obj))

    if (length(obj) == 1){
      df[[obj_name]] <- obj
    } else {
      df <-
        lapply(seq_along(gas), function(i){
          df_part <- df[df$gas == gas[i],]
          df_part[[obj_name]] <- obj[i]
          df_part
        }) %>%
        dplyr::bind_rows()
    }
  } else {
    # columns must still be present but are NA instead
    df[[obj_name]] <- NA
  }

  df
}

error_if_missing <- function(df,cols){
  lapply(cols, function(i) if(!i %in% names(df)) stop(paste0(i, " must be present in layers_map!")))
}

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
#' @exportS3Method
print.cfp_soilphys <- function(x){
  cat("\nA cfp_soilphys object \n")
  print_id_cols(x)
  cat("\n")
  NextMethod()
}

#' @exportS3Method
print.cfp_gasdata <- function(x){
  cat("\nA cfp_gasdata object \n")
  print_id_cols(x)
  cat("\n")
  NextMethod()
}

#' @exportS3Method
print.cfp_layers_map <- function(x){
  cat("\nA cfp_layers_map object \n")
  id_cols <- cfp_id_cols(x)
  cat("id_cols:", id_cols, "\n")
  cat("\n")
  NextMethod()
}

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



print_id_cols <- function(x){
  id_cols <- cfp_id_cols(x)
  unique_groups <- x[id_cols] %>% dplyr::distinct() %>% nrow()
  cat("id_cols:", id_cols, "\n")
  cat(unique_groups, " unique profiles", "\n")
}









