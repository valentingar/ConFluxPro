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
#' @param x An object of class cfp_dat
#'
# @param known_flux (dataframe) a dataframe that gives a known efflux for each
#   profile defined by id_cols. If this is provided, the productions are
#   optimised to meet this flux as well as the concentration measurements
#   provided.
#'
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
#' is characterised by all \code{id_cols} present in the original input as well as the correspongin
#' "gd_id", "sp_id", and "group_id". Each row has a unique identifier "prof_id".}
#' \item{id_cols}{A character vector of all columns that identify a profile uniquely.}
#'}

#' @importFrom dplyr filter
#' @export
# helper
cfp_dat <- function(gasdata,
                    soilphys,
                    layers_map){

  stopifnot("gasdata must be created with cfp_gasdata() first" = inherits(gasdata,"cfp_gasdata"),
            "soilphys must be created with cfp_soilphys() first" = inherits(soilphys,"cfp_soilphys"),
            "layers_map must be created with cfp_layers_map() first" = inherits(layers_map,"cfp_layers_map"))

  message("\nvalidating datasets")
  gasdata <- validate_cfp_gasdata(gasdata)
  soilphys <- validate_cfp_soilphys(soilphys)
  layers_map <- validate_cfp_layers_map(layers_map)

  # get id_cols
  id_cols_list <- lapply(list(gasdata,soilphys,layers_map), cfp_id_cols)
  id_cols <- unlist(id_cols_list) %>% unique()
  message(paste0("id_cols: ", paste0(id_cols, collapse = ", "), collapse = ""))

  stopifnot("All id_cols of layers_map must be present in soilphys!" =
              all(id_cols_list[[3]] %in% id_cols_list[[2]]))

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
    new_cfp_gasdata(id_cols = id_cols_list[[1]])

  soilphys <-
    soilphys %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(id_cols))) %>%
    dplyr::mutate(sp_id = dplyr::cur_group_id()) %>%
    dplyr::arrange(upper) %>%
    dplyr::ungroup() %>%
    new_cfp_soilphys(id_cols = id_cols_list[[2]])


  # calculating profiles
  profiles <-
    gasdata %>%
    select_id_cols(c(id_cols,"gd_id")) %>%
    dplyr::left_join(soilphys %>% select_id_cols(c(id_cols,"sp_id")), by = merger_1) %>%
    dplyr::left_join(layers_map %>% select_id_cols(c(id_cols,"group_id")), by = merger_2) %>%
    dplyr::filter((is.na(sp_id) + is.na(gd_id) + is.na(group_id)) == 0) %>% # only complete profiles
    dplyr::mutate(prof_id = dplyr::row_number()) %>%
    dplyr::distinct() %>%
    as.data.frame()

  soilphys <- soilphys %>%
    filter(sp_id %in% profiles$sp_id) %>%
    new_cfp_soilphys(id_cols = cfp_id_cols(soilphys))

  gasdata <- gasdata %>%
    dplyr::filter(gd_id %in% profiles$gd_id) %>%
    new_cfp_gasdata(id_cols = cfp_id_cols(gasdata))


  message(paste0(nrow(profiles)," unique profiles"))

  # checking if layersmap range = soilphys range
  stopifnot("layers_map and soilphys must have the same max-min upper/lower bounds per group!" = same_range(soilphys,layers_map))


  # splitting soilphys layers to match layers_map and gasdata
  soilphys <- split_soilphys(soilphys,
                             gasdata,
                             layers_map) %>%
    sp_add_pmap(layers_map)



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
    dplyr::rename(umax_x = umax,
           lmin_x = lmin) %>%
    dplyr::left_join(get_upper_lower_range(layers_map),
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

  # get id_cols for joining
  sp_id_cols <- cfp_id_cols(soilphys)
  gd_id_cols <- cfp_id_cols(gasdata)
  lmap_id_cols <- cfp_id_cols(layers_map)

  # which id cols are in both?
  sel_gd <- gd_id_cols[gd_id_cols %in% sp_id_cols]
  sel_lmap <- lmap_id_cols[lmap_id_cols %in% sp_id_cols]
  sel_both <- sel_gd[sel_gd %in% sel_lmap]

  # combine layers_map and gasdata
  all_depths <-
  layers_map[,c(sel_both, "upper")] %>%
  dplyr::rename(depth = upper) %>%
  dplyr::bind_rows(gasdata[,c(sel_both, "depth")])

  # add a single grouping variable
  groups_map <-
    soilphys %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(sel_both))) %>%
    dplyr::summarise(depth_group = dplyr::cur_group_id())

  all_depths <- all_depths %>%
    dplyr::right_join(groups_map, by = sel_both) %>%
    dplyr::distinct()

  #add row_number
  merger <- names(groups_map)[names(groups_map) %in% names(soilphys)]
  soilphys <-
    soilphys %>%
    dplyr::left_join(groups_map, merger) %>%
    dplyr::mutate(row_id = dplyr::row_number())

  sp_dist <-
  soilphys %>%
    dplyr::select(depth_group,upper,lower) %>%
    dplyr::distinct()

  soilphys_new <-
    mapply(sp_dist$upper,
           sp_dist$lower,
           sp_dist$depth_group,
           FUN = add_between,
           MoreArgs = list(df = all_depths),
           SIMPLIFY = FALSE) %>%
    do.call(what = rbind, args = .) %>%
    data.frame() %>%
    setNames(c("upper_new","lower_new","upper","lower","depth_group")) %>%
    dplyr::left_join(soilphys,
                     by = c("upper","lower","depth_group")) %>%
    dplyr::mutate(upper = upper_new,
                  lower = lower_new) %>%
    dplyr::select(!dplyr::any_of(c("upper_new","lower_new"))) %>%
    dplyr::mutate(height = (upper-lower)/100) %>%
    dplyr::group_by(sp_id) %>%
    dplyr::arrange(upper) %>%
    dplyr::mutate(step_id = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    new_cfp_soilphys(id_cols = sp_id_cols)
}

add_between <- function(upper,
                        lower,
                        depth_group,
                        df){

  depths <- sort(
    unique(
      df$depth[df$depth < upper &
                 df$depth > lower &
                 df$depth_group == depth_group]
    )
  )

  upper_new <- c(depths, upper)
  lower_new <- c(lower, depths)

  l <- length(upper_new)

  matrix(c(upper_new,lower_new,rep(upper, l),rep(lower, l),rep(depth_group, l)), ncol = 5)
}



sp_add_pmap <- function(soilphys,
                        layers_map){

  id_cols_sp <- cfp_id_cols(soilphys)
  id_cols_lmap <- cfp_id_cols(layers_map)
  merger <- id_cols_lmap[id_cols_lmap %in% id_cols_sp]

  soilphys <-
  soilphys %>%
    dplyr::select(dplyr::any_of(c(merger, "upper", "lower"))) %>%
    dplyr::distinct() %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(merger))) %>%
    dplyr::group_modify(~{

      lmap <- .y %>% dplyr::left_join(layers_map,
                                      by = names(.y))

      .x$pmap <- sapply(1:nrow(.x), function(i){
        lmap$pmap[.x$upper[i] <= lmap$upper & .x$lower[i] >= lmap$lower]
      })
      .x
    }) %>%
      dplyr::left_join(soilphys, by = c(merger, "upper", "lower")) %>%
    dplyr::ungroup() %>%
    new_cfp_soilphys(id_cols = c(id_cols_sp))

}



# methods  -------------------

##### PRINTING #####
#' @exportS3Method
print.cfp_dat <- function(x, ...){
  cat("\nA cfp_dat object to be used as input in ConFluxPro models. \n")
  id_cols <- cfp_id_cols(x)
  cat("id_cols:", id_cols, "\n")
  n_profs <- x$profiles %>% nrow()
  n_groups <- x$layers_map %>% dplyr::pull(group_id) %>% unique() %>% length()
  cat("number of profiles: ", n_profs, "\n")
  cat("number of groups: ", n_groups, "\n")
  cat("\n")
}


###### EXTRACTION #####
#' @describeIn extractors id_cols
#' @export
cfp_id_cols <- function(x){
  UseMethod("cfp_id_cols")
}
#' @export
cfp_id_cols.default <- function(x){
  attr(x,"id_cols")
}


##### COERSION #######
#' @describeIn coercion to cfp_dat
#' @export
as_cfp_dat <- function(x){
  UseMethod("as_cfp_dat")
}

#' @exportS3Method
as_cfp_dat.cfp_dat <- function(x){
  x <-
    new_cfp_dat(x$gasdata,
              x$soilphys,
              x$layers_map,
              x$profiles,
              cfp_id_cols(x))
  x
}

##### FILTER ######
#' @exportS3Method
filter.cfp_dat <- function(.data,
                           ...,
                           .preserve = FALSE){
  tables <- names(.data)
  tables <- tables[tables == "profiles"]

  .data$profiles <- .data$profiles %>%
    dplyr::filter(...)

  possible_cols <- names(.data$profiles)

  out <-
    lapply(.data, function(t){

      col_names <- names(t)
      merger <- col_names[col_names %in% possible_cols]
      deselector <- possible_cols[!possible_cols %in% merger]

      t_new <-
      t %>%
        dplyr::right_join(.data$profiles %>%
                            dplyr::select({merger}) %>%
                            dplyr::distinct(),
                          by = merger) %>%
        dplyr::select(!dplyr::any_of(deselector))

      old_atr <- attributes(t)
      new_atr <- old_atr[!names(old_atr) %in% names(attributes(data.frame()))]
      attributes(t_new) <- c(attributes(t_new), new_atr)
      class(t_new) <- class(t)
      t_new
    })

  attributes(out) <- attributes(.data)

  out
}



##### SPLITTING #####
#' @rdname cfp_dat
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
      new_cfp_soilphys(id_cols = cfp_id_cols(x$soilphys))

    gd <-
      x$gasdata[x$gasdata$gd_id %in% profs_tmp$gd_id,] %>%
      new_cfp_gasdata(id_cols = cfp_id_cols(x$gasdata))

    lmap <-
      x$layers_map[x$layers_map$group_id %in% profs_tmp$group_id,] %>%
      cfp_layers_map(id_cols = cfp_id_cols(x$layers_map))

    cfp_dat_group <- new_cfp_dat(gd,sp,lmap,profs_tmp,id_cols = cfp_id_cols(x))
    attributes(cfp_dat_group) <- attributes(x)
    cfp_dat_group
  })
  out
}

#' @rdname cfp_dat
#' @export
split_by_prof <- function(x){
  UseMethod("split_by_prof")
}

#' @exportS3Method
split_by_prof.cfp_dat <- function(x){

  profiles <- x$profiles

  soilphys <-
  profiles %>%
    dplyr::select(sp_id, prof_id) %>%
    dplyr::left_join(x$soilphys, by = "sp_id")

  gasdata <-
    profiles %>%
    dplyr::select(gd_id, prof_id) %>%
    dplyr::left_join(x$gasdata, by = "gd_id")

  lmap <- x$layers_map


  atts <- attributes(x)


  out <-
    mapply(split(profiles, profiles$prof_id),
           split(soilphys, soilphys$prof_id),
           split(gasdata, gasdata$prof_id),
           MoreArgs = list(
           lmap = lmap,
           id_cols = cfp_id_cols(x),
           atts = list(atts)),
           FUN = function(profs,
                    sp,
                    gd,
                    lmap,
                    id_cols,
                    atts){
      cfp_dat_group <- new_cfp_dat(gd[!colnames(gd) == "prof_id"],
                                   sp[!colnames(sp) == "prof_id"],
                                   lmap,
                                   profs,
                                   id_cols = id_cols)
      attributes(cfp_dat_group) <- unlist(atts, recursive = FALSE)
      cfp_dat_group
    },
    SIMPLIFY = FALSE)
  out
}







