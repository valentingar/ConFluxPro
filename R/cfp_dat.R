#' @title Model input data
#'
#' @description `cfp_dat` is the essential object class that binds all necessary
#'   input data to run a ConFluxPro model. It automatically combines the
#'   different datasets and checks them for validity. It may split soilphys
#'   layers to correspond with layers_map and gasdata depths.
#'
#' @param gasdata A cfp_gasdata object created by running \code{cfp_gasdata()}.
#' @param soilphys A cfp_soilphys object created by running
#'   \code{cfp_soilphys()}.
#' @param layers_map A cfp_layers_map object created by running
#'   \code{cfp_layers_map}.
#'
#' @param x An object of class cfp_dat
#'
# @param known_flux (dataframe) a dataframe that gives a known efflux for each
#   profile defined by id_cols. If this is provided, the productions are
#   optimised to meet this flux as well as the concentration measurements
#   provided.
#'
#' @family data formats
#'
#'@returns A cfp_dat object with the following parameters:
#' \describe{
#' \item{gasdata}{The gasdata object with added column "gd_id" that is unique
#' for each profile.}
#' \item{soilphys}{The soilphys object with added columns "sp_id" that is unique
#'  for each profile, "step_id" indicating the position of each step from the
#'  bottom up, "height" in m of each layer, "pmap" indicating which layer it
#'  belongs to from the bottom up. Potentially, some original
#' steps were split to account for the depths within gasdata or layers_map.}
#' \item{layers_map}{The layers_map object with added column "group_id"
#' indicating each unique group of the same layer parameterization set
#' by layers_map.}
#' \item{profiles}{A \code{data.frame} where each row indicates one unique
#' profile that is characterised by all \code{id_cols} present in the original
#' input as well as the corresponding "gd_id", "sp_id", and "group_id". Each
#' row has a unique identifier "prof_id".}
#'  \item{id_cols}{A character vector of all columns that identify a
#'  profile uniquely.}
#'}
#'
#'
#' @examples
#' gasdata <- cfp_gasdata(
#'   ConFluxPro::gasdata,
#'   id_cols = c("site", "Date"))
#' soilphys <- cfp_soilphys(
#'   ConFluxPro::soilphys,
#'   id_cols = c("site", "Date"))
#' layers_map <-
#'  cfp_layers_map(
#'    ConFluxPro::layers_map,
#'    gas = "CO2",
#'    lowlim = 0,
#'    highlim = 1000,
#'    id_cols = "site")
#' base_dat <- cfp_dat(gasdata, soilphys, layers_map)
#'
#' ### filter similar to dplyr::fliter
#' filter(base_dat, site == "site_a")
#' filter(base_dat, prof_id %in%  1:5)
#'
#' ### coersion from derived objects
#' PROFLUX <- pro_flux(base_dat)
#' as_cfp_dat(PROFLUX)
#'

#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @export
cfp_dat <- function(
    gasdata,
    soilphys,
    layers_map){

  stopifnot("gasdata must be created with cfp_gasdata() first" =
              inherits(gasdata,"cfp_gasdata"),
            "soilphys must be created with cfp_soilphys() first" =
              inherits(soilphys,"cfp_soilphys"),
            "layers_map must be created with cfp_layers_map() first" =
              inherits(layers_map,"cfp_layers_map"))

  message("\nvalidating datasets")
  gasdata <- validate_cfp_gasdata(gasdata)
  soilphys <- validate_cfp_soilphys(soilphys)
  layers_map <- validate_cfp_layers_map(layers_map)

  # get id_cols
  id_cols_list <- lapply(list(gasdata,soilphys,layers_map), cfp_id_cols)
  names_list <- lapply(list(gasdata,soilphys,layers_map), names)
  normal_cols_list <- mapply(id_cols_list,
                             names_list,
                             FUN = function(x,y) y[!y %in% x])
  id_cols <- unlist(id_cols_list) %>% unique()

  if(any(unlist(normal_cols_list) %in% id_cols)){
    problem_cols <- lapply(normal_cols_list, function(x) x[x %in% id_cols])
    problem_datasets <- vapply(problem_cols, length, FUN.VALUE = integer(1))
    problem_message <-
    mapply(problem_cols[problem_datasets],
           c("gasdata", "soilphys", "layers_map")[problem_datasets],
           FUN = function(x,y) paste0(paste0(paste0('"',x,'"'),
                                             collapse = ", "), " in ", y)) %>%
      paste0(collapse = " and ")
    stop("id_col of one dataset cannot be a non-id_col in another!\n",
         "remove/rename non-id_cols: ",
         problem_message)
  }


  stopifnot("All id_cols of layers_map must be present in soilphys!" =
              all(id_cols_list[[3]] %in% id_cols_list[[2]]))

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
    dplyr::left_join(soilphys %>%
                       select_id_cols(c(id_cols,"sp_id")), by = merger_1) %>%
    dplyr::left_join(layers_map %>%
                       select_id_cols(c(id_cols,"group_id")), by = merger_2) %>%
    dplyr::filter( # only complete profiles
      (is.na(sp_id) + is.na(gd_id) + is.na(group_id)) == 0) %>%
    dplyr::mutate(prof_id = dplyr::row_number()) %>%
    dplyr::distinct() %>%
    as.data.frame()

  profiles_insufficient_gasdata <-
    profiles %>%
    dplyr::left_join(gasdata, by = c(cfp_id_cols(gasdata), "gd_id"),
                     relationship = "many-to-many") %>%
    dplyr::left_join(layers_map,
                     by = c(cfp_id_cols(layers_map),"group_id"),
                     relationship = "many-to-many") %>%
    dplyr::filter(!is.na(depth),
                  !is.na(x_ppm)) %>%
    dplyr::filter(depth >= lower,
                  depth <= upper) %>%
    dplyr::filter(
      (is.na(gd_id) + is.na(depth) + is.na(pmap) + is.na(group_id)) == 0) %>%
    dplyr::group_by(group_id, gd_id, pmap) %>%
    dplyr::summarise(n_depths = length(unique(depth))) %>%
    dplyr::mutate(
      n_depths = ifelse(.data$n_depths == 1, NA, .data$n_depths)) %>%
    dplyr::right_join(profiles %>%
                        dplyr::left_join(layers_map,
                                  by = c(cfp_id_cols(layers_map), "group_id"),
                                  relationship = "many-to-many"),
                    by = c("gd_id", "group_id", "pmap")) %>%
  dplyr::group_by(prof_id) %>%
    dplyr::filter(anyNA(.data$n_depths)) %>%
    dplyr::pull(prof_id)

  profiles <-
    profiles %>%
    dplyr::filter(!prof_id %in% profiles_insufficient_gasdata) %>%
    data.frame() %>%
    cfp_profile(id_cols = "prof_id")

  stopifnot("No valid profiles! Maybe the input data dont match?" =
              nrow(profiles) > 0)

  soilphys <- soilphys %>%
    dplyr::filter(sp_id %in% profiles$sp_id) %>%
    new_cfp_soilphys(id_cols = cfp_id_cols(soilphys))

  gasdata <- gasdata %>%
    dplyr::filter(gd_id %in% profiles$gd_id) %>%
    dplyr::left_join(
      layers_map %>%
        dplyr::group_by(
          dplyr::across(dplyr::any_of(cfp_id_cols(layers_map)))) %>%
        dplyr::summarise(upper = max(upper),
                         lower = min(lower)),
        by = cfp_id_cols(layers_map)
        ) %>%
    dplyr::filter(depth <= upper,
                  depth >= lower) %>%
    dplyr::select(!c("upper", "lower")) %>%
    new_cfp_gasdata(id_cols = cfp_id_cols(gasdata))


  message(paste0(nrow(profiles)," unique profiles"))

  # checking if layersmap range = soilphys range
  stopifnot("layers_map and soilphys must have the same max-min
            upper/lower bounds per group!" = same_range(soilphys,layers_map))


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
    dplyr::rename(umax_x = "umax",
           lmin_x = "lmin") %>%
    dplyr::left_join(get_upper_lower_range(layers_map),
              by = whats_in_both(list(cfp_id_cols(layers_map),
                                      cfp_id_cols(soilphys)))
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
  dplyr::rename(depth = "upper") %>%
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
    dplyr::select("depth_group", "upper", "lower") %>%
    dplyr::distinct()

  soilphys_new <-
    mapply(sp_dist$upper,
           sp_dist$lower,
           sp_dist$depth_group,
           FUN = add_between,
           MoreArgs = list(df = all_depths),
           SIMPLIFY = FALSE) %>%
    do.call(what = rbind) %>%
    data.frame() %>%
    stats::setNames(c("upper_new","lower_new","upper","lower",
                      "depth_group")) %>%
    dplyr::left_join(soilphys,
                     by = c("upper","lower","depth_group"),
                     relationship = "many-to-many") %>%
    dplyr::mutate(upper = .data$upper_new,
                  lower = .data$lower_new) %>%
    dplyr::select(!dplyr::any_of(c("upper_new","lower_new"))) %>%
    dplyr::mutate(height = (.data$upper - .data$lower)/100) %>%
    dplyr::mutate(depth = (.data$upper + .data$lower)/2) %>%
    dplyr::group_by(.data$sp_id) %>%
    dplyr::arrange("upper") %>%
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

  matrix(c(upper_new,lower_new,rep(upper, l),rep(lower, l),
           rep(depth_group, l)), ncol = 5)
}



sp_add_pmap <- function(soilphys,
                        layers_map){

  soilphys <- soilphys[,names(soilphys) != "pmap"]

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

      .x$pmap <- flex_length_apply(seq_nrow(.x), function(i){
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
#' @rdname extractors
#' @export
cfp_id_cols <- function(x){
  UseMethod("cfp_id_cols")
}
#' @export
cfp_id_cols.default <- function(x){
  attr(x,"id_cols")
}

#' Get number of groups/profiles
#'
#'
#'

#' @rdname utility
#' @param x A `cfp_dat` object.
#'
#' @returns An integer giving the number of groups of the object.
#' @examples
#' n_groups(base_dat)
#'
#' @export
n_groups <- function(x) {
  UseMethod("n_groups")
}


#' @exportS3Method
n_groups.cfp_dat <- function(x) {
  length(unique(x$profiles$group_id))
}


join_with_profiles <- function(target_data,
                               profiles,
                               id_cols){

  extra_cols <- c("sp_id", "gd_id", "group_id", "prof_id",
                  "step_id", "pmap", "row_id", "depth_group")
  join_cols <- names(profiles)[names(profiles) %in% names(target_data)]
  #id_cols <- join_cols[!join_cols %in% extra_cols]

  x <-
    profiles %>%
    dplyr::select(dplyr::all_of(c(join_cols, id_cols))) %>%
    dplyr::distinct() %>%
    dplyr::left_join(target_data,
                     by = join_cols,
                     relationship = "one-to-many") %>%
    dplyr::select(!dplyr::any_of(extra_cols))

  list(x, id_cols)
}

# @rdname get_soilphys
# @keywords internal
# @export
get_soilphys <- function(x){
  UseMethod("get_soilphys")
}
#' @exportS3Method
get_soilphys.cfp_dat <- function(x){
  soilphys <- x$soilphys
  profiles <- x$profiles

  x <- join_with_profiles(
    soilphys,
    profiles,
    cfp_id_cols(soilphys))

  x<-
    cfp_soilphys(x[[1]],
                 id_cols = x[[2]])

  x
}

# @rdname get_gasdata
# @keywords internal
# @export
get_gasdata <- function(x){
  UseMethod("get_gasdata")
}
#' @exportS3Method
get_gasdata.cfp_dat <- function(x){
  gasdata <- x$gasdata
  profiles <- x$profiles

  x <-
    join_with_profiles(gasdata,
                       profiles,
                       cfp_id_cols(gasdata))

  x <- cfp_gasdata(x[[1]],
                id_cols = x[[2]])
  x
}


# @rdname get_layers_map
# @keywords internal
# @export
get_layers_map <- function(x){
  UseMethod("get_layers_map")
}
#' @exportS3Method
get_layers_map.cfp_dat <- function(x){
  layers_map <- x$layers_map

  layers_map <-
  layers_map %>%
    dplyr::select(
      !dplyr::any_of(c("group_id")))
  layers_map
}



##### COERSION #######
#' @rdname cfp_dat
#' @export
as_cfp_dat <- function(x){
  UseMethod("as_cfp_dat")
}

#' @rdname cfp_dat
#' @exportS3Method
as_cfp_dat.cfp_dat <- function(x){

  gasdata <- cfp_gasdata(x)
  soilphys <- cfp_soilphys(x)
  layers_map <- cfp_layers_map(x)

  x <- cfp_dat(gasdata, soilphys, layers_map)
  x
}



##### SPLITTING #####
#' @rdname split_by_prof
#' @examples
#' split_by_group(base_dat)
#'
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

split_by_group_efficient <-
  function (x) {
    sp <- x$soilphys %>%
      dplyr::right_join(x$profiles[, c("sp_id", "group_id")] %>%
                         dplyr::distinct(),
                       by = "sp_id") %>%
      dplyr::group_by(group_id) %>%
      dplyr::group_split(.keep = FALSE)

    gd <- x$gasdata %>%
      dplyr::right_join(x$profiles[, c("gd_id", "group_id")] %>%
                         dplyr::distinct(),
                       by = "gd_id") %>%
      dplyr::group_by(group_id) %>%
      dplyr::group_split(.keep = FALSE)

    pf <- x$profiles %>%
      dplyr::group_by(group_id) %>%
      dplyr::group_split()

    lmap <- x$layers_map %>%
      dplyr::filter(group_id %in% x$profiles$group_id) %>%
      dplyr::group_by(group_id) %>%
      dplyr::group_split()


    out <- purrr::pmap(list(
      sp = sp,
      gd = gd,
      pf = pf,
      lmap = lmap
    ), function(sp, gd, pf, lmap) {
      cfp_dat_group <- ConFluxPro:::new_cfp_dat(
        new_cfp_gasdata(gd,
                        id_cols = cfp_id_cols(x$gasdata)),
        new_cfp_soilphys(sp,
                         id_cols = cfp_id_cols(x$soilphys)),
        cfp_layers_map(lmap,
                       id_cols = cfp_id_cols(x$layers_map)),
        new_cfp_profile(pf, id_cols = "prof_id"),
        id_cols = cfp_id_cols(x))
      attributes(cfp_dat_group) <- attributes(x)
      cfp_dat_group
    })
    out
  }

