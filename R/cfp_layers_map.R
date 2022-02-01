
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
