#' @inheritParams cfp_pfmod
#' @rdname extractors
#' @export
n_profiles <- function(x) {
  UseMethod("n_profiles")
}

#' @exportS3Method
n_profiles.cfp_dat <- function(x) {
  nrow(x$profiles)
}

#' @exportS3Method
n_profiles.cfp_soilphys <- function(x) {
  id_cols_to_n_profiles(x)
}

#' @exportS3Method
n_profiles.cfp_gasdata <- function(x) {
  id_cols_to_n_profiles(x)
}

#' @exportS3Method
n_profiles.cfp_layers_map <- function(x) {
  id_cols_to_n_profiles(x)
}


#### HELPERS #### ------------
id_cols_to_n_profiles <- function(x){
  x[cfp_id_cols(x)] %>% dplyr::distinct() %>% nrow()
}
