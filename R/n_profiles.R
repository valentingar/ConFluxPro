#' @rdname utility
#' @returns An integer giving the number of profiles of the object.
#' @examples
#' n_profiles(base_dat)
#' n_profiles(cfp_soilphys(base_dat))
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
  NextMethod()
}

#' @exportS3Method
n_profiles.cfp_gasdata <- function(x) {
  NextMethod()
}

#' @exportS3Method
n_profiles.cfp_layers_map <- function(x) {
  NextMethod()
}

#' @exportS3Method
n_profiles.cfp_profile <- function(x){
  id_cols_to_n_profiles(x)
}


#### HELPERS #### ------------
id_cols_to_n_profiles <- function(x){
  x[cfp_id_cols(x)] %>% dplyr::distinct() %>% nrow()
}
