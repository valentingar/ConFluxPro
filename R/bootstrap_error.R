#' @title Bootstrap-estimate of Flux-error
#'
#' @param x A \code{cfp_pfres} model result from a call to [pro_flux()].
#' @param n_samples The number of samples to take in the bootstrapping.
#' @param sd_x_ppm An optional estimate of the standard deviation of x_ppm. Can
#' be either
#' \itemize{
#' \item{a single value applied equally to all}
#' \item{a data.frame with a column of the same name that
#' maps a value to every observation depth. See [depth_structure()] for an easy
#' way to create it.}
#' \item{be provided as its own column already present in \code{x$gasdata}.
#' }
#' }
#' @param n_replicates The number of replicates to be generated if sd_x_ppm is
#' set.
#'
#' @returns x with added columns DELTA_flux and DELTA_prod as an estimate
#' of the error of of the corresponding columns in the same units.
#'
#' @details
#' # General procedure
#' [bootstrap_error()] is mostly a wrapper around two functions that can also be
#' run separately.
#'
#' In [make_bootstrap_model()], the \code{gasdata} concentration data is
#' resampled for every depth and profile a total number of \code{n_samples}.
#' This is done by randomly sampling the observations at each depth without
#' changing the number of observations but while allowing replacing. Each newly
#' sampled profile is identifiable by the added \code{bootstrap_id} column which
#' is also added to \code{id_cols}.
#'
#' After this new model is run again, the bootstap error is caculated in
#' [calculate_bootstrap_error()]. This is the standard deviation of the
#' production and flux parameters across all bootstrapped model runs and is
#' calculated for each profile and layer of the original model.
#' These are added to the \code{PROFLUX} data of the original model and can
#' thereby be extracted by [efflux()].
#'
#' # Artificial observations
#' If there are not enough observations per depth (e.g.) because there is only
#' one measurement per depth, it is possible to create artificial observations
#' by providing \code{n_replicates} and \code{sd_x_ppm}. Here, every depth of
#' every profile is first averaged to its mean (redundant if there is only one
#' observation). Then, a random dataset of \code{n_replicates} observations
#' is generated that is normally distributed around the mean with a standard
#' deviation (in ppm) of \code{sd_x_ppm}. These observations are then resampled
#' as described above. Note that this error should be representative of the
#' sampling error in the field and not the measurement error of the measurement
#' device, which is much lower.

#' @examples
#' PROFLUX <- pro_flux(ConFluxPro::base_dat)
#' PROFLUX_BSE <- bootstrap_error(PROFLUX)
#' efflux(PROFLUX_BSE)
#'
#' PROFLUX_BSE <- bootstrap_error(PROFLUX, n_replicates = 5, sd_x_ppm = 25)
#' efflux(PROFLUX_BSE)

#' @name bootstrap_error
#' @export

bootstrap_error <- function(x,
                            n_samples = 50,
                            sd_x_ppm = NULL,
                            n_replicates = NULL){
  UseMethod("bootstrap_error")
}

#' @rdname bootstrap_error
#' @exportS3Method
bootstrap_error.cfp_pfres <- function(x,
                                      n_samples = 50,
                                      sd_x_ppm = NULL,
                                      n_replicates = NULL){

  y <- make_bootstrap_model(x, n_samples, sd_x_ppm, n_replicates)

  y <- pro_flux(y)

  y <- calculate_bootstrap_error(x,y)
  y
}


#' @rdname bootstrap_error
#' @export
make_bootstrap_model <- function(x,
                                 n_samples = 50,
                                 sd_x_ppm = NULL,
                                 n_replicates = NULL){
  UseMethod("make_bootstrap_model")
}

#' @rdname bootstrap_error
#' @exportS3Method
make_bootstrap_model.cfp_pfres <- function(x,
                                 n_samples = 50,
                                 sd_x_ppm = NULL,
                                 n_replicates = NULL){

  gasdata <- create_extended_gasdata(x, sd_x_ppm, n_replicates)

  gasdata <- create_bootstrap_gasdata(gasdata, n_samples)

  y <- cfp_pfmod(cfp_dat(gasdata, cfp_soilphys(x), cfp_layers_map(x)),
                 cfp_zero_flux(x),
                 cfp_zero_limits(x),
                 cfp_DSD0_optim(x),
                 cfp_evenness_factor(x),
                 cfp_known_flux_factor(x)
  )
  y
}

#' @rdname bootstrap_error
#' @param y The result of the bootstrap model.
#' @export
calculate_bootstrap_error <- function(x, y){
  UseMethod("calculate_bootstrap_error")
}
#' @rdname bootstrap_error
#' @exportS3Method
calculate_bootstrap_error.cfp_pfres <- function(x, y){
  y_id_cols <- cfp_id_cols(y)
  y_id_cols <- y_id_cols[!(y_id_cols == "bootstrap_id")]

  BOOTSTRAP_FLUX <-
    y$PROFLUX %>%
    dplyr::left_join(y$profiles, by = c("prof_id", "sp_id")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(y_id_cols, "pmap")))) %>%
    dplyr::summarise(DELTA_flux = sd(flux, na.rm = TRUE),
                     DELTA_F0 = sd(F0, na.rm = TRUE),
                     DELTA_prod = sd(prod, na.rm = TRUE))

  x_profiles <- x$profiles
  x_FLUX <- data.frame(x$PROFLUX)

  x$PROFLUX <-
    x_profiles %>%
    dplyr::left_join(BOOTSTRAP_FLUX, by = cfp_id_cols(x)[cfp_id_cols(x) %in% names(BOOTSTRAP_FLUX)]) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c("prof_id", "pmap", "DELTA_flux", "DELTA_F0", "DELTA_prod"))) %>%
    dplyr::right_join(x_FLUX, by = c("prof_id", "pmap")) %>%
    cfp_layered_profile(id_cols = cfp_id_cols(x$PROFLUX))

  x
}



### helpers --------------------------------------------------------------------
create_extended_gasdata <- function(x,
                             sd_x_ppm = NULL,
                             n_replicates = NULL){

  if (is.null(n_replicates)){
    return(x$gasdata)
  }

  stopifnot("'n_replicates' must be an integer!" = (round(n_replicates) == n_replicates))

  message(paste0("Adding gasdata replicates as normal distribution of ", n_replicates, " samples around the mean."))

  gasdata_depths <- depth_structure(x, structure_from = "gasdata")

  if ("sd_x_ppm" %in% names(x$gasdata)){
    if (!is.null(sd_x_ppm)) message("Ignoring sd_x_ppm, because it's present in gasdata.")
  } else if (is.data.frame(sd_x_ppm)){
    gasdata_depths <- gasdata_depths %>%
      dplyr::left_join(sd_x_ppm, by = c(cfp_id_cols(gasdata_depths), "depth"))

    stopifnot("Missing column 'sd_x_ppm' in data.frame 'sd_x_ppm'" = ("sd_x_ppm" %in% names(gasdata_depths)))
    stopifnot("'sd_x_ppm' cannot contain NAs and must cover all profile depths!" = all(!is.na(gasdata_depths$sd_x_ppm)) )
  } else {
    stopifnot("Length of 'sd_x_ppm' must be 1 or be a data.frame" = length(sd_x_ppm) == 1)
    gasdata_depths$sd_x_ppm <- sd_x_ppm
  }

  gasdata <- x$gasdata

  gasdata %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(cfp_id_cols(gasdata), "depth")))) %>%
    dplyr::summarise(dplyr::across(dplyr::everything(), function(x) mean(x, na.rm = TRUE))) %>%
    dplyr::left_join(gasdata_depths, by = c(cfp_id_cols(gasdata_depths), "depth")) %>%
    dplyr::ungroup() %>%
    dplyr::rowwise() %>%
    dplyr::reframe(x_ppm = stats::rnorm(n_replicates, .data$x_ppm, .data$sd_x_ppm),
                   dplyr::across(-dplyr::any_of("x_ppm"), function(x) rep(x, n_replicates))) %>%
    cfp_gasdata(cfp_id_cols(gasdata))
}


create_bootstrap_gasdata <- function(gasdata, n_samples){


  split_id <- gasdata %>%
    dplyr::group_by(dplyr::across(dplyr::any_of(c(cfp_id_cols(gasdata),"depth")))) %>%
    dplyr::group_indices()

  split_id_split <- split(1:length(split_id), split_id)

  gasdata <-
    lapply(1:n_samples, function(i){
      gd <- gasdata[sapply(split_id_split, function(x){
        sample(x, length(x), replace = TRUE)
      })%>% unlist(),]

    }) %>%
    dplyr::bind_rows(.id = "bootstrap_id") %>%
    cfp_gasdata(id_cols = c(cfp_id_cols(gasdata), "bootstrap_id"))

  rownames(gasdata) <- 1:nrow(gasdata)

  gasdata
}

