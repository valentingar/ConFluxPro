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
#' @param sample_from From which dataset to sample the bootstrapping dataset.
#' Can either be \code{'gasdata'} or \code{'soilphys'} or \code{'both'}.
#' @param rep_cols The id_cols that represent repetitions. If removed, the
#' repetitions in soilphys of each profile must match in their structure exactly.
#'
#' @returns x with added columns DELTA_flux and DELTA_prod as an estimate
#' of the error of of the corresponding columns in the same units.
#'
#' @details
#' # General procedure
#' [bootstrap_error()] is mostly a wrapper around two functions that can also be
#' run separately.
#'
#' In [make_bootstrap_model()], for \code{sample_from = "gasdata"} the
#' \code{gasdata} concentration data is resampled for every depth and profile
#' a total number of \code{n_samples}. This is done by randomly sampling the
#' observations at each depth without changing the number of observations but
#' while allowing replacing. If \code{rep_cols} are given, these columns are
#' removed from the \code{id_cols} and the resulting profiles combined as one.
#'
#' For \code{sample_from = "soilphys"}, the \code{soilphys} data is combined
#' using the \code{rep_cols} as repetitions. Among every remaining profile and
#' depth, one observation across all repetitions is chosen for each of
#' \code{n_samples}. \code{sample_from = "both"} applies both methods above.
#' Each newly sampled profile is identifiable by the
#' added \code{bootstrap_id} column which is also added to \code{id_cols}.
#'
#' After this new model is run again, the bootstap error is caculated in
#' [calculate_bootstrap_error()]. This is the standard deviation of the
#' production and flux parameters across all bootstrapped model runs and is
#' calculated for each profile and layer of the original model, or for each
#' destinct profile in the new model without \code{rep_cols}.
#' These are returned together with the mean values of \code{prod}, \code{flux}
#' and \code{F0} across all runs in the \code{PROFLUX} data.frame and can
#' thereby be extracted by [efflux()] and [production()].
#'
#' # Artificial observations in gasdata
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
                            n_replicates = NULL,
                            sample_from = "gasdata",
                            rep_cols = NULL){
  UseMethod("bootstrap_error")
}

#' @rdname bootstrap_error
#' @exportS3Method
bootstrap_error.cfp_dat <- function(x,
                            n_samples = 50,
                            sd_x_ppm = NULL,
                            n_replicates = NULL,
                            sample_from = "gasdata",
                            rep_cols = NULL){
  stop("Can't bootstrap error from data alone!
       Create a model frame first with cfp_pfmod() or cfp_fgmod().")
}

#' @rdname bootstrap_error
#' @exportS3Method
bootstrap_error.cfp_fgmod <- function(x,
                                    n_samples = 50,
                                    sd_x_ppm = NULL,
                                    n_replicates = NULL,
                                    sample_from = "gasdata",
                                    rep_cols = NULL){
  stop("Bootstrapping error for cfp_fgmod() models is not yet implemented.")
}

#' @rdname bootstrap_error
#' @exportS3Method
bootstrap_error.cfp_pfmod  <- function(x,
                                      n_samples = 50,
                                      sd_x_ppm = NULL,
                                      n_replicates = NULL,
                                      sample_from = "gasdata",
                                      rep_cols = NULL){

  sample_from <- match.arg(sample_from, c("gasdata", "soilphys", "both"))

  if(sample_from != "gasdata" & is.null(rep_cols)){
    stop("Provide rep_cols that identify the repetition of each profile!")
  }

  stopifnot("rep_cols must be in id_cols or NULL" = is.null(rep_cols) || all(rep_cols %in% cfp_id_cols(x)))

  y <- make_bootstrap_model(x,
                            n_samples,
                            sd_x_ppm,
                            n_replicates,
                            sample_from,
                            rep_cols)

  y <- pro_flux(y)

  y <- calculate_bootstrap_error(x,y)
  y
}


#' @rdname bootstrap_error
#' @export
make_bootstrap_model <- function(x,
                                 n_samples = 50,
                                 sd_x_ppm = NULL,
                                 n_replicates = NULL,
                                 sample_from = "gasdata",
                                 rep_cols = NULL){
  UseMethod("make_bootstrap_model")
}

#' @rdname bootstrap_error
#' @exportS3Method
make_bootstrap_model.cfp_pfmod <- function(x,
                                 n_samples = 50,
                                 sd_x_ppm = NULL,
                                 n_replicates = NULL,
                                 sample_from = "gasdata",
                                 rep_cols = NULL){

  sample_from_gasdata <- any(c("gasdata", "both") %in% sample_from)
  sample_from_soilphys <- any(c("soilphys", "both") %in% sample_from)

  gasdata <- x$gasdata
  soilphys <- x$soilphys

  gd_id_cols <- cfp_id_cols(gasdata)
  sp_id_cols <- cfp_id_cols(soilphys)
  stopifnot("rep_cols also id_cols in gasdata! Can't aggregate model." =
              !(!sample_from_gasdata && any(rep_cols %in% gd_id_cols)))
  stopifnot("rep_cols also id_cols in soilphys! Can't aggregate model." =
              !(!sample_from_soilphys && any(rep_cols %in% sp_id_cols)))

  if (sample_from_gasdata){
    if (!is.null(rep_cols)){
      gasdata <- cfp_gasdata(gasdata[!names(gasdata) %in% rep_cols],
                             id_cols = cfp_id_cols(x$gasdata)[!cfp_id_cols(x$gasdata) %in% rep_cols])
    }

    gasdata <- create_extended_gasdata(gasdata,
                                       depth_structure(x, structure_from = "gasdata"),
                                       sd_x_ppm, n_replicates)

    gasdata <- create_bootstrap_gasdata(gasdata, n_samples)
  }

  if (sample_from_soilphys){
    stopifnot("provide rep_cols if sampling from soilphys" = !is.null(rep_cols))
    stopifnot("rep_cols lead to profiles that don't fit" =
                check_matching_repetitions(soilphys, rep_cols))

    soilphys <- create_bootstrap_soilphys(
      soilphys,
      n_samples,
      rep_cols
    )




  }

  y <- cfp_pfmod(cfp_dat(gasdata, soilphys, cfp_layers_map(x)),
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
calculate_bootstrap_error.cfp_pfmod <- function(x, y){
  y_id_cols <- cfp_id_cols(y)
  y_id_cols <- y_id_cols[!(y_id_cols == "bootstrap_id")]

  BOOTSTRAP_FLUX <-
    y$PROFLUX %>%
    dplyr::left_join(y$profiles, by = c("prof_id", "sp_id")) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(y_id_cols,
                                                  "pmap",
                                                  "upper",
                                                  "lower",
                                                  "step_id")))) %>%
    dplyr::summarise(DELTA_flux = sd(flux, na.rm = TRUE),
                     DELTA_F0 = sd(F0, na.rm = TRUE),
                     DELTA_prod = sd(prod, na.rm = TRUE),
                     flux = mean(flux, na.rm = TRUE),
                     F0 = mean(F0, na.rm = TRUE),
                     prod = mean(prod, na.rm = TRUE),
                     conc = mean(conc, na.rm = TRUE),
                     RMSE = mean(RMSE, na.rm = TRUE),
                     )

  if(cfp_zero_flux(x)){
    BOOTSTRAP_FLUX$DELTA_F0 <- NA # makes no sense if F0 set to zero
  }

  x_id_cols <- cfp_id_cols(x)

  if (!all(x_id_cols %in% y_id_cols)){
    soilphys <- x$soilphys %>%
      dplyr::select(!"sp_id") %>%
      dplyr::group_by(dplyr::across(
        dplyr::all_of(c(y_id_cols[y_id_cols %in% cfp_id_cols(x$soilphys)],
                        "upper", "lower", "depth", "pmap", "step_id")))) %>%
      dplyr::summarise(dplyr::across(dplyr::where(is.numeric), function(x) mean(x, na.rm = TRUE))) %>%
      cfp_soilphys(id_cols = y_id_cols[y_id_cols %in% cfp_id_cols(x$soilphys)])

    gasdata <- cfp_gasdata(x$gasdata,
                           id_cols = y_id_cols[y_id_cols %in% cfp_id_cols(x$gasdata)])

    x_new <- cfp_dat(gasdata, soilphys, x$layers_map)
    x$profiles <- x_new$profiles
    x$soilphys <- x_new$soilphys
    x$gasdata <- x_new$gasdata
    attr(x, "id_cols") <- y_id_cols
  }

  x_profiles <- x$profiles
  #x_FLUX <- data.frame(x$PROFLUX)

  y <-
    x_profiles %>%
    dplyr::left_join(BOOTSTRAP_FLUX, by = cfp_id_cols(x)[cfp_id_cols(x) %in% names(BOOTSTRAP_FLUX)]) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(
      "upper",
      "lower",
      "prof_id",
      "sp_id",
      "step_id",
      "pmap",
      "DELTA_flux",
      "DELTA_F0",
      "DELTA_prod",
      "flux",
      "F0",
      "prod",
      "conc",
      "RMSE"))) %>%
    #dplyr::right_join(x_FLUX, by = c("prof_id", "pmap")) %>%
    cfp_layered_profile(id_cols = "prof_id")

  x <- cfp_pfres(x, y)

  x
}



### helpers --------------------------------------------------------------------
create_extended_gasdata <- function(
    gasdata,
    gasdata_depths,
    sd_x_ppm = NULL,
    n_replicates = NULL){

  if (is.null(n_replicates)){
    return(gasdata)
  }

  stopifnot("'n_replicates' must be an integer!" = (round(n_replicates) == n_replicates))

  message(paste0("Adding gasdata replicates as normal distribution of ", n_replicates, " samples around the mean."))

  if ("sd_x_ppm" %in% names(gasdata)){
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

  new_sel <-
  replicate(n_samples, sapply(split_id_split, function(x){
          sample.vec(x, length(x), replace = TRUE)
        })%>% unlist()) %>%
    c()

  gasdata <- gasdata[new_sel, ]
  gasdata$bootstrap_id <- rep(1:n_samples, each = length(split_id))

  gasdata <- cfp_gasdata(gasdata, id_cols = c(cfp_id_cols(gasdata), "bootstrap_id"))

  rownames(gasdata) <- 1:nrow(gasdata)

  gasdata
}


create_bootstrap_soilphys <- function(
    soilphys,
    n_samples,
    rep_cols){

  id_cols <- cfp_id_cols(soilphys)
  attr(soilphys, "id_cols") <- id_cols[!id_cols %in% rep_cols]

  split_id <- soilphys %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::any_of(c(cfp_id_cols(soilphys), "upper", "lower")))) %>%
    dplyr::group_indices()

  split_id_split <- split(1:length(split_id), split_id)

  new_sel <-
    replicate(n_samples, sapply(split_id_split, function(x){
      sample.vec(x, 1, replace = TRUE)
    })%>% unlist()) %>%
    c()

  soilphys <- soilphys[new_sel, ]
  soilphys$bootstrap_id <- rep(1:n_samples, each = length(split_id_split))

  soilphys <- cfp_soilphys(
    soilphys,
    id_cols = c(cfp_id_cols(soilphys),
                "bootstrap_id"))

    rownames(soilphys) <- 1:nrow(soilphys)

  soilphys
}

