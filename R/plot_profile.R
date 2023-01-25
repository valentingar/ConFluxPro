#' plot_profile
#'
#' Plot ConFluxPro profiles using ggplot. Supported objects:
#' \describe{
#' \item{cfp_soilphys}{Displays TPS, SWC and AFPS, as well as values of Ds and Temperature.}
#' \item{cfp_pfres}{Displays TPS, SWC and AFPS, as well as production and measured and modelled gas concentrations.}
#'}
#'
#' @param x
#'
#' @importFrom ggplot2 aes
#' @export
plot_profile <- function(x) {
  UseMethod("plot_profile")
}


#' @exportS3Method
plot_profile.cfp_pfres <- function(x) {
  check_for_too_many_profiles(x)

  profiles <- x$profiles

  soilphys <- x$soilphys %>%
    dplyr::left_join(profiles)
  gasdata <- x$gasdata %>%
    dplyr::left_join(profiles)
  PROFLUX <- x$PROFLUX %>%
    dplyr::left_join(profiles)

  prod_range <- range(PROFLUX$prod)
  if (prod(prod_range) < 0)
    prod_state <- 2
  else if (prod_range[1] < 0)
    prod_state <- 3
  else
    prod_state <- 1

  prod_start <- c(0, 0.5, 1)[prod_state]
  prod_scale <- c(1, 0.5, 1)[prod_state]

  prod_max <- max(abs(prod_range))

  x_max <- max(gasdata$x_ppm)

  gasdata_adapted <-
    soilphys %>%
    dplyr::select(!depth) %>%
    dplyr::rename(depth = upper) %>%
    dplyr::right_join(PROFLUX) %>%
    dplyr::arrange(dplyr::desc(depth))

  p <-
    soilphys %>%
    ggplot2::ggplot(aes(ymax = upper, ymin = lower)) +
    stat_variable_bar(aes(x = 1, fill = "soil")) +
    stat_variable_bar(aes(x = TPS, fill = "AFPS")) +
    stat_variable_bar(aes(x = SWC, fill = "SWC")) +
    ggplot2::geom_rect(
      data = PROFLUX,
      aes(
        xmin = prod_start,
        xmax = prod_start + ((prod) / prod_max) * prod_scale,
        ymax = upper,
        ymin = lower,
        fill = "production"
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = gasdata,
      aes(
        y = depth,
        x = x_ppm / x_max,
        color = "measured"
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_point(
      data = gasdata_adapted,
      aes(
        y = depth,
        x = conc / x_max / c_air,
        color = "modeled"
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_line(
      data = gasdata_adapted,
      aes(
        y = depth,
        x = conc / x_max / c_air,
        color = "modeled"
      ),
      inherit.aes = FALSE,
      orientation = "y"
    ) +
    ggplot2::scale_x_continuous(
      name = "concentration [ppm]",
      breaks = scales::pretty_breaks(5)(c(0,x_max)) / x_max,
      labels = function(x)
        x * x_max,
      sec.axis = ggplot2::sec_axis(
        trans = ~ (. - prod_start) * prod_max / prod_scale,
        name = expression("production [µmol m" ^
                            "-3" ~ "s" ^ "-1" ~ "]")
      )
    ) +
    scale_cfp_color +
    scale_cfp_fill


  if (prod_state == 2) {
    p <- p +  ggplot2::geom_vline(xintercept = 0.5, alpha = 0.5)
  }

  p +
    ggplot2::facet_wrap(cfp_id_cols(x))
}


#' @exportS3Method
plot_profile.cfp_fgres <- function(x) {
  check_for_too_many_profiles(x)

  profiles <- x$profiles

  soilphys <- x$soilphys %>%
    dplyr::left_join(profiles)
  gasdata <- x$gasdata %>%
    dplyr::left_join(profiles)
  FLUX <- x$FLUX %>%
    dplyr::left_join(profiles)

  flux_range <- range(FLUX$flux)
  if (prod(flux_range) < 0)
    flux_state <- 2
  else if (flux_range[1] < 0)
    flux_state <- 3
  else
    flux_state <- 1

  flux_start <- c(0, 0.5, 1)[flux_state]
  flux_scale <- c(1, 0.5, 1)[flux_state]

  flux_max <- max(abs(flux_range))

  x_max <- max(gasdata$x_ppm)


  p <-
    soilphys %>%
    ggplot2::ggplot(aes(ymax = upper, ymin = lower)) +
    stat_variable_bar(aes(x = 1, fill = "soil")) +
    stat_variable_bar(aes(x = TPS, fill = "AFPS")) +
    stat_variable_bar(aes(x = SWC, fill = "SWC")) +
    stat_variable_bar(aes(x = SWC, fill = "SWC")) +
    ggplot2::geom_rect(
      data = FLUX,
      aes(
        xmin = flux_start,
        xmax = flux_start + ((flux) / flux_max) * flux_scale,
        ymax = upper,
        ymin = lower,
        fill = "flux"
      ),
      alpha = 0.5
    ) +
    ggplot2::geom_point(
      data = gasdata,
      aes(
        y = depth,
        x = x_ppm / x_max,
        color = "measured"
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(data = FLUX,
              aes( x = 0.8,
                   y = (upper + lower) / 2,
                   label = signif(dcdz_ppm, 3))) +
    ggplot2::geom_text(aes(x = 0.8,
                  y = 1.1*diff(range(c(upper, lower))) + min(lower),
                  label = "dcdz_ppm")) +
    ggplot2::scale_x_continuous(
      name = "concentration [ppm]",
      breaks = scales::pretty_breaks(5)(c(0,x_max)) / x_max,
      labels = function(x)
        x * x_max,
      sec.axis = ggplot2::sec_axis(
        trans = ~ (. - flux_start) * flux_max / flux_scale,
        name = expression("flux [µmol m" ^
                            "-2" ~ "s" ^ "-1" ~ "]")
      )
    ) +
    scale_cfp_color +
    scale_cfp_fill

  if (flux_state == 2) {
    p <- p +  ggplot2::geom_vline(xintercept = 0.5, alpha = 0.5)
  }

  p +
    ggplot2::facet_wrap(cfp_id_cols(x))
}


#' @exportS3Method
plot_profile.cfp_soilphys <- function(x) {
  check_for_too_many_profiles(x)

  t_max <- max(x$t)
  t_min <- min(x$t)
  DS_max <- max(x$DS)

  x %>%
    ggplot2::ggplot(aes(ymax = upper, ymin = lower)) +
    stat_variable_bar(aes(x = 1, fill = "soil")) +
    stat_variable_bar(aes(x = TPS, fill = "AFPS")) +
    stat_variable_bar(aes(x = SWC, fill = "SWC")) +
    ggplot2::geom_line(aes(
      y = depth,
      x = (t - min(t)) / (max(t) - min(t)),
      col = "temperature"
    ),
    orientation = "y") +
    ggplot2::geom_line(aes(
      y = depth,
      x = DS / !!DS_max,
      col = "DS"
    ),
    orientation = "y") +
    ggplot2::geom_text(aes(
      x = (TPS + SWC) / 2,
      y = depth,
      label = signif(c_air, 3)
    )) +
    ggplot2::scale_x_continuous(
      name = "temperature [°C]",
      breaks = (scales::pretty_breaks(5)(c(t_min, t_max)) - t_min) / (t_max - t_min),
      labels = function(x)
        (x * (t_max - t_min)) + t_min,
      sec.axis = ggplot2::sec_axis(
        trans = ~ . * DS_max,
        name = expression("DS [m" ^ "2" ~
                            "s" ^ "-1" ~ "]")
      )
    ) +
    scale_cfp_color +
    scale_cfp_fill +
    ggplot2::facet_wrap(cfp_id_cols(x))
}



#' @exportS3Method
plot_profile.cfp_layers_map <- function(x) {
  check_for_too_many_profiles(x)

  x %>%
    dplyr::mutate(depth = (upper+lower)/2) %>%
    dplyr::mutate(range = diff(range(c(lowlim, highlim)))) %>%
    dplyr::mutate(yrange = diff(range(c(upper, lower)))) %>%
    dplyr::mutate(layer_couple = ifelse(lower == min(lower), NA, layer_couple)) %>%
    ggplot2::ggplot(aes(ymax = upper, ymin = lower, y = depth))+
    ggplot2::geom_rect(aes(xmin = lowlim,
                           xmax = highlim,
                           fill = "production"),
                       alpha = 0.5)+
    ggplot2::geom_hline(aes(yintercept = upper), col = "black")+
    ggplot2::geom_hline(aes(yintercept = lower), col = "black")+
    ggplot2::geom_text(aes(label = layer,
                           x = 0.2*range), col = "white")+
    ggplot2::geom_text(aes(label = pmap,
                           x = 0.4*range), col = "white")+
    ggplot2::geom_text(aes(label = layer_couple,
                           x = 0.8*range,
                           y = lower),
                       col = "white")+
    ggplot2::geom_text(aes(label = "layer",
                           x = 0.2*range[1],
                           y = 1.1*yrange[1] + min(lower)),
                       col = "black")+
    ggplot2::geom_text(aes(label = "pmap",
                           x = 0.4*range[1],
                           y = 1.1*yrange[1] + min(lower)),
                       col = "black")+
    ggplot2::geom_text(aes(label = "layer_couple",
                           x = 0.8*range[1],
                           y = 1.1*yrange[1] + min(lower)),
                       col = "black")+
    ggplot2::facet_wrap(cfp_id_cols(x))+
    scale_cfp_fill
}


#' @exportS3Method
plot_profile.cfp_gasdata <- function(x) {
  check_for_too_many_profiles(x)

  x %>%
    ggplot2::ggplot(aes(y = depth)) +
    ggplot2::geom_point(aes(x = x_ppm, color = "measured")) +
    ggplot2::stat_summary(aes(x = x_ppm, color = "measured"),
                          geom = "line",
                          orientation = "y",
                          fun = "mean") +
    scale_cfp_color+
    ggplot2::facet_wrap(cfp_id_cols(x))
}


### HELPERS ### ------------------------
scale_cfp_color <-
  ggplot2::scale_color_manual(
    name = "",
    breaks = c("temperature", "DS", "measured", "modeled"),
    values = c("red", "black", "black", "darkgrey")
  )
scale_cfp_fill <-
  ggplot2::scale_fill_manual(
    name = "",
    breaks = c("soil", "AFPS", "SWC", "production", "flux"),
    values = c("darkorange", "white", "blue", "black", "black")
  )

check_for_too_many_profiles <- function(x){
  number_of_profiles <- n_profiles(x)
  if (number_of_profiles > 20){
    warning("Plotting ", number_of_profiles, " plots may take a while")
  }
}

