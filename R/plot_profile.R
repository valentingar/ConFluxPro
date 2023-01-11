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

  profiles <- x$profiles

  soilphys <- x$soilphys %>%
    dplyr::left_join(profiles)
  gasdata <- x$gasdata %>%
    dplyr::left_join(profiles)
  PROFLUX <- x$PROFLUX %>%
    dplyr::left_join(profiles)

  prod_range <- range(PROFLUX$prod)
  if (prod(prod_range) < 0) prod_state <- 2
  else if(prod_range[1] < 0) prod_state <- 3
  else prod_state <- 1

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
    ggplot(aes(ymax = upper, ymin = lower)) +
    stat_variable_bar(aes(x = 1, fill = "soil")) +
    stat_variable_bar(aes(x = TPS, fill = "AFPS")) +
    stat_variable_bar(aes(x = SWC, fill = "SWC"))+
  stat_variable_bar(aes(x = SWC, fill = "SWC")) +
  geom_rect(
    data = PROFLUX,
    aes(xmin = prod_start,
        xmax = prod_start + ((prod)/prod_max)*prod_scale,
        ymax = upper,
        ymin = lower,
        fill = "production"),
    alpha = 0.5)+
  geom_point(data = gasdata,
             aes(y = depth,
                 x = x_ppm / x_max,
                 color = "measured"),
             inherit.aes = FALSE)+
  geom_point(data = gasdata_adapted,
             aes(y = depth,
               x = conc / x_max/ c_air,
                 color = "modeled"),
             inherit.aes = FALSE)+
  geom_line(data = gasdata_adapted,
            aes(y = depth,
                x = conc / x_max/ c_air,
                color = "modeled"),
            inherit.aes = FALSE,
            orientation = "y")+
  scale_x_continuous(name = "concentration [ppm]",
                     breaks = clean_breaks_zeroone(x_max,c(0,0.25,0.5,0.75,1)),
                     labels = function(x)x*x_max,
                     sec.axis = sec_axis(trans = ~(.-prod_start)*prod_max/prod_scale,
                                         name = expression("production [µmol m"^"-2"~"s"^"-1"~"]")))+
    scale_cfp_color+
    scale_cfp_fill


if (prod_state == 2){
  p +  geom_vline(xintercept = 0.5, alpha = 0.5)
}
}

#' @exportS3Method
plot_profile.cfp_soilphys <- function(x) {

  t_max <- max(x$t)
  t_min <- min(x$t)
  DS_max <- max(x$DS)

  x %>%
  ggplot(aes(ymax = upper, ymin = lower)) +
  stat_variable_bar(aes(x = 1, fill = "soil")) +
  stat_variable_bar(aes(x = TPS, fill = "AFPS")) +
  stat_variable_bar(aes(x = SWC, fill = "SWC"))+
    geom_line(aes(y = depth, x = (t-min(t))/(max(t)-min(t)), col = "temperature"),
              orientation = "y")+
    geom_line(aes(y = depth, x = DS/!!DS_max, col = "DS"),
              orientation = "y")+
    geom_text(aes(x = (TPS+SWC)/2, y = depth, label = signif(c_air, 3)))+
    scale_x_continuous(name = "temperature [°C]",
                       breaks = clean_breaks_zeroone(range = (t_max-t_min), breaks = c(0,0.25,0.5,0.75,1)),
                       labels = function(x)x*(t_max-t_min),
                       sec.axis = sec_axis(trans = ~.*DS_max,
                                           name = expression("DS [m"^"2"~"s"^"-1"~"]")))+
    scale_cfp_color+
    scale_cfp_fill
  }



clean_breaks_zeroone <- function(range, breaks){
  round(range, (-nchar(round((range)/(length(breaks) - 1)))))/range*breaks
}


scale_cfp_color <- scale_color_manual(
  name = "",
  breaks = c("temperature", "DS", "measured", "modeled"),
  values = c("red", "black", "black", "darkgrey") )
scale_cfp_fill <-
  scale_fill_manual(
    name = "",
    breaks = c("soil", "AFPS", "SWC", "production"),
    values = c("darkorange", "white", "blue", "black"))
