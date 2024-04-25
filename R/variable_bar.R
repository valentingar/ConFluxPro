#' variable bar
#'
#' @description A custom ggplot2 stat that implements a variable-width bar plot.
#'
#'
#'


Statvariable_bar <- ggplot2::ggproto("variable_bar", ggplot2::Stat,
                            compute_group = function(data, scales) {
                              parameter_x <- c("y", "xmax", "xmin")
                              parameter_y <- c("x", "ymax", "ymin")
                              data_columns <- names(data)

                              if (all(parameter_x %in% data_columns) &
                                  !(any(parameter_y %in% data_columns))){

                                data <-
                                  data.frame(x = (data$xmax + data$xmin) / 2,
                                             y = data$y / 2,
                                             width = data$xmax - data$xmin,
                                             height = data$y,
                                             xmax = data$xmax,
                                             xmin = data$xmin
                                  )
                              } else if (all(parameter_y %in% data_columns) &
                                         !(any(parameter_x %in% data_columns))){

                                data <-
                                  data.frame(y = (data$ymax + data$ymin) / 2,
                                             x = data$x / 2,
                                             height = data$ymax - data$ymin,
                                             width = data$x,
                                             ymax = data$ymax,
                                             ymin = data$ymin

                                  )
                              } else {
                                stop("provide either x, ymin and ymax or y, xmin and xmax")
                              }

                              data
                            }
)

stat_variable_bar <- function(mapping = NULL, data = NULL, geom = "tile",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = Statvariable_bar, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

