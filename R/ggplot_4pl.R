#' @import dplyr

# Creating a 4PL regression curve layer for ggplot
# Adapted from the example http://docs.ggplot2.org/dev/vignettes/extending-ggplot2.html

# Eric Koncina 2016

Stat4PL <- ggplot2::ggproto("Stat4PL", ggplot2::Stat, 
                            required_aes = c("x", "y"),
                            compute_group = function(data, scales, logDose) {
                              model.4pl <- data %>%
                                drc::drm(y ~ x, data = ., fct = drc::LL.4(), logDose = logDose)
                              grid <- data.frame(x = seq(min(data$x), max(data$x), length.out = 100))  %>%
                                mutate(y = predict(model.4pl, .)) %>%
                                select(x, y)
                              grid
                            }
)

#' @export
stat_4pl <- function(mapping = NULL, data = NULL, geom = "line",
                     position = "identity", na.rm = FALSE, show.legend = NA, 
                     inherit.aes = TRUE, logDose = NULL, ...) {
  layer(
    stat = Stat4PL, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(logDose = logDose, na.rm = na.rm, ...)
  )
}

# TODO: I don't know how to detect the use scale_x_log10() to adjust the logDose argument