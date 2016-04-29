#' @import dplyr

# Creating a 4PL regression curve layer for ggplot
# Adapted from the example http://docs.ggplot2.org/dev/vignettes/extending-ggplot2.html

# Eric Koncina 2016

Stat4PL <- ggplot2::ggproto("Stat4PL", ggplot2::Stat, 
                            required_aes = c("x", "y"),
                            compute_group = function(data, scales, logDose, n, x.range) {
                              x.range <- scales$x$get_limits()
                              if (isTRUE(logDose)){
                                if (scales$x$trans$name == "log-10") {
                                  warning("adjusting drc::drm logDose to 10")
                                  logDose <- 10
                                }
                                else logDose <- NULL
                              }
                              model.4pl <- drc::drm(y ~ x, data = data, fct = drc::LL.4(), logDose = logDose)
                              grid <- data.frame(x = seq(x.range[1], x.range[2], length.out = n))  %>%
                                mutate(y = predict(model.4pl, .))
                              grid
                            }
)

#' Draws a 4-PL regression line
#'
#' stat_4pl() performs a 4 parameter logistic regression (using drc::drm) for plotting in ggplot2.
#' @inheritParams ggplot2::stat_identity
#' @param logDose a numeric value or NULL. Argument to be passed to \code{drc::drm}. See \code{?drc::drm} for more informations. If no value is set, \code{stat_4pl} detects if \code{scale_x_log10()} has been used and adjusts logDose to 10.
#' @param n Number of points used for interpolation.
#' @export
stat_4pl <- function(mapping = NULL, data = NULL, geom = "line",
                     position = "identity", na.rm = FALSE, show.legend = NA, 
                     inherit.aes = TRUE, logDose = NULL, n = 100, x.range = NULL, ...) {
  if (!"drc" %in% rownames(installed.packages())) stop("could not find drc library")
  if (!is.null(logDose) && !is.numeric(logDose)) stop("bad logDose argument. See ?drc::drm")
  if (missing(logDose)) logDose <- TRUE
  layer(
    stat = Stat4PL, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(logDose = logDose, n = n, x.range = x.range, na.rm = na.rm, ...)
  ) 
}
