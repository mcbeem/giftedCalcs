#' Plot method for class est_performance
#'
#' @param width Adjusts the scale of the x-axis. Defaults to 0.3.
#' @param adjust Adjusts the smoothing bandwidth for density estimation. Defaults to 1.2.
#' @param type A character string selecting density plots or histograms. Must be one of
#'  "density" or "hist"
#' @param x an object of class est_performance to be plotted
#' @param ... additional arguments to be passed to \code{plot}
#'
#' @export

plot.est_performance <- function(x, width = .3, adjust = 1.2, type = "density", ...) {
  if (!type %in% c("density", "hist")) {
    stop("type must be \"density\" or \"hist\"")
  }
  if (width < 0) {
    stop("width must be greater than zero")
  }
  if (adjust <= 0 & type == "density") {
    stop("adjust must be greater than zero")
  }

  old.mfrow <- par()$mfrow
  old.mar <- par()$mar

  par(mfrow = c(3, 1))
  par(mar = c(2.3, 2, 2, 1))

  if (type == "density") {
    d <- apply(x$samples[, -c(4, 7)], 2, density, adjust = adjust)
    plot(d$sensitivity,
      main = "sensitivity", yaxt = "n",
      xlim = c(
        max(0, min(x$samples$sensitivity) - width),
        min(1, max(x$samples$sensitivity) + width)
      ), ...
    )
    plot(d$nom.passrate,
      main = "nomination pass rate", yaxt = "n",
      xlim = c(
        max(0, min(x$samples$nom.passrate) - width),
        min(1, max(x$samples$nom.passrate) + width)
      ), ...
    )
    plot(d$valid,
      main = "nomination validity", yaxt = "n",
      xlim = c(
        max(0, min(x$samples$valid) - width),
        min(1, max(x$samples$valid) + width)
      ), ...
    )
  }
  if (type == "hist") {
    hist(x$samples$sensitivity,
      main = "sensitivity", yaxt = "n",
      xlim = c(
        max(0, min(x$samples$sensitivity) - width),
        min(1, max(x$samples$sensitivity) + width)
      ), ...
    )
    hist(x$samples$nom.passrate,
      main = "nomination pass rate", yaxt = "n",
      xlim = c(
        max(0, min(x$samples$nom.passrate) - width),
        min(1, max(x$samples$nom.passrate) + width)
      ), ...
    )
    hist(x$samples$valid,
      main = "nomination validity", yaxt = "n",
      xlim = c(
        max(0, min(x$samples$valid) - width),
        min(1, max(x$samples$valid) + width)
      ), ...
    )
  }
  par(mfrow = old.mfrow)
  par(mar = old.mar)
}
