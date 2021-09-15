#' Print method for class est_performance
#'
#' @param x an object of class est_performance to be printed
#' @param ... additional arguments to be passed to \code{print}
#'
#' @export

print.est_performance <- function(x, ...) {
  hid <- attr(x, "hidden")
  print(x[!names(x) %in% hid], ...)
}
