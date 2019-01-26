#' Print method for class perf_by_cutoff
#'
#' @param x an object of class perf_by_cutoff to be printed
#' @param ... additional arguments to be passed to \code{print}
#'
#' @export

print.perf_by_cutoff <- function (x, ...) {
  hid <- attr(x, "hidden")
  print(x[!names(x) %in% hid], ...)
}
