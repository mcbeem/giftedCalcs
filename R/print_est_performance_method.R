#' Print method for class est_performance
#' @export

print.est_performance <- function (x) {
  hid <- attr(x, "hidden")
  print(x[!names(x) %in% hid])
}
