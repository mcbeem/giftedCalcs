#' Plot method for class est_performance
#'
#' @param width Adjusts the scale of the x-axis. Defaults to 0.3.
#' @param adjust Adjusts the smoothing bandwidth for density estimation. Defaults to 1.2.
#' @param type A character string selecting density plots or histograms. Must be one of
#'  "density" or "hist"
#'
#' @export

plot.est_performance <- function(x, width=.3, adjust=1.2, type="density", ...) {

  if (!type %in% c("density", "hist")) {stop("type must be \"density\" or \"hist\"")}
  if (width < 0) {stop("width must be greater than zero")}
  if (adjust <= 0 & type=="density") {stop("adjust must be greater than zero")}

  old.mfrow <- par()$mfrow
  old.mar <- par()$mar

  par(mfrow=c(3,2))
  par(mar=c(2.3, 2, 2, 1))

  if (type=="density") {
    d <- apply(a$samples[,-c(4,7)], 2, density, adjust=adjust)
    plot(d$sensitivity,  main="sensitivity", yaxt='n',
         xlim=c(max(0, min(a$samples$sensitivity)-width),
                min(1, max(a$samples$sensitivity)+width)))
    plot(d$relyt, main="test reliability", yaxt='n',
         xlim=c(max(0, min(a$samples$relyt)-width),
                min(1, max(a$samples$relyt)+width)))
    plot(d$IIR, main="incorrect identification rate", yaxt='n',
         xlim=c(max(0, min(a$samples$IIR)-width),
                min(1, max(a$samples$IIR)+width)))
    plot(d$test.cutoff,  main="test cutoff", yaxt='n',
         xlim=c(max(0, min(a$samples$test.cutoff)-width),
                min(1, max(a$samples$test.cutoff)+width)))
    plot(d$nom.passrate, main="nomination pass rate", yaxt='n',
         xlim=c(max(0, min(a$samples$nom.passrate)-width),
                min(1, max(a$samples$nom.passrate+width))))
    plot(d$valid,  main="nomination validity", yaxt='n',
         xlim=c(max(0, min(a$samples$valid)-width),
                min(1, max(a$samples$valid)+width)))
    }
  if (type=="hist") {
    hist(a$samples$sensitivity,  main="sensitivity", yaxt='n',
         xlim=c(max(0, min(a$samples$sensitivity)-width),
                min(1, max(a$samples$sensitivity)+width)))
    hist(a$samples$relyt, main="test reliability", yaxt='n',
         xlim=c(max(0, min(a$samples$relyt)-width),
                min(1, max(a$samples$relyt)+width)))
    hist(a$samples$IIR, main="incorrect identification rate", yaxt='n',
         xlim=c(max(0, min(a$samples$IIR)-width),
                min(1, max(a$samples$IIR)+width)))
    hist(a$samples$test.cutoff,  main="test cutoff", yaxt='n',
         xlim=c(max(0, min(a$samples$test.cutoff)-width),
                min(1, max(a$samples$test.cutoff)+width)))
    hist(a$samples$nom.passrate, main="nomination pass rate", yaxt='n',
         xlim=c(max(0, min(a$samples$nom.passrate)-width),
                min(1, max(a$samples$nom.passrate+width))))
    hist(a$samples$valid,  main="nomination validity", yaxt='n',
         xlim=c(max(0, min(a$samples$valid)-width),
                min(1, max(a$samples$valid)+width)))
  }
    par(mfrow=old.mfrow)
    par(mar=old.mar)
}
