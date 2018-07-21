#' Estimate psychometric parameters of an identification system given x
#'  of the identified students using bootstrapping to provide uncertainty estimates.
#'
#' @return This function stimates the psychometric parameters of an identification system
#'  given the x of the identified students. This function calculates statistics
#'  using the \code{\link{estimate_valid}} function with bootstrapping. The resulting
#'  nomination validity (\code{valid}) estimates are passed to the
#'  \code{\link{marginal_psychometrics}} function in order to calculate the implied
#'  performance statistics.
#'
#'  At least 500 bootstrapped samples are suggested. This can take a while.
#'
#' @param x Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param id.rate The proportion of students who have been identified. Range (0, 1). Must
#'  be less than or equal to \code{nom.rate}.
#' @param nom.rate The proportion of students who have been nominated. Range (0, 1). Used to
#'  calculate the nomination cutoff.
#' @param reps The number of bootstrap samples.
#' @param relyt Confirmatory test reliability coefficient. Range (0,1]. Must not be exactly 0.
#'  Used in the calculation of sensitivity. Defaults to 1, in which case the reported
#'  sensitivity is relative to a universal screening system and does not include sensitivity
#'  loss from imperfect test reliability. If <1, the test reliability is included in the
#'  sensitivity calculation.
#' @param pop.mean The known general population mean of the x. Defaults to 0.
#' @param pop.sd The known general population standard deviation of the x.
#'  Defaults to 1.
#' @param adjust Number that controls the amount of smoothing in the density
#'   estimation. Defaults to 1.0, which has been found to work well in simulation.
#' @param CI The confidence limit. Must be between 0 and 1. Defaults to .95.
#'
#' @examples
#' # generate some data
#' set.seed(123)
#' x <- r_identified(n=300, test.cutoff=.9, valid=.5,
#'   nom.cutoff=.9)
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(test.cutoff=.9, valid=.5,
#'   nom.cutoff=.9)$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(test.cutoff=.9, valid=.5,
#'   nom.cutoff=.9)$nom.rate
#'
#' # estimate the system parameters with 200 bootstrapped samples
#' #  this uses one CPU
#' a <- estimate_performance(x=x, id.rate=id.rate,
#'   nom.rate=nom.rate, reps=200)
#' a
#'
#' # plot the results for each parameter
#' plot(a)
#'
#' # plot with histograms
#' plot(a, type="hist")
#'
#' # the width argument controls the x-limits
#' plot(a, type="hist", width=.0)
#'
#' # for type="density", the bandwidth is adjustable
#' #  (default is 1.2, see ?density for details)
#' plot(a, type="density", width=.05, adjust=.5)
#' plot(a, type="density", width=.05, adjust=1.5)
#'
#' @export

estimate_performance <- function(x, id.rate, nom.rate, reps, relyt=1, pop.mean=0,
                                   pop.sd=1, adjust=1.0, CI=.95) {

      a <- boot_estimate_valid(x=x, reps=reps, id.rate=id.rate,
                         nom.rate=nom.rate, pop.mean=pop.mean, pop.sd=pop.sd,
                         adjust=adjust)

      a <- matrix(a, ncol=1)
     # a contains the samples returned from boot

      a <- na.omit(a)

     #boundary estimates usually indicate convergence problems
     for (i in 1:length(a)) {
       if ((min(abs(a[i,] - c(.999))) == 0) ||
           (min(abs(a[i,] - c(.001))) == 0)) {
         a[i,] <- NA}
     }

      test.cutoff <- pnorm(min(x))
      nom.cutoff <- 1-nom.rate

      a <- na.omit(a)
      a <- matrix(c(
                  rep(test.cutoff, times=length(a)),
                  rep(relyt, times=length(a)),
                  valid=a,
                  rep(nom.cutoff, times=length(a))), ncol=4)

     # b contains the result of feeding the samples in a to the marginal_psychometrics
     #  function (calc sensitivity etc)
     b <- mapply(marginal_psychometrics, test.cutoff=test.cutoff, relyt=relyt,
                 valid=a[,3], nom.cutoff=nom.cutoff)[1:4,] # drop estimated id.rate
     b <- matrix(unlist(b), nrow=nrow(a), ncol=4, byrow=T)


     if (relyt != 1) {

      b[,1] <- b[,1]*marginal_psychometrics(test.cutoff, relyt=relyt)$sensitivity
      note <- "Sensitivity is achieved sensitivity accounting for test reliability."

     } else {
       note <- "Since the test reliability was not specified, the sensitivity value is interpreted as relative to universal screening. Achieved sensitivity is dependent on the test reliability."
     }

     nms <-  c("test.cutoff", "relyt", "valid", "nom.cutoff",
               "sensitivity", "IIR", "nom.rate", "nom.passrate")

     # object samples is a data frame containing all the sampled values
     samples <- data.frame(cbind(a,b))
     names(samples) <- nms

     # calculate the means, SEs, and CI boundaries
     means <- matrix(apply(samples, 2, mean, na.rm=T), ncol=1)
     stderr <- matrix(apply(samples, 2, sd, na.rm=T), ncol=1)
     CI_ll <- matrix(means+qnorm((1-CI)/2)*stderr, ncol=1)
     CI_ul <- matrix(means+qnorm(1-((1-CI)/2))*stderr, ncol=1)

     # create the summary object for printing
     summary <- round(cbind(means, stderr, CI_ll, CI_ul),5)
     summary <- data.frame(summary)
     row.names(summary) <- nms

     names(summary) <- c("Estimate", "StdErr", paste0("CI.", CI*100, ".lower"),
                         paste0("CI.", CI*100, ".upper"))
     # fixed values should have NAs for stderr and CIs
     # id.rate is known and therefore fixed
     summary[c(1,2,4,7), c(2:4)] <- NA

     # re-order the rows
     summary <- summary[c(3,5,8,1,4),]

     if (reps < 500) {
       warning("A minimum of 500 reps is suggested for trustworthy standard errors and confidence intervals.")
     }

     if (nrow(samples) < reps) {
       warning(paste0(reps-nrow(samples), " of ", reps, " bootstrapped estimates did not converge."))
     }

     output <- list(samples=samples, summary=summary, note=note)
     class(output) <- c("est_performance", "list")
     # don't print the samples or the boot output
     attr(output, "hidden") <- c("samples")

     return(output)
}


