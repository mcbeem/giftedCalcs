#' Estimate psychometric parameters of an identification system given scores
#'  of the identified students using bootstrapping to provide uncertainty estimates.
#'
#' @return This function stimates the psychometric parameters of an identification system
#'  given the scores of the identified students. This function calculates statistics
#'  using the \code{\link{estimate_parms}} function with bootstrapping. The resulting
#'  \code{relyt}, \code{test.cutoff}, and \code{valid} estimates are passed to the
#'  \code{\link{marginal_psychometrics}} function in order to calculate the implied
#'  performance statistics.
#'
#'  Simulation results suggest a mimimum sample size of n=150 to n=200 for reasonable
#'   performance. Further, at least 500 bootstrapped samples are suggested.
#'   This can take a while.
#'
#' @param scores Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param id.rate The proportion of students who have been identified. Range (0, 1). Must
#'  be less than or equal to \code{nom.rate}.
#' @param nom.rate The proportion of students who have been nominated. Range (0, 1). Used to
#'  calculate the nomination cutoff.
#' @param reps The number of bootstrap samples.
#' @param pop.mean The known general population mean of the scores. Defaults to 0.
#' @param pop.sd The known general population standard deviation of the scores.
#'  Defaults to 1.
#' @param adjust Number that controls the amount of smoothing in the density
#'   estimation. Defaults to 0.5. Values much larger than this result in biased estimates.
#' @param CI The confidence limit. Must be between 0 and 1. Defaults to .95.
#' @param ... optional parameters passed to the boot::boot() function. The \code{ncpus} and
#'  \code{parallel} arguments can be specified to increase performance on multiprocessor
#'  hardware.
#'
#' @examples
#' # generate some data
#' set.seed(123)
#' scores <- r_identified(n=500, test.cutoff=.9, relyt=.93, valid=.5,
#'   nom.cutoff=.9)
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.5,
#'   nom.cutoff=.9)$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.5,
#'   nom.cutoff=.9)$nom.rate
#'
#' # estimate the system parameters with 200 bootstrapped samples
#' #  this uses one CPU
#' a <- estimate_performance(scores=scores, id.rate=id.rate,
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

estimate_performance <- function(scores, id.rate, nom.rate, reps, pop.mean=0,
                                   pop.sd=1, adjust=.5, CI=.95, ...) {

     bootdata <- pbboot(data=scores, statistic=estimate_parms, R=reps, id.rate=id.rate,
                           nom.rate=nom.rate, ...)
     # a contains the samples returned from boot
     a <- bootdata$t

     #boundary estimates usually indicate convergence problems
     for (i in 1:nrow(a)) {
       if ((min(abs(a[i,1:3] - c(.999, .999, -.001))) == 0) ||
           (min(abs(a[i,1:3] - c(.5, .5, -.7))) == 0)) {
         a[i,] <- rep(NA, 4)}
     }

      a <- a[complete.cases(a),]

     # b contains the result of feeding the samples in a to the marginal_psychometrics
     #  function (calc sensitivity etc)
     b <- mapply(marginal_psychometrics, test.cutoff=a[,1], relyt=a[,2],
                 valid=a[,3], nom.cutoff=a[,4])
     b <- matrix(unlist(b), nrow=nrow(a), ncol=5, byrow=T)

     # column names
     nms <-  c("test.cutoff", "relyt", "valid", "nom.cutoff",
               "sensitivity", "IIR", "nom.rate", "nom.passrate", "identification.rate")

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
     summary[c(4,7), c(2:4)] <- NA
     summary[9, c(2:4)] <- NA

     # re-order the rows
     summary <- summary[c(1,2,3,5,6,8,4,7,9),]

     if (reps < 500) {
       warning("A minimum of 500 reps is suggested for trustworthy standard errors and confidence intervals.")
       }

     output <- list(samples=samples, summary=summary, boot.output=bootdata)
     class(output) <- c("est_performance", "list")
     # don't print the samples or the boot output
     attr(output, "hidden") <- c("samples", "boot.output")

     return(output)
}
