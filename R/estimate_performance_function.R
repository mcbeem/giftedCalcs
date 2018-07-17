#' Estimate psychometric parameters of an identification system given scores
#'  of the identified students using bootstrapping to provide uncertainty estimates.
#'
#' \code{estimate_performance} estimates the psychometric parameters of an identification system
#'  given the scores of the identified students. This function calculates statistics
#'  using the \code{estimate_parms} function with bootstrapping. The resulting \code{relyt},
#'  \code{test.cutoff}, and \code{valid} estimates are passed to the \code{marginal_psychometrics}
#'  function in order to calculate the implied performance statistics.
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
#' @param pop.mean The known general population standard deviation of the scores.
#'  Defaults to 1.
#' @param CI The confidence limit. Must be between 0 and 1. Defaults to .95.
#' @param ... optional parameters passed to the boot::boot() function. The \code{ncpus} and
#'  \code{parallel} arguments can be specified to increase performance on multiprocessor
#'  hardware.
#'
#' @examples
#' # generate some data
#' set.seed(1)
#' scores <- r_identified(n=500, test.cutoff=.9, relyt=.93, valid=.6,
#'   nom.cutoff=.85)
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.6,
#'   nom.cutoff=.85)$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.6,
#'   nom.cutoff=.85)$nom.rate
#'
#' # estimate the system parameters with 200 bootstrapped samples
#' a <- estimate_performance(scores=scores, id.rate=id.rate,
#'   nom.rate=nom.rate, reps=200)
#' a
#'
#' # plot the results for each parameter
#' plot(a)
#'
#' @export

estimate_performance <- function(scores, id.rate, nom.rate, reps, pop.mean=0,
                                   pop.sd=1, adjust=.5, CI=.95, ...) {

     bootdata <- boot2(data=scores, statistic=estimate_parms, R=reps, id.rate=id.rate,
                           nom.rate=nom.rate, ...)
     # a contains the samples returned from boot
     a <- bootdata$t

     # b contains the result of feeding the samples in a to the marginal_psychometrics
     #  function (calc sensitivity etc)
     b <- mapply(marginal_psychometrics, a[,1], a[,2],
                 valid=a[,3], nom.cutoff=a[,4])
     b <- matrix(unlist(b), nrow=reps, ncol=5, byrow=T)

     # column names
     nms <-  c("relyt", "test.cutoff", "valid", "nom.cutoff",
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
