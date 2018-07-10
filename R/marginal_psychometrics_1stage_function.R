#' Function for calculating marginal performance statistics for a single-stage 
#'  identification system
#'
#' \code{marginal_psychometrics_1stage} calculates marginal performance statistics 
#'  for a single-stage identification system with a single assessment. The results 
#'  are valid if all students are tested on the confirmatory assessment. If only a
#'  subset are tested, use function \code{marginal_psychometrics_2stage}.
#'
#' This function returns a list containing the following:
#' 
#'  sensitivity: The proprtion of qualifying students who are identified.
#'  
#'  IIR: Incorrect identification rate; the proportion of identified students 
#'    who do not qualify.
#'  
#'  identification.rate: The proportion of the student population that is identified.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero. 
#' 
#' @examples
#' # the population mean ability is zero, the test cutoff is 
#' # at the 90th percentile, and the confirmatory test 
#' reliability is .9
#'
#' marginal_psychometrics_1stage(mu=0, 
#'  test.cutoff=.9, relyt=.9)
#' 
#' @export

marginal_psychometrics_1stage <- function(relyt, test.cutoff, mu=0) {
  
  errortrapping(relyt=relyt, test.cutoff=test.cutoff, mu=mu)
  
  # order:  t true, t obs
  cov <- matrix(c(
    1, sqrt(relyt),
    sqrt(relyt), 1),
    nrow=2, ncol=2)
  
  # cutoff
  tau <- qnorm(test.cutoff, 0, 1)
  
  # order:  t true, t obs
  means <- c(mu, mu*sqrt(relyt))
  
  # Sensitivity
  sensitivity <- mnormt::sadmvn(lower=c(tau, tau), upper=c(5, 5),
                                mean=means,
                                varcov=cov, maxpts=10*100000)[[1]] /
    mnormt::sadmvn(lower=c(tau, -5), upper=c(5,5),
                   mean=means,
                   varcov=cov, maxpts=10*100000)[[1]]
  
  # incorrect identification rate
  IIR <- mnormt::sadmvn(lower=c(-5, tau), upper=c(tau, 5),
                        mean=means, varcov=cov, maxpts=10*10000)[[1]] /
    mnormt::sadmvn(lower=c(-5, tau), upper=c(5, 5),
                   mean=means, varcov=cov, maxpts=10*10000)[[1]]
  
  # proportion identified
  identification.rate <- mnormt::sadmvn(lower=c(-5, tau), upper=c(5, 5),
                                        mean=means, varcov=cov, maxpts=10*100000)[[1]]
  
  return(list(sensitivity=sensitivity, IIR=IIR,
              identification.rate=identification.rate))
}
