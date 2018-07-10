#' Function for calculating marginal performance statistics for a two-stage 
#'  identification system.
#'
#' \code{marginal_psychometrics_2stage} calculates marginal performance statistics 
#'  for a two-stage identification system, in which an initial nomination process 
#'  is used to select students who are tested on the confirmatory assessment. 
#'
#' This function returns a list containing the following:
#' 
#'  sensitivity: The proprtion of qualifying students who are identified.
#'  
#'  IIR: Incorrect identification rate; the proportion of identified students 
#'    who do not qualify.
#'    
#'  nom.rate: Nomination rate; the proportion of students who are nominated, which controls 
#'    testing costs.
#'    
#'  nom.passrate: The proportion of nominated students who go on to be identified.
#'  
#'  identification.rate: The proportion of the student population that is identified.
#'  
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1. 
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero. 
#' 
#' @examples
#' # the population mean ability is zero, the test cutoff is 
#' # at the 90th percentile,the nomination cutoff is at the 80th 
#' # percentile, the confirmatory test reliability
#' # is .95, and the nomination validity is .7
#'
#'  marginal_psychometrics_2stage(mu=0, test.cutoff=.9, 
#'   nom.cutoff=.8, relyt=.95, valid=.7)
#' 
#' @export

marginal_psychometrics_2stage <- function(relyt, valid, nom.cutoff, test.cutoff, mu=0) {
  
  errortrapping(relyt=relyt, valid=valid, nom.cutoff=nom.cutoff,
                test.cutoff=test.cutoff, mu=mu)
  
  # order:  t true, n obs, t obs
  cov <- matrix(c(
    1, valid/sqrt(relyt), sqrt(relyt),
    valid/sqrt(relyt), 1, valid,
    sqrt(relyt), valid, 1),
    nrow=3, ncol=3)
  
  # cutoffs
  nu <- qnorm(nom.cutoff, 0, 1)
  tau <- qnorm(test.cutoff, 0, 1)
  
  # order:  t true, n obs, t obs
  means <- c(mu, mu*valid/sqrt(relyt), mu*sqrt(relyt))
  
  # Sensitivity
  sensitivity <- mnormt::sadmvn(lower=c(tau, nu, tau), upper=c(5,5,5),
                                mean=means,
                                varcov=cov, maxpts=10*100000)[[1]] /
    mnormt::sadmvn(lower=c(tau, -5, -5), upper=c(5,5,5),
                   mean=means,
                   varcov=cov, maxpts=10*100000)[[1]]
  
  # incorrect identification rate
  IIR <- mnormt::sadmvn(lower=c(-5, nu, tau), upper=c(tau, 5, 5),
                        mean=means, varcov=cov, maxpts=10*10000)[[1]] /
    mnormt::sadmvn(lower=c(-5, nu, tau), upper=c(5, 5, 5),
                   mean=means, varcov=cov, maxpts=10*10000)[[1]]
  
  # proportion identified
  identification.rate <- mnormt::sadmvn(lower=c(-5, nu, tau), upper=c(5, 5, 5),
                                        mean=means, varcov=cov, maxpts=10*100000)[[1]]
  
  # nomination rate
  nom.rate <- 1-pnorm(qnorm(nom.cutoff)-mu)
  
  # nomination pass rate
  nom.passrate <-  mnormt::sadmvn(lower=c(-5, nu, tau), upper=c(5,5,5),
                                  mean=means, varcov=cov, maxpts=10*100000)[[1]] /
    mnormt::sadmvn(lower=c(-5, nu, -5), upper=c(5,5,5),
                   mean=means, varcov=cov, maxpts=10*100000)[[1]]
  
  return(list(sensitivity=sensitivity, IIR=IIR,
              nom.rate=nom.rate,
              nom.passrate=nom.passrate,
              identification.rate=identification.rate))
}