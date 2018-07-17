#' Marginal performance statistics identification systems
#'
#' \code{marginal_psychometrics} calculates marginal performance statistics for
#'  one- and two-stage identification systems. A two-stage system is one in which
#'  an initial nomination process is used to select students who are tested on
#'  the confirmatory assessment.
#'
#' Two-stage system results are reported if arguments \code{valid} and \code{nom.cutoff}
#'  are provided. Otherwise, one-stage results are reported.
#'
#' The function returns a list containing the following:
#'
#'  sensitivity: The proprtion of qualifying students who are identified.
#'
#'  IIR: Incorrect identification rate; the proportion of identified students
#'    who do not qualify.
#'
#'  nom.rate: Nomination rate; the proportion of students who are nominated, which controls
#'    testing costs. (Two-stage only).
#'
#'  nom.passrate: The proportion of nominated students who go on to be identified.
#'    (Two-stage only)
#'
#'  identification.rate: The proportion of the student population that is identified.
#'
#' @usage \code{marginal_psychometrics(relyt, test.cutoff, mu, valid, nom.cutoff)}
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#'
#' @examples
#' # one-stage system
#' marginal_psychometrics(mu=0, test.cutoff=.9, relyt=.95)
#'
#' # two-stage system
#' marginal_psychometrics(mu=0, test.cutoff=.9,
#'         nom.cutoff=.8, relyt=.95, valid=.7)
#' @export

marginal_psychometrics <- function(relyt, test.cutoff, valid=1e-7, nom.cutoff=1e-7, mu=0) {


  # select 1- or 2-stage version based on the supplied arguments
  if (valid==1e-7 & nom.cutoff==1e-7) {stages=1} else {stages=2}

  if(stages==2) {
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

  if(stages==1) {

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
}

#marginal_psychometrics <- Vectorize(marginal_psychometrics, SIMPLIFY=F, USE.NAMES=F)
