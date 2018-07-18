#' Conditional moments of the observed score distribution given the true score
#'  and identification system parameters
#'
#'  This function calculates the mean and variance of observed scores
#'  conditional on the true score and the identification system parameters. In a
#'  one-stage identification system, the only observed score is the observed test
#'  score, and so the mean and variance are scalar values. The distribution of
#'  observed scores is assumed to be normal.
#'
#'  In a two-stage identification system, there are observed scores for
#'  both the nomination and the confirmatory test. In this case, there is a mean vector
#'  and a covariance matrix, and the joint distribution is assumed to be bivariate
#'  normal. A two-stage system is one in which an initial nomination process is used
#'  to select students who are tested on the confirmatory assessment.
#'
#' The observed score distribution for a student with a specific true score is justified
#'  by classical test theory, in which measurement errors are random variables. Because
#'  observed scores are true score (T) plus random error (E), the observed scores are
#'  also random variables.
#'
#' If the argument valid is supplied, the two-stage conditional moments are
#'  returned. Otherwise the one-stage moments are returned.
#'
#' This function returns a list containing the following:
#'
#'  conditional.mean: For one-stage systems, the mean of the confirmatory test scores.
#'  For two-stage systems, the mean vector of the bivariate normal distribution of
#'  the observed nomination and confirmatory test scores, in that order.
#'
#'  conditional.cov: For one-stage systems, the variance of the observed confirmatory
#'   test scores. For two-stage systems, the covariance matrix of the bivariate
#'   normal distribution of the observed nomination and confirmatory test scores,
#'   in that order.
#'
#' @usage conditional_moments(true.score, relyt, valid)
#'
#' @param true.score The student's true score on a standardized (z-score) metric.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability. If provided, the two-stage
#'  version of the computation is performed.
#'
#' @examples
#'
#' # one-stage system
#' conditional_moments(relyt=.9, true.score=2)
#'
#' # two-stage system
#' conditional_moments(relyt=.9, true.score=2, valid=.6)
#' @export

conditional_moments <- function(true.score, relyt, valid=0) {


  if(valid > 1e-7) {

    errortrapping(relyt=relyt, valid=valid)

    # order:  n obs, t obs, t true
    means <- c(0, 0, 0)

    sigma.xx = matrix(c(1, valid, valid, 1), nrow=2, byrow=T)
    sigma.yy = 1
    sigma.xy = matrix(c(valid/sqrt(relyt), sqrt(relyt)), nrow=2, byrow=T)
    sigma.yx = t(sigma.xy)

    # conditional means of nom true, nom obs, and test obs
    conditional.mean = means[1:2] + sigma.xy %*% solve(sigma.yy) * (true.score - means[3])
    # conditional covariance matrix of nom true, nom obs, and test obs
    conditional.cov = sigma.xx - sigma.xy %*% solve(sigma.yy) %*% sigma.yx
  }

  if (valid <= 1e-7) {
    errortrapping(relyt=relyt)

    # marginal mean of the true [1] and observed [2] scores
    means <- c(0, 0)

    sigma.xx = 1
    sigma.yy = 1
    sigma.xy = sqrt(relyt)
    sigma.yx = t(sqrt(relyt))

    # conditional means of nom true, nom obs, and test obs
    conditional.mean = means[2] + sigma.xy * solve(sigma.yy) * (true.score - means[1])
    # conditional covariance matrix of nom true, nom obs, and test obs
    conditional.cov = sigma.xx - sigma.xy %*% solve(sigma.yy) %*% sigma.yx
  }

  return(list(
    conditional.mean=conditional.mean,
    conditional.cov=conditional.cov))
}
