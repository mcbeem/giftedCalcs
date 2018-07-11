#' Conditional mean vector and covariance matrix
#'  of the observed nomination and confirmatory test scores given the true
#'  score for a two-stage identification system
#'
#' \code{conditional_moments_2stage} calculates the conditional mean vector and
#'  covariance matrix of the bivarate normal distribution of the observed nomination
#'  and confirmatory test scores, given the student's true ability score and the system
#'  parameters. These parameters include the test reliability and the nomination validity.
#'
#' This function returns a list containing the following:
#'
#'  conditional.mean: The mean vector of the bivariate normal distribution
#'   of the observed nomination and confirmatory test scores, in that order.
#'
#'  conditional.var: The covariance matrix of the bivariate normal distribution
#'    of the observed nomination and confirmatory test scores, in that order.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param true.score The student's true score on a standardized (z-score) metric.
#'
#' @examples
#' # test reliability is .9, the student's true score
#' # is 2 SDs above the mean, and the nomination validity is
#' # 0.6
#' #
#' conditional_moments_2stage(relyt=.9, true.score=2, valid=.6)
#'
#' @export

conditional_moments_2stage <- function(relyt, valid, true.score) {

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

  return(list(
    conditional.mean=conditional.mean,
    conditional.cov=conditional.cov))
}

