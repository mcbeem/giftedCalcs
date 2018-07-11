#' The conditional mean and variance of the observed scores given
#'  the true score for a single-stage identification system
#'
#' \code{conditional_moments_1stage} calculates the conditional mean and variance
#'  of the observed score (upon which identification decisions are based) given a
#'  student's true score, and the test reliability. Under classical test theory,
#'  the observed score X = T + E. Since E follows a normal distribution, the observed
#'  scores for a student with a particular true score are normally distributed
#'  around the true score, but regressed to the mean depending on the test reliability. This
#'  function returns the moments of f(X|T=t), where f() is the normal probability density
#'  function. One can imagine this as the idealized distribution of observed scores that
#'   would occur if a student with a given true score took the test an infinite number of times.
#'
#' This function returns a list containing the following:
#'
#'  conditional.mean: The mean of the normal distribution of the observed scores.
#'
#'  conditional.var: The variance of the normal distribution of the observed scores.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param true.score The student's true score on a standardized (z-score) metric.
#'
#' @examples
#' # test reliability is .9, and the student's
#' # true score is 2 SDs above the mean
#' #
#' conditional_moments_1stage(relyt=.9, true.score=2)
#'
#' @export

conditional_moments_1stage <- function(relyt, true.score) {

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
  conditional.var = sigma.xx - sigma.xy %*% solve(sigma.yy) %*% sigma.yx

  return(list(
    conditional.mean=conditional.mean,
    conditional.var=conditional.var))
}
