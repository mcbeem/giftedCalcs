#' The variance of the weighted mean of assessments
#'
#' This function calculates the variance of the weighted mean of a set of unit-standardized
#' assessments. When the assessments are imperfectly correlated, this variance will
#' be less than one. The identification cut score must be adjusted accordingly to
#' maintain the desired percentile cutoff.
#'
#' The value returned by this function can be interpreted as the shrinkage factor
#' of the variance of the weighted mean. The square root of this value is the shrinkage
#' factor of the standard deviation of the weighted mean.
#'
#' @param r Either a correlation matrix or a vector of unique correlations. If the weights
#'  are not all equal, it is recommended to specify the correlations as a matrix to avoid
#'  erronous pairings of assessment correlations and weights, since this can be confusing
#'  if the correlations are supplied as a vector.
#' @param w A vector of weights. Will be internally normalized to sum to 1 and presumes the
#'  same order of assessments as the correlation matrix. If omitted, it is assumed that all
#'  assessments have the same weight.
#'
#' @examples
#' var_mean(r = c(.4, .7, .9), w = c(1, 2, 3))
#'
#' var_mean(r = matrix(c(
#'   1, .4, .7,
#'   .4, 1, .9,
#'   .7, .9, 1
#' ), 3, 3, byrow = TRUE))
#' @export

var_mean <- function(r, w = NA) {

  # calculate the number of assessments
  if ("matrix" %in% class(r)) {
    n_scores <- nrow(r)
  } else if ("numeric" %in% class(r)) {
    n_scores <- choose(length(r), 2)
  }

  # if no weights were provided, create a vector of equal weights
  if (is.na(min(w))) {
    w <- rep(1 / n_scores, times = n_scores)
  }

  # check weights
  if (min(w) < 0) {
    stop("Weights must be positive")
  }

  # normalize the weights
  w <- w / sum(w)

  # make sure r is either a vector or matrix
  if (!is.vector(r) & !is.matrix(r)) {
    stop("r must be a correlation matrix or a vector of unique correlations")
  }

  if (is.vector(r)) {
    # check that r contains valid correlation values
    if (min(r) < -1 | max(r) >= 1) {
      stop("r contains an out-of-range correlation value")
    }
    # if r is supplied as a vector, 1s should not be included
    if (max(r) == 1) {
      warning("r contains one or more values of 1. The vector of unique correlations provided to this function should not include the 1s from the diagonal. Ensure that the values in r are intended")
    }

    # make sure that the length of r is compatible with choose(n,2)
    if (!(length(r) %in% choose(seq(2, 100), 2))) {
      stop("length of vector r is incorrect")
    }

    # find the number of assessments from the set of correlations
    p <- 1
    while (p^2 < 2 * length(r)) {
      p <- p + 1
    }

    # check that the lengths of r and weights are compatible
    if (min(c(p, length(w)) == rep(length(w), 2)) == 0) {
      stop("The number of assessments implied by the length of r, the number of weights, and the number of reliability coefficients must be the same")
    }
    # now build the correlation matrix
    cov <- matrix(1, p, p)
    cov[lower.tri(cov)] <- r
    t.cov <- t(cov)
    cov[upper.tri(cov)] <- t.cov[upper.tri(t.cov)]

    unique.r <- r
  }

  if (is.matrix(r)) {
    # check that r is square and has 1s on the diagonal
    if ((dim(r)[1] != dim(r)[2]) | (max(diag(r) != rep(1, dim(r)[1])))) {
      stop("r must be a square correlation matrix with ones on the diagonal or a vector of unique correlations")
    }

    # check that r is symmetric
    if (!isSymmetric(r)) {
      stop("the correlation matrix r must be symmetric")
    }

    cov <- r
    p <- nrow(cov)
    unique.r <- r[lower.tri(r)]
  }

  # check that correlation matrix is positive definite
  if (matrixcalc::is.positive.definite(cov) == FALSE) stop("correlation matrix is not positive definite")

  var.m <- sum(w^2) + 2 * sum(combn(w, 2, prod) * unique.r)

  return(var.m)
}
