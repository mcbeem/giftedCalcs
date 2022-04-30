#' Reliability of the weighted mean of assessments
#'
#' This function calculates the reliability of a weighted mean of assessments given the
#'  reliability coefficient of each assessment, the correlations between them, and the
#'  weights.
#'
#' @param rely A vector of reliability coefficients.
#' @param r Either a correlation matrix or a vector of unique correlations. It is
#'  recommended to specify the correlations as a matrix to avoid
#'  erronous pairings of assessment correlations with reliability coefficients and weights,
#'  since this can be confusing if the correlations are supplied as a vector. Presumes that the
#'  correlation matrix has the same order of assessments as the reliability and weight vectors.
#' @param w A vector of weights. Will be internally normalized to sum to 1 and presumes the
#'  same order of assessments as the correlation matrix and vector of reliabilities.
#'  If omitted, it is assumed that all assessments have the same weight.
#'
#' @examples
#' r <- matrix(c(
#'   1, .4, .7,
#'   .4, 1, .5,
#'   .7, .5, 1
#' ), 3, 3, byrow = TRUE)
#' reliability_mean(rely = c(.9, .85, .88), r = r, w = c(.5, .3, .2))
#' @export

reliability_mean <- function(rely, r, w = NA) {

  # if no weights were provided, create a vector of equal weights
  if (is.na(min(w))) {
    w <- rep(1 / length(rely), times = length(rely))
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

  # make sure reliability coefficients are between zero and one
  if (min(rely) < 0 | max(rely) >= 1) {
    stop("rely contains an out-of-range value. reliability coefficients must be between zero and one.")
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

    # check that the lengths of r, weights, and rely are compatible
    if (min(c(p, length(rely), length(w)) == rep(length(w), 3)) == 0) {
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

  # check that no correlation exceeds the sqrt of the prod of the reliabilities
  checkmat <- sqrt(as.matrix(rely) %*% t(as.matrix(rely)))
  diag(checkmat) <- 1
  if (min(checkmat - cov) < 0) {
    stop("a correlation is larger than the square root of the product of the involved reliability coefficients")
  }

  # corrs[i] <- sum(r[i,]*w) /  sqrt(sum(w^2)+2*sum(combn(w, 2, prod)*unique.r))
  rely.m <- (sum(rely * w^2) + 2 * sum(combn(w, 2, prod) * unique.r)) / (sum(w^2) + 2 * sum(combn(w, 2, prod) * unique.r))

  return(rely.m)
}
