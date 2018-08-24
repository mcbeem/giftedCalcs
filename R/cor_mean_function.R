#' Correlation between each assessment and the weighted mean of the assessments
#'
#' This function calculates the correlations between each asssessment and the
#'  weighted mean of the assessments. If each consistuent assessment is a
#'  potential basis for nomination, and the weighted mean of all of the assessments
#'  constitutes the confirmatory test, the returned correlations can be interpreted
#'  as the nomination validity coefficients that would result from selecting each
#'  instrument for nomination.
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
#' cor_mean(r=c(.4, .7, .9), w=c(1,2,3))
#'
#' cor_mean(r=matrix(c( 1,.4,.7,
#'                       .4, 1,.9,
#'                       .7,.9, 1), 3,3, byrow=TRUE))
#'
#' @export

cor_mean <- function(r, w=NA) {

  # make sure that r is either a vector or matrix
  if (!is.vector(r) & !is.matrix(r)) {stop("r must be a correlation matrix or a vector of unique correlations")}

  if (is.vector(r)) {

    # make sure the vector r contains admissible values
    if (min(r) < -1 | max(r) >= 1) {stop("r contains an out-of-range correlation value")}
    if (max(r) == 1) {warning("r contains one or more values of 1. The vector of unique correlations provided to this function should not include the 1s from the diagonal. Ensure that the values in r are intended.")}

    # first, find the number of assessments from the set of correlations
    p=1
    while (p^2 < 2*length(r)) {
      p <- p + 1
    }

    #now build the correlation matrix
    cov <- matrix(1, p, p)
    cov[lower.tri(cov)] <- r
    t.cov <- t(cov)
    cov[upper.tri(cov)] <- t.cov[upper.tri(t.cov)]

    unique.r <- r

  }

  if (is.matrix(r)) {

    # make sure that r is a square correlation matrix with 1s on the diagonal
    if ((dim(r)[1] != dim(r)[2]) | (max(diag(r) != rep(1, dim(r)[1])))) {stop("r must be a square correlation matrix with ones on the diagonal or a vector of unique correlations")}
    cov <- r
    p <- nrow(cov)
    unique.r <- r[lower.tri(r)]
  }

  # make sure that the correlation matrix is positive definite
  if (matrixcalc::is.positive.definite(cov)==FALSE) stop("correlation matrix is not positive definite")

  # if no weights were provided, create a vector of equal weights
  if (is.na(min(w))) {w <- rep(1/nrow(cov), times=nrow(cov))}

  # check weights
  if (min(w)<0) {stop("Weights must be positive")}

  # normalize the weights
  w <- w / sum(w)

  # stop if the number of weights and the number of assessments is not compatible
  if (choose(length(w), 2) != nrow(cov)) {stop("Either the wrong number of weights or the wrong number of correlations was given")}

  #define vector to hold results
  corrs <- vector()

  for (i in 1:nrow(cov)) {
    corrs[i] <- sum(cov[i,]*w) /  sqrt(sum(w^2)+2*sum(combn(w, 2, prod)*unique.r))
  }

  return(corrs)
}
