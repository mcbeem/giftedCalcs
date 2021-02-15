#' Conditional moments of scores
#'
#'  This function calculates the conditional mean and variance of scores. The user specifies
#'  a value for the confirmatory test true score, the confirmatory test observed score, or
#'  the nomination observed score. The moments of the conditional distribution of the other
#'  two scores are calculated.
#'
#'  In a two-stage identification system, there is a test true score, a test observed score,
#'  and a nomination observed score. (In principle there is also a nomination true score, but
#'  it is irrelevant and may be ignored). In a one-stage system, there is a test true score and
#'  observed score.
#'
#' The joint distribution of these three variables is assumed to be multivariate
#' normal. Therefore, the conditional distribution is also normal.
#'
#' If the argument valid is supplied, the two-stage conditional moments are
#'  returned. Otherwise the one-stage moments are returned.
#'
#' @param t.true The confirmatory test true score on a standardized (z-score) metric. Only
#'  one of \code{t.true}, \code{t.obs}, or \code{n.obs} should be specified.
#' @param t.obs The confirmatory test observed score on a standardized (z-score) metric.
#' @param n.obs The nomination observed score on a standardized (z-score) metric. If \code{n.obs}
#'  is specified, a value for \code{valid} must also be given.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability. If provided, the two-stage
#'  version of the computation is performed.
#'
#'  @return A list containing the following:
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
#' @examples
#'
#' # one-stage system
#' conditional_moments(t.true=2, relyt=.9)
#'
#' # two-stage system
#' conditional_moments(t.true=2, relyt=.9, valid=.6)
#' @export

conditional_moments <- function(t.true=NA, n.obs=NA, t.obs=NA, relyt, valid=1e-7) {

  if (valid==1e-7 & !is.na(n.obs)) {stop("A value for argument valid must be provided when an argument is given for n.obs")}

  if(valid != 1e-7) {

    errortrapping(relyt=relyt, valid=valid)

    # index of row/column positions for the arguments supplied
    # this is the value to be conditioned on
    has <- seq(1:3)[!is.na(c(n.obs, t.obs, t.true))]

    if (length(has) != 1) {stop("Only one of n.obs, t.obs, or t.true should be provided")}

    # index of row/column positions for the remaining arguments
    wants <- seq(1:3)[-has]
    # vector of names
    nms <- c("n.obs", "t.obs", "t.true")

    cov <- matrix(c(
      1,                 valid,          valid/sqrt(relyt),
      valid,             1,              sqrt(relyt),
      valid/sqrt(relyt), sqrt(relyt),    1),
      nrow=3, ncol=3, byrow=T)

    # order:  n obs, t obs, t true
    means <- c(0, 0, 0)

    # sigma.xx is the cov matrix of the values for which the conditional
    # moments are desired
    sigma.xx <- cov[wants, wants]

    # sigma.yy is the cov matrix of the values to be conditioned on
    sigma.yy <- cov[has, has]

    sigma.xy <- matrix(cov[wants, has], ncol=1)
    sigma.yx <- t(sigma.xy)

    # y is the vector of known values to be conditioned on
    y <- as.numeric(na.omit(c(n.obs, t.obs, t.true)))

    # y is the expectation of the known values
    Ey <-  means[has]

    # conditional means of nom true, nom obs, and test obs
    conditional.mean <- means[wants] + sigma.xy %*% solve(sigma.yy) * (y - Ey)

    # names the rows
    row.names(conditional.mean) <- nms[wants]

    # conditional covariance matrix of nom true, nom obs, and test obs
    conditional.cov <- sigma.xx - sigma.xy %*% solve(sigma.yy) %*% sigma.yx

    # name the rows and columns
    row.names(conditional.cov) <- nms[wants]
    colnames(conditional.cov) <- nms[wants]
  }

  if (valid == 1e-7) {
    errortrapping(relyt=relyt)

    # index of row/column positions for the arguments supplied
    # this is the value to be conditioned on
    has <- seq(1:2)[!is.na(c(t.obs, t.true))]

    if (length(has) != 1) {stop("Only one of n.obs, t.obs, or t.true should be provided")}

    # index of row/column positions for the remaining arguments
    wants <- seq(1:2)[-has]
    # vector of names
    nms <- c("t.obs", "t.true")

    cov <- matrix(c(
      1,              sqrt(relyt),
      sqrt(relyt),    1),
      nrow=2, ncol=2, byrow=T)

    # marginal mean of the true [1] and observed [2] scores
    means <- c(0, 0)

    # sigma.xx is the cov matrix of the values for which the conditional
    # moments are desired
    sigma.xx <- cov[wants, wants]

    # sigma.yy is the cov matrix of the values to be conditioned on
    sigma.yy <- cov[has, has]

    sigma.xy <- matrix(cov[wants, has], ncol=1)
    sigma.yx <- t(sigma.xy)

    # y is the vector of known values to be conditioned on
    y <- as.numeric(na.omit(c(t.obs, t.true)))

    # y is the expectation of the known values
    Ey <-  means[has]

    # conditional means of nom true, nom obs, and test obs
    conditional.mean <- means[wants] + sigma.xy %*% solve(sigma.yy) * (y - Ey)

    # name the rows
    row.names(conditional.mean) <- nms[wants]

    # conditional covariance matrix of nom true, nom obs, and test obs
    conditional.cov <- sigma.xx - sigma.xy %*% solve(sigma.yy) %*% sigma.yx

    # name the rows
    row.names(conditional.cov) <- nms[wants]

    }

  return(list(
    conditional.mean=conditional.mean,
    conditional.cov=conditional.cov))
}
