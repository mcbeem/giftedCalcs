#' Quantile function for true or observed scores for identified students
#'
#' This is the conditional quantile function for identified students.
#' Given a percentile, it returns the corresponding score (on a
#' z-score metric).
#'
#' See also \code{d_identified} for the normalized density, \code{p_identified} for
#' the cumulative density function, and \code{r_identified} for random generation.
#'
#' @usage q_identified(percentile, relyt, test.cutoff, valid, nom.cutoff, mu=0)
#'
#' @param percentile The percentile of the distribution of identified students. Range (0, 1).
#'   Must not be exactly 0 or 1.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1].
#'  Must not be exactly 0. Defaults to 1; in this case, the returned value is an
#'  observed score. If an alternative value is supplied for
#'  \code{relyt}, the returned value is a true score.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability. Defaults to 1e-7 for a single-
#'  stage identification system.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1. Defaults to 1e-7 for a single-
#'  stage identification system.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero.
#'
#' @examples
#' # one-stage identification program
#' # returns the true score
#' q_identified(percentile = .1, relyt = .9, test.cutoff = .9)
#'
#' # one-stage identification program
#' # returns the observed score
#' q_identified(percentile = .1, test.cutoff = .9)
#'
#' # two-stage identification program
#' #  returns the true score
#' q_identified(
#'   percentile = .9, relyt = .95, valid = .6,
#'   test.cutoff = .975, nom.cutoff = .9
#' )
#' @export

q_identified <- function(percentile, relyt = 1, test.cutoff, valid = 1e-7,
                         nom.cutoff = 1e-7, mu = 0) {
  if (percentile <= 0 | percentile >= 1) {
    stop("\nThe value of percentile must be between zero and one. It is the percentile of the score you wish to identify.")
  }

  zero.at.percentile <- function(x) {
    p_identified(
      x = x, relyt = relyt, test.cutoff = test.cutoff, valid = valid,
      nom.cutoff = nom.cutoff, mu = mu
    ) - percentile
  }
  return(uniroot(zero.at.percentile, interval = c(-10, 10))$root)
}

q_identified <- Vectorize(q_identified)
