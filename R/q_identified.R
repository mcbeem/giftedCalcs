#' Conditional quantile function for the distribution of true scores for identified students
#'
#' \code{q_identified} is the conditional quantile function for identified students. 
#' Given a percentile for the true score, it returns the corresponding true score (on a
#' z-score metric).
#' 
#' See also \code{d_identified_unnormed} for the unnormalized density, \code{d_identified} 
#' for the normalized density, \code{p_identified} for the cumulative density function, and 
#' \code{r_identified} for random generation.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param percentile The percentile of the distribution of identified students. Range (0, 1).
#'   Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1. 
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1. 
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero. 
#'
#' @examples
#' q_identified(percentile=.9, relyt=.95, valid=.6, 
#'   test.cutoff=.975, nom.cutoff=.9, mu=0)

q_identified <- function(relyt, valid, test.cutoff, nom.cutoff, percentile, mu=0) {
  
  if (percentile <= 0 | percentile >= 1) {
    stop("\nThe value of percentile must be between zero and one. It is the percentile of the score you wish to identify.")
  }
  
  zero.at.percentile <- function(true.score)
    p_identified(relyt=relyt, valid=valid,
                 test.cutoff=test.cutoff, nom.cutoff=nom.cutoff,
                 mu=mu, true.score=true.score)-percentile
  return(uniroot(zero.at.percentile,interval=c(-10, 10))$root)
}