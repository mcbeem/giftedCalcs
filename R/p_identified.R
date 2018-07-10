#' Function for the conditional cumulative density of true scores for identified students
#'
#' \code{p_identified} is the conditional cumulative density function (cdf) for 
#' identified students. Given a value for the true score, it returns the proportion
#' of scores for identified students that will the lower than the target. In short, it
#' returns the percentile for a given true score.
#' 
#' See also \code{d_identified_unnormed} for the unnormalized density, \code{d_identified} 
#' for the normalized density, \code{q_identified} for the quantile function, and 
#' \code{r_identified} for random generation.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param true.score The student's true score on a standardized (z-score) metric.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1. 
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1). 
#'  Must not be exactly 0 or 1. 
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero. 
#' 
#' @examples
#' p_identified(relyt=.95, valid=.6, test.cutoff=.975, 
#'   nom.cutoff=.9, true.score=2.5, mu=0)

p_identified <- function(relyt, valid, test.cutoff, nom.cutoff, true.score, mu=0) {
  return(integrate(d_identified, relyt=relyt, valid=valid,
                   test.cutoff=test.cutoff, nom.cutoff=nom.cutoff,
                   mu=mu, lower=-Inf, upper=true.score)[[1]])
}
