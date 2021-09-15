#' Conditional cumulative density function of true or observed scores for identified students
#'
#' The conditional cumulative density function (cdf) for
#' identified students. Given a value for the true score, it returns the proportion
#' of scores for identified students that will the lower than the target. In short, it
#' returns the percentile for a given true score.
#'
#' See also \code{d_identified} for the density function, \code{q_identified} for
#' the quantile function, and \code{r_identified} for random generation.
#'
#' @param x The student's score on a standardized (z-score) metric. Interpreted
#'  as a true score if a value is specified for \code{relyt}, otherwise intepreted
#'  as an observed score.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1].
#'  Must not be exactly 0. Defaults to 1; in this case, x is assumed
#'  to be an observed score. If an alternative value is supplied for
#'  \code{relyt}, x is assumed to be a true score.
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
#' p_identified(
#'   relyt = .95, valid = .6, test.cutoff = .975,
#'   nom.cutoff = .9, x = 2.5
#' )
#' @export

# this code checks the arguments supplied to determine if the one-stage
#   or two-stage version of the calculation should commence. If improper
#   arguments are supplied, the function exits with an error.

p_identified <- function(x, relyt = 1, test.cutoff, valid = 1e-7,
                         nom.cutoff = 1e-7, mu = 0) {
  return(integrate(d_identified,
    relyt = relyt, test.cutoff = test.cutoff, valid = valid,
    nom.cutoff = nom.cutoff, mu = mu, normalize = T, lower = -Inf, upper = x
  )[[1]])
}

p_identified <- Vectorize(p_identified)
