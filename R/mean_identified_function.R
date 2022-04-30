#' Mean of the conditional distribution of true or observed scores for identified students
#'
#' This function calculates the expected value (mean) of the ability true
#'   score distribution for identified students.
#'
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
#' # mean true score for identified students
#' mean_identified(
#'   relyt = .9, valid = .8,
#'   test.cutoff = .9, nom.cutoff = .5
#' )
#'
#' # mean observed score for identified students
#' mean_identified(
#'   valid = .8, test.cutoff = .9,
#'   nom.cutoff = .5
#' )
#' @export

mean_identified <- function(relyt = 1, test.cutoff, valid = 1e-7,
                            nom.cutoff = 1e-7, mu = 0) {

  # errortrapping(...)

  # expectation
  f1 <- function(x, relyt, test.cutoff, valid, nom.cutoff, mu) {
    return(x * d_identified(
      x = x, relyt = relyt,
      test.cutoff = test.cutoff, valid = valid,
      nom.cutoff = nom.cutoff, mu = mu, normalize = T
    ))
  }

  return(integrate(f1,
    relyt = relyt, test.cutoff = test.cutoff,
    valid = valid, nom.cutoff = nom.cutoff, mu = mu, lower = -Inf, upper = Inf
  )[[1]])
}
