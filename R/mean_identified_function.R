#' Mean of the conditional distribution of ability for identified students
#'
#' \code{mean_identified} calculates the expected value (mean) of the ability true
#'   score distribution for identified students.
#'
#' @usage \code{mean_identified(relyt, test.cutoff, valid, nom.cutoff, mu=0)}
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero.
#'
#' @examples
#' # mean on the z-score metric
#' mean_identified(relyt=.9, valid=.8,
#'   test.cutoff=.9, nom.cutoff=.5, mu=0)
#'
#' # mean on the IQ-metric
#' 100+(mean_identified(relyt=.9, valid=.8,
#' test.cutoff=.9, nom.cutoff=.5, mu=0)*15)
#' @export

mean_identified <- function(relyt, test.cutoff, valid=1e-7,
                            nom.cutoff=1e-7, mu=0) {

# errortrapping(...)

  # expectation
  f1 <- function(true.score=true.score, relyt=relyt, test.cutoff=test.cutoff,
                 valid=valid, nom.cutoff=nom.cutoff, mu=mu) {
    return(true.score * d_identified(true.score=true.score, relyt=relyt,
                                     test.cutoff=test.cutoff, valid=valid,
                                    nom.cutoff=nom.cutoff, mu=mu, normalize=T))
  }

  return(integrate(f1, relyt=relyt, test.cutoff=test.cutoff,
                   valid=valid, nom.cutoff=nom.cutoff, mu=mu, lower=-Inf, upper=Inf)[[1]])
}
