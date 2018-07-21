#' Standard deviation of the conditional distribution of true or observed scores for
#'  identified students
#'
#' \code{sd_identified} calculates the standard deviation of the ability true
#'   score distribution for identified students.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1].
#'  Must not be exactly 0. Defaults to 1; in this case, x is assumed
#'  to be an observed score. If an alternative value is supplied for
#'  \code{relyt}, x is assumed to be a true score.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero.
#'
#' @examples
#' # std dev for true scores
#' sd_identified(relyt=.9, valid=.8,
#'   test.cutoff=.9, nom.cutoff=.5, mu=0)
#'
#' # std dev for observed scores
#' 15*(sd_identified(valid=.8,
#' test.cutoff=.9, nom.cutoff=.5, mu=0))
#' @export

sd_identified <- function(relyt=1, test.cutoff, valid=1e-7,
                          nom.cutoff=1e-7, mu=0) {

  f2 <- function(x, relyt, test.cutoff, valid, nom.cutoff, mu, normalize) {
    return(x^2 * d_identified(x=x, relyt=relyt, test.cutoff=test.cutoff,
                                       valid=valid, nom.cutoff=nom.cutoff, mu=mu, normalize=T))
  }

  v <- integrate(f2, relyt=relyt, test.cutoff=test.cutoff,
                 valid=valid, nom.cutoff=nom.cutoff, mu=mu, normalize=T, lower=-Inf, upper=Inf)[[1]]-
    mean_identified(relyt=relyt, test.cutoff=test.cutoff,
                    valid=valid, nom.cutoff=nom.cutoff, mu=mu)^2
  return(sqrt(v))
}
