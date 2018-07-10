#' Function for calculing the standard deviation of the conditional distribution of ability for identified students
#'
#' \code{sd_identified} calculates the expected value (mean) of the ability true
#'   score distribution for identified students. 
#'   
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1). 
#'  Must not be exactly 0 or 1.
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
#' # std dev on the z-score metric
#' sd_identified(relyt=.9, valid=.8, 
#'   test.cutoff=.9, nom.cutoff=.5, mu=0)
#' 
#' # sd on the IQ-metric
#' 15*(sd_identified(relyt=.9, valid=.8, 
#' test.cutoff=.9, nom.cutoff=.5, mu=0))


sd_identified <- function(relyt, valid, test.cutoff, nom.cutoff, mu=0) {
  
  f2 <- function(relyt, valid, test.cutoff, nom.cutoff, true.score, mu=0) {
    return(true.score^2 * d_identified(relyt=relyt, valid=valid,
                                       test.cutoff=test.cutoff, nom.cutoff=nom.cutoff, 
                                       true.score=true.score, mu=mu))
  }
  
  v <- integrate(f2, relyt=relyt, valid=valid,
                 test.cutoff=test.cutoff, nom.cutoff=nom.cutoff,
                 mu=mu,
                 lower=-Inf, upper=Inf)[[1]]-
    mean_identified(relyt=relyt, valid=valid,
                    test.cutoff=test.cutoff, nom.cutoff=nom.cutoff, mu=mu)^2
  return(sqrt(v))
}
