#' Conditional density of true scores for identified students
#'
#' \code{d_identified} is the conditional probability density function (pdf) for
#' identified students. It is a true probablity density with a total area of one.
#'
#' See also \code{d_identified_unnormed} for the unnormalized density, \code{p_identified}
#' for the cumulative density, \code{q_identified} for the quantile function, and
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
#' # test reliability is .9, the student's true score
#' # is 1 SD above the mean, the test cutoff is
#' # at the 90th percentile, the nomination
#' # cutoff is at the 90th percentile, the
#' # nomination validity is 0.5, and the student's
#' # population mean ability true score is 0
#' #
#' d_identified(relyt=.9, true.score=1, test.cutoff=.9,
#'   nom.cutoff=.9, valid=.5, mu=0)
#'
#' # make plot of the density
#' #
#' # create vector of true scores
#' Tscores <- seq(0,4, length.out=200)
#'
#' # plot the density
#' p.id <- sapply(Tscores, d_identified, relyt=.95,
#'   test.cutoff=.9, nom.cutoff=.9, valid=.5)
#'
#' plot(x=Tscores, y=p.id, type="l", xlab="true score", ylab="density")
#' @export

d_identified <- function(relyt, valid, test.cutoff, nom.cutoff, true.score, mu=0) {

  id_rate <- function(relyt, valid, true.score, test.cutoff, nom.cutoff, mu=0) {

    return(integrate(d_identified_unnormed, relyt=relyt, valid=valid,
                     test.cutoff=test.cutoff, nom.cutoff=nom.cutoff, mu=mu,
                     lower=-Inf, upper=Inf)[[1]])
  }

  return(d_identified_unnormed(relyt=relyt, valid=valid,
                                test.cutoff=test.cutoff, nom.cutoff=nom.cutoff,
                                true.score=true.score, mu=mu) /
           id_rate(relyt=relyt, valid=valid,
                   test.cutoff=test.cutoff, nom.cutoff=nom.cutoff, mu=mu))
}
