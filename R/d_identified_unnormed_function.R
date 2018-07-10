#' Function for the unnormalized conditional density of true scores for identified students
#'
#' \code{d_identified_unnormed} is the conditional unnormalized probability density function
#' (pdf) for identified students. Because it is unnormalized, it is not a true probability
#' density and does not have a total area of one, so it should not be used for calculations.
#' However, its enclosed area is proportional to the identification rate, so this function can
#' be used to compare identification systems with varying identification rates in a meaningful
#' way.
#'
#' See also \code{d_identified} for the normalized density, \code{p_identified}
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
#' d_identified_unnormed(relyt=.9, true.score=1,
#'  test.cutoff=.9, nom.cutoff=.9, valid=.5, mu=0)
#'
#' # make plot of the unnormalized density
#' #
#' # create vector of true scores
#' T <- seq(0,4, length.out=200)
#'
#' # plot the un-normed density for universal screening
#' p.id <- sapply(T, d_identified_unnormed, relyt=.9,
#'   test.cutoff=.9, nom.cutoff=.0000001, valid=.5)
#'
#' plot(x=T, y=p.id, type="l", xlab="true score",
#'   col="blue")
#'
#' # plot the un-normed density for a bad system
#' p.id2 <- sapply(T, d_identified_unnormed, relyt=.9,
#'   test.cutoff=.9, nom.cutoff=.9, valid=.5)
#'
#' points(x=T, y=p.id2, type="l", col="red")
#' @export

d_identified_unnormed <- function(relyt, valid, true.score, test.cutoff,
                                     nom.cutoff, mu=0) {

  p.id <- conditional_p_id_2stage(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                          nom.cutoff=nom.cutoff, true.score=true.score)

  return(p.id*dnorm(true.score, mean=mu))
}
