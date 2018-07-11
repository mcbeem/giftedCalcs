#' Conditional probability of identification given
#'  the true score for a single-stage identification system
#'
#' \code{conditional_p_id_1stage} calculates the conditional probabilty of identification
#'  for a single-stage system given a student's true score, the test reliability,
#'  and the population mean true score. The probabilities returned by this function can
#'  be plotted against a range of true scores to create an identification curve.
#'
#' This function returns the probability of identification.
#'
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param true.score The student's true score on a standardized (z-score) metric.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#'
#' @examples
#' # test reliability is .9, the student's true score
#' # is 1 SDs above the mean, and the test cutoff is
#' # at the 90th percentile
#' #
#' conditional_p_id_1stage(relyt=.9, true.score=1,
#'    test.cutoff=.9)
#'
#' # make an identification curve
#' #
#' # create vector of true scores
#' Tscores <- seq(0,3, length.out=100)
#'
#' # calculate the identification probability for each
#' p.id <- sapply(Tscores, conditional_p_id_1stage, relyt=.9,
#'   test.cutoff=.9)
#'
#' # make a plot
#' plot(x=Tscores, y=p.id, type="l", xlab="true score",
#'   ylab="p identified")
#'
#' # add a reference line for the test cutoff
#' abline(v=qnorm(.9), col="red")
#' @export

conditional_p_id_1stage <- function(relyt, true.score, test.cutoff) {

  errortrapping(relyt=relyt, test.cutoff=test.cutoff)

  b <- qnorm(test.cutoff) / sqrt(relyt)
  a <- sqrt(relyt / (1-relyt))

  p.identification <- pnorm(a*(true.score-b))

  return(p.identification)
}
