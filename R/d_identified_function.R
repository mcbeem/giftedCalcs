#' Conditional density of true scores for identified students
#'
#' \code{d_identified} is the conditional probability density function (pdf) for
#' identified students.
#'
#' See also \code{\link{p_identified}} for the cumulative density, \code{\link{q_identified}}
#' for the quantile function, and \code{\link{r_identified}} for random generation.
#'
#' @usage d_identified(true.score, relyt, test.cutoff, mu=0, valid=1e-7,
#' nom.cutoff=1e-7, normalize=TRUE)
#'
#' @param true.score The student's true score on a standardized (z-score) metric.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability. Defaults to 1e-7 for a single-
#'  stage identification system.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1. Defaults to 1e-7 for a single-
#'  stage identification system.
#' @param normalize Logical. Should the density be normalized to have a total area of one?
#'  Defaults to TRUE.
#'
#' @examples
#' # un-normalized density for t=1.0
#' d_identified(relyt=.9, true.score=1, test.cutoff=.9,
#'   nom.cutoff=.9, valid=.5, mu=0, normalize=FALSE)
#'
#' # normalized density for t=1.0
#' d_identified(relyt=.9, true.score=1, test.cutoff=.9,
#'   nom.cutoff=.9, valid=.5, mu=0, normalize=TRUE)
#'
#' # compare the density of identified students for universal
#' # screening vs. a poor-performing nomination stage
#' #
#' # area of each curve is proportion to the identification rate
#' # under each system
#'
#' # create vector of true scores
#' Tscores <- seq(0,4, length.out=200)
#'
# # plot the un-normed density for universal screening
#' p.universal <- sapply(Tscores, d_identified, relyt=.9,
#'   test.cutoff=.9, normalize=FALSE)
#'
#' plot(x=Tscores, y=p.universal, type="l", xlab="true score",
#'   col="blue")
#'
#' # add the un-normed density for the bad system
#' p.bad <- sapply(Tscores, d_identified, relyt=.9,
#'   test.cutoff=.9, nom.cutoff=.9, valid=.5, normalize=FALSE)
#'
#' points(x=Tscores, y=p.bad, type="l", col="red")
#' @export

d_identified <- function(true.score, relyt, test.cutoff,
                         mu=0, valid=1e-7, nom.cutoff=1e-7, normalize=TRUE) {

   errortrapping(mu=mu)

  # if (!is.logical(normalize)) {
  #   stop("\nargument normalize must be TRUE or FALSE")}

  d_identified_unnormed <- function(true.score=true.score, relyt=relyt,
                                    test.cutoff=test.cutoff, valid=valid,
                                    nom.cutoff=nom.cutoff, mu=mu) {

    p.id <- conditional_p_id(true.score=true.score, relyt=relyt,
                             test.cutoff=test.cutoff, valid=valid,
                             nom.cutoff=nom.cutoff)

    return(p.id*dnorm(true.score, mean=mu))
  }

  if (normalize==F) {
    return(d_identified_unnormed(true.score=true.score, relyt=relyt,
                                 test.cutoff=test.cutoff, valid=valid,
                                 nom.cutoff=nom.cutoff, mu=mu))
  } else {
    return(d_identified_unnormed(true.score=true.score, relyt=relyt,
                                 test.cutoff=test.cutoff, valid=valid,
                                 nom.cutoff=nom.cutoff, mu=mu) /
             integrate(d_identified_unnormed, relyt=relyt,
                       test.cutoff=test.cutoff, valid=valid,
                       nom.cutoff=nom.cutoff, mu=mu, lower=-Inf, upper=Inf)[[1]])
  }
}
