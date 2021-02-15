#' Random generation from the distribution of true or observed scores for identified students
#'
#' This function samples random variates from the distribution of true scores for
#'   identified students using rejection sampling.
#'
#' The returned values are interpreted as true scores if a value is provided for
#' argument \code{relyt}; otherwise, they are observed scores.
#' See also \code{\link{d_identified}} for the normalized density, \code{\link{p_identified}}
#' for the cumulative density function, and \code{\link{q_identified}} for the quantile
#' function.
#'
#' @param n The number of values to sample.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1].
#'  Must not be exactly 0. Defaults to 1; in this case, the returned values are
#'  observed scores. If an alternative value is supplied for
#'  \code{relyt}, the returned values are true scores.
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
#' # generate true scores
#' r_identified(n=10, relyt=.9, valid=.6,
#'  test.cutoff=.9, nom.cutoff=.1, mu=0)
#'
#' # generate observed scores
#' r_identified(n=10, relyt=.9, valid=.6,
#'  test.cutoff=.9, nom.cutoff=.1, mu=0)
#'
#' # make a histogram of data from 100000 draws
#' draws <- r_identified(n=100000, relyt=.99, valid=.6,
#'  test.cutoff=.95, nom.cutoff=.9, mu=0)
#' hist(draws, breaks=80, freq=FALSE, xlab="True score")
#'
#' # superimpose the theoretical density
#'
#' # create vector of true scores
#' Tscores <- seq(0,4, length.out=200)
#'
#' # add the density to the histogram
#' p.id <- sapply(Tscores, d_identified, relyt=.99,
#'   test.cutoff=.95, nom.cutoff=.9, valid=.6)
#'
#' points(x=Tscores, y=p.id, type="l", col="red")
#' @export

r_identified <- function(n, relyt=1, test.cutoff, valid=1e-7,
                         nom.cutoff=1e-7, mu=0) {

  if (n <= 0) {
    stop("\ncThe value of n must be greater than zero.")
  }

  #errortrapping(...)

  M <- d_identified(
    x=mean_identified(relyt=relyt, test.cutoff=test.cutoff, valid=valid,
                               nom.cutoff=nom.cutoff, mu=mu),
    relyt=relyt, test.cutoff=test.cutoff, valid=valid,
    nom.cutoff=nom.cutoff, mu=mu, normalize=T)*3

  df <- function(x) {d_identified(x=x, relyt=relyt,
                                           test.cutoff=test.cutoff, valid=valid,
                                           nom.cutoff=nom.cutoff, mu=mu, normalize=T)}

  dg <-function(x) {dnorm(x, mean=mean_identified(relyt=relyt, test.cutoff=test.cutoff, valid=valid,
                                                  nom.cutoff=nom.cutoff, mu=mu),
                          sd=sd_identified(relyt=relyt, test.cutoff=test.cutoff, valid=valid,
                                           nom.cutoff=nom.cutoff, mu=mu))}

  rg <- function(n) {
    m <- mean_identified(relyt=relyt, test.cutoff=test.cutoff, valid=valid,
                         nom.cutoff=nom.cutoff, mu=mu)
    s <- sd_identified(relyt=relyt, test.cutoff=test.cutoff, valid=valid,
                       nom.cutoff=nom.cutoff, mu=mu)
    return(rnorm(n, mean=m, sd=s))
  }

  return(SimDesign::rejectionSampling(n+20, df=df, dg=dg, rg=rg, M=M)[1:n])
}
