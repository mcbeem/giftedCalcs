#' Random generation from the distribution of true scores for identified students
#'
#' \code{r_identified} samples random variates from the distribution of true scores for
#'   identified students.
#' 
#' See also \code{d_identified_unnormed} for the unnormalized density, \code{d_identified} 
#' for the normalized density, \code{p_identified} for the cumulative density function, and 
#' \code{q_identified} for the quantile function.
#'
#' @param n The number of values to sample.
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
#' r_identified(n=10, relyt=.9, valid=.6, 
#'  test.cutoff=.9, nom.cutoff=.1, mu=0)
#'
#' # make a histogram of data from 100000 draws
#' dat <- r_identified(n=100000, relyt=.99, valid=.6, 
#'  test.cutoff=.95, nom.cutoff=.9, mu=0)
#' hist(dat, breaks=80, freq=F)
#' 
#' # superimpose the theoretical density
#' 
#' # create vector of true scores
#' T <- seq(0,4, length.out=200)
#' 
#' # add the density to the histogram
#' p.id <- sapply(T, d_identified, relyt=.99, 
#'   test.cutoff=.95, nom.cutoff=.9, valid=.6)
#'
#' points(x=T, y=p.id, type="l", col="red") 

r_identified <- function(n, relyt, valid, test.cutoff, nom.cutoff, mu=0) {
  
  if (n <= 0) {
    stop("\nThe value of n must be greater than zero.")
  }
  
  M <- d_identified(
    true.score=mean_identified(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                               nom.cutoff=nom.cutoff, mu=mu), relyt=relyt, valid=valid, test.cutoff=test.cutoff,
    nom.cutoff=nom.cutoff, mu=mu)*2
  
  
  df <- function(true.score) {d_identified(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                                           nom.cutoff=nom.cutoff, true.score=true.score, mu=mu)}
  
  dg <-function(x) {dnorm(x, mean=mean_identified(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                                                  nom.cutoff=nom.cutoff, mu=mu),
                          sd=sd_identified(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                                           nom.cutoff=nom.cutoff, mu=mu))}
  
  rg <- function(n) {
    m <- mean_identified(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                         nom.cutoff=nom.cutoff, mu=mu)
    s <- sd_identified(relyt=relyt, valid=valid, test.cutoff=test.cutoff,
                       nom.cutoff=nom.cutoff, mu=mu)
    return(rnorm(n, mean=m, sd=s))
  }
  
  return(SimDesign::rejectionSampling(n+1, df=df, dg=dg, rg=rg, M=M)[1:n])
}