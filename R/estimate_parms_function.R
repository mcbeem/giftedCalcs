#' Estimate psychometric parameters of an identification system given scores
#'  of the identified students
#'
#' \code{estimate_params} estimates the psychometric parameters of an identification system
#'  given the scores of the identified students. The four parameters are the nomination cutoff
#'  (\code{nom.cutoff}), the confirmatory test cutoff (\code{test.cutoff}), the test reliability
#'  (\code{relyt}), and the nomination validity (\code{valid}).
#'
#'  The function uses the Levenburg-Marquardt algorithm to minimize the discrepancy
#'   between the density of the scores and the theoretical unnormalized density of the
#'   data. See \code{d_identified} for details. The density is estimated via
#'   shape-constrained kernel density estimation.
#'
#'  Simulation results suggest a mimimum sample size of n=150 to n=200 for reasonable
#'   performance.
#'
#' @param scores Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param w Optional argument specifying a vector of row numbers that should be included
#'  in the analysis. Defaults to including all cases.
#' @param id.rate The proportion of students who have been identified. Range (0, 1). Must
#'  be less than or equal to \code{nom.rate}.
#' @param nom.rate The proportion of students who have been nominated. Range (0, 1). Used to
#'  calculate the nomination cutoff.
#' @param pop.mean The known general population mean of the scores. Defaults to 0.
#' @param pop.sd The known general population standard deviation of the scores.
#'  Defaults to 1.
#' @param adjust Controls the bandwidth of the density estimator. Defaults to 0.5, which
#'  has been found to perform well in simulation.
#'
#' @examples
#' # generate some data
#' set.seed(1)
#' scores <- r_identified(n=500, test.cutoff=.9, relyt=.93, valid=.6,
#'   nom.cutoff=.85)
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.6,
#'   nom.cutoff=.85)$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.6,
#'   nom.cutoff=.85)$nom.rate
#'
#' # estimate the system parameters
#' estimate_parms(scores=scores, id.rate=id.rate, nom.rate=nom.rate)
#'
#' # estimate the parameters using only the first hundred cases
#' estimate_parms(scores=scores, id.rate=id.rate, nom.rate=nom.rate, w=1:100)
#'
#'
#' @export

estimate_parms <- function(scores, w, id.rate, nom.rate, pop.mean=0,
                           pop.sd=1, adjust=.5) {

  # standardize the scores
  scores <- (scores - pop.mean) / pop.sd

  # calculate nomination cutoff
  nom.cutoff <- 1-nom.rate
  #nom.cutoff <- pnorm(qnorm(1-nom.rate-mu))

  # remove missing values
  scores <- na.omit(scores)

  if (missing(w)) {w <- 1:length(scores)}
  if (!missing(w) & is.numeric(w) & length(w)==1) {stop("Argument w provided but is invalid. w must be a vector, not a scalar.")}
  if (!missing(w) & !is.numeric(w)) {stop("Argument w provided but is invalid. w not a numeric vector.")}
  if ((!missing(w) & length(w)>1  & is.numeric(w) & (max(w) > length(scores) || (min(w) < 1)))) {
    stop("Argument w provided but is invalid. The min value of w cannot be smaller than 1. The max value of w cannot exceed the number of non-missing scores.")}

    # make the vector of row numbers

  # estimate the density
  dens <- density(scores[w], n=256, from=.5, to=3.5, adjust=adjust, window="t")
  #dens <- scdensity::scdensity(scores[w], adjust=adjust, constraint="unimodal", n=256)

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y*id.rate), ncol=2)
  parms <- NULL

 try(parms <- summary(
    minpack.lm::nlsLM(data[,2] ~ d_identified_v(true.score=data[,1],
                                                test.cutoff=test.cutoff,
                                                relyt=relyt,
                                                valid=sqrt(relyt)+valid,
                                                nom.cutoff=1-nom.rate,
                                                mu=0,
                                                normalize=F),
                      lower=c(.5, .5, -.7),
                      upper=c(.999, .999, -.001),
                      start=list(test.cutoff=.9, relyt=.9, valid=-.2),
                      control= minpack.lm::nls.lm.control(maxiter=200)))$coef,
          silent=TRUE)

    if (!is.null(parms)) {
      results <- parms[,1]
      results[3] <- sqrt(parms[2,1])+parms[3,1]
      results[4] <- nom.cutoff

    } else {
      results <- c(NA, NA, NA, nom.cutoff) }

  names(results) <- c("test.cutoff", "relyt", "valid", "nom.cutoff")
  return(results)
}
