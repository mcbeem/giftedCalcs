#' Bootstrapping with a progress bar
#'
#' @param reps Number of bootstrapped repetitions.
#' @param x Numeric vector of observed x.
#' @param id.rate Identification rate. Must be between 0 and 1.
#' @param nom.rate Nomination rate. Must be between 0 and 1.
#' @param pop.mean The known general population mean of x. Defaults to 0.
#' @param pop.sd The known general population standard deviation of x.
#'  Defaults to 1.
#' @param adjust Controls the bandwidth of the density estimator. Defaults to 1.0, which
#'  has been found to perform well in simulation.
#'
#' @examples
#' # generate some observed scores
#' # (note the lack of a relyt argument)
#' # true validity is .6
#' set.seed(1)
#' x <- r_identified(n=500, test.cutoff=.9, valid=.6,
#'   nom.cutoff=.85)
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(test.cutoff=.9, valid=.6,
#'   nom.cutoff=.85)$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(test.cutoff=.9, valid=.6,
#'   nom.cutoff=.85)$nom.rate
#'
#' # estimate the nomination validity with 10 bootstrapped samples
#' boot_estimate_valid(x, id.rate=id.rate, nom.rate=nom.rate, reps=10)
#'
#' @export

boot_estimate_valid <- function(reps, x, nom.rate, id.rate, pop.mean=0,
                                pop.sd=1, adjust=1) {

    samples <- pbapply::pbreplicate(n=reps,
                    expr=estimate_valid(x=sample(x, size=length(x),
                                        replace=T),
                             id.rate=id.rate, nom.rate=nom.rate, adjust=adjust,
                             pop.mean=pop.mean, pop.sd=pop.sd), simplify="matrix")
    return(t(samples))
}
