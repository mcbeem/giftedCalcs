#' Bootstrapping with a progress bar
#'
#' @param reps Number of bootstrapped repetitions.
#' @param scores Numeric vector of observed scores.
#' @param id.rate Identification rate. Must be between 0 and 1.
#' @param nom.rate Nomination rate. Must be between 0 and 1.
#' @param multicore Logical value indicating whether parallel processing should be used.
#'  Must be \code{TRUE} or \code{FALSE}. Defaults to \code{FALSE}.
#'
#' @examples
#' # generate some data
#' set.seed(123)
#' scores <- r_identified(n=500, test.cutoff=.9, relyt=.93, valid=.5,
#'   nom.cutoff=.9)
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.5,
#'   nom.cutoff=.9)$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(test.cutoff=.9, relyt=.93, valid=.5,
#'   nom.cutoff=.9)$nom.rate
#'
#' # estimate the system parameters with 10 bootstrapped samples
#' boot_estimate_parms(scores, id.rate=id.rate, nom.rate=nom.rate, reps=10)
#'
#' @export

boot_estimate_parms <- function(reps, scores, id.rate, nom.rate, multicore=FALSE) {

  if (multicore==T) {
     cpus <- max(1, parallel::detectCores())
     clus <- parallel::makeCluster(mc <- cpus)
      parallel::clusterExport(clus,
                              varlist=c("reps", "scores", "id.rate", "nom.rate",
                                        "estimate_parms"),
                              envir=environment())

     samples <- pbapply::pbreplicate(n=reps,
                expr=estimate_parms(scores=sample(scores, size=length(scores),
                                      replace=T),
                                      id.rate=id.rate, nom.rate=nom.rate), simplify="matrix", cl=cpus)
     parallel::stopCluster(clus)

  } else if (multicore==F) {
    samples <- pbapply::pbreplicate(n=reps,
                              expr=estimate_parms(scores=sample(scores, size=length(scores),
                                                                replace=T),
                             id.rate=id.rate, nom.rate=nom.rate), simplify="matrix")
  } else {stop("multicore must be TRUE or FALSE")}

    return(t(samples))
}
