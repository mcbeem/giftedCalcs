#' Conditional probability of identification
#'
#'  This function calculates the conditional probability of identification
#'  for a one- or two-stage system given a student's true or observed score, the test reliability,
#'  and, for two-stage systems, the nomination validity and the nomination cutoff. A
#'  two-stage system is one in which an initial nomination process is used to select
#'  students who are tested on the confirmatory assessment.
#'
#'  The probabilities returned by this function can be plotted against a range of true
#'  scores to create an identification curve (see examples).
#'
#' The two-stage identification probability is returned if arguments valid
#'  and nom.cutoff are provided. Otherwise, the one-stage probability is
#'  returned.
#'
#' @usage conditional_p_id(x, relyt=1, test.cutoff, valid=1e-7, nom.cutoff=1e-7)
#'
#' @param x The student's score on a standardized (z-score) metric. Interpreted
#'  as a true score if a value is specified for \code{relyt}, otherwise intepreted
#'  as an observed score.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1].
#'  Must not be exactly 0. Defaults to 1; in this case, x is assumed
#'  to be an observed score. If an alternative value is supplied for
#'  \code{relyt}, x is assumed to be a true score.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1. Can be a vector or scalar.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability. Defaults to 1e-7 for a single-
#'  stage system.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1). Defaults to 1e-7 for a single-
#'  stage system. Must not be exactly 0 or 1.
#'
#' @examples
#' # one-stage system, true score=1
#' conditional_p_id(x=1, relyt=.9, test.cutoff=.9)
#'
#' # one-stage system, observed score=1
#' # (note that the relyt argument is not specified)
#' conditional_p_id(x=1, test.cutoff=.9)
#'
#' # two-stage system, true score=2
#' conditional_p_id(x=2, relyt=.9, test.cutoff=.9,
#'    nom.cutoff=.9, valid=.5)
#'
#' # two-stage system, observed score=2
#' conditional_p_id(x=2, test.cutoff=.9,
#'    nom.cutoff=.9, valid=.5)
#'
#' # make an identification curve:
#' #  true score vs p identified
#' #
#' # create vector of true scores
#' Tscores <- seq(0,3, length.out=100)
#'
#' # calculate the identification probability for each
#' p.id <- conditional_p_id(x=Tscores, relyt=.9,
#'   test.cutoff=.9, nom.cutoff=.9, valid=.5)
#'
#' # make a plot
#' plot(x=Tscores, y=p.id, type="l", xlab="true score",
#'   ylab="p identified")
#'
#' # add a reference line for the test cutoff
#' abline(v=qnorm(.9), col="red")
#' @export
#'

conditional_p_id <- function(x, relyt=1, test.cutoff, valid=1e-7, nom.cutoff=1e-7) {

  # select true or observed-score version based on supplied arguments
  if (relyt==1) {version="O"} else {version="T"}
  # select 1- or 2-stage version based on the supplied arguments
  if (valid==1e-7 & nom.cutoff==1e-7) {stages=1} else {stages=2}

  if (version=="T") {

    if(stages==2) {
      errortrapping(relyt=relyt, test.cutoff=test.cutoff,
                    nom.cutoff=nom.cutoff, valid=valid)

      b_C <- qnorm(test.cutoff) / sqrt(relyt)
      a_C <- sqrt(relyt / (1-relyt))

      b_N <- (qnorm(nom.cutoff)*sqrt(relyt)) / valid
      a_N <- sqrt(((valid^2) / relyt) /
                    (1-((valid^2) / relyt)))

      p.identification <- pnorm(a_C*(x-b_C))*
        pnorm(a_N*(x-b_N))
    }

    if(stages==1) {
      errortrapping(relyt=relyt, test.cutoff=test.cutoff)

      b <- qnorm(test.cutoff) / sqrt(relyt)
      a <- sqrt(relyt / (1-relyt))

      p.identification <- pnorm(a*(x-b))
    }
  }

  if (version=="O") {
    if (stages == 2) {
      errortrapping(test.cutoff = test.cutoff,
                    nom.cutoff = nom.cutoff, valid = valid)

      b_N <- qnorm(nom.cutoff) / valid
      a_N <- sqrt((valid^2) / (1 - valid^2))
      #trunc <- ifelse(x <= qnorm(test.cutoff), 0, 1)
      #stepfun(x=qnorm(test.cutoff), y=c(0,1))(x)

      p.identification <- pnorm(9e5*(x-qnorm(test.cutoff)))*pnorm(a_N * (x - b_N))
    }

    if (stages == 1) {
      errortrapping(test.cutoff = test.cutoff)
      trunc <- ifelse(x <= qnorm(test.cutoff), 0, 1)
      p.identification <- trunc
    }
  }

  return(p.identification)
}
