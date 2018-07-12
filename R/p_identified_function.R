#' Conditional cumulative density function of true scores for identified students
#'
#' \code{p_identified} is the conditional cumulative density function (cdf) for
#' identified students. Given a value for the true score, it returns the proportion
#' of scores for identified students that will the lower than the target. In short, it
#' returns the percentile for a given true score.
#'
#' See also \code{d_identified} for the density function, \code{q_identified} for
#' the quantile function, and \code{r_identified} for random generation.
#'
#' Warning: use named arguments only; do not rely on position.
#'  e.g., use \code{p_identified(true.score=1.5, relyt=.9, test.cutoff=.9)}
#'  rather than \code{p_identified(1.5, .9, .9)}
#'
#' @usage \code{p_identified(true.score, relyt, test.cutoff, valid, nom.cutoff, mu=0)}
#'
#' @param true.score The student's true score on a standardized (z-score) metric.
#' @param relyt Confirmatory test reliability coefficient. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param test.cutoff Confirmatory test cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param valid Nomination validity coefficient. Controls the relatedness of the nomination
#'  scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
#'  must be less than the square root of the test reliability.
#' @param nom.cutoff Nomination cutoff percentile. Range (0, 1).
#'  Must not be exactly 0 or 1.
#' @param mu Population mean true score on a standardized (z-score) metric.
#'  Defaults to zero.
#'
#' @examples
#' p_identified(relyt=.95, valid=.6, test.cutoff=.975,
#'   nom.cutoff=.9, true.score=2.5, mu=0)
#' @export

# this code checks the arguments supplied to determine if the one-stage
#   or two-stage version of the calculation should commence. If improper
#   arguments are supplied, the function exits with an error.

p_identified <- function(true.score, ...) {

  #check for the correct number of arguments
  if (!nargs() %in% c(3, 4, 5, 6)) {stop("Incorrect arguments supplied; see ?p_identified")}

  arguments <- as.list(match.call()[-1])

  #check if incorrect arguments are supplied
  if (!(("true.score") %in% names(arguments)) |
      !(("relyt") %in% names(arguments)) |
      !(("test.cutoff") %in% names(arguments))) {
    stop("Incorrect arguments supplied; see ?p_identified")}

  argcheck <- arguments
  argcheck$valid <- NULL
  argcheck$nom.cutoff <- NULL
  argcheck$mu <- NULL
  if (length(argcheck) != 3) {stop("Incorrect arguments supplied; see ?p_identified")}


  return(integrate(d_identified, ..., lower=-Inf, upper=true.score)[[1]])
}
