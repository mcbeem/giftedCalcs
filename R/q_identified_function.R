#' Quantile function for true scores for identified students
#'
#' \code{q_identified} is the conditional quantile function for identified students.
#' Given a percentile for the true score, it returns the corresponding true score (on a
#' z-score metric).
#'
#' See also \code{d_identified} for the normalized density, \code{p_identified} for
#' the cumulative density function, and \code{r_identified} for random generation.
#'
#' @usage \code{q_identified(percentile, relyt, test.cutoff, valid, nom.cutoff, mu=0)}
#'
#' @param percentile The percentile of the distribution of identified students. Range (0, 1).
#'   Must not be exactly 0 or 1.
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
#' # one-stage identification program
#' q_identified(percentile=.9, relyt=.95, test.cutoff=.975)
#'
#' # two-stage identification program
#' q_identified(percentile=.9, relyt=.95, valid=.6,
#'   test.cutoff=.975, nom.cutoff=.9, mu=0)
#' @export

q_identified <- function(percentile, mu=0, ...) {

  #check for the correct number of arguments
  if (!nargs() %in% c(3, 4, 5, 6)) {stop("Incorrect arguments supplied; see ?q_identified")}

  arguments <- as.list(match.call()[-1])

  #check if incorrect arguments are supplied
  if (!(("percentile") %in% names(arguments)) |
      !(("relyt") %in% names(arguments)) |
      !(("test.cutoff") %in% names(arguments))) {
    stop("Incorrect arguments supplied; see ?q_identified")}

  argcheck <- arguments
  # how many arguments were supplied?
  start.length <- length(arguments)
  # remove valid and nom.cutoff from the set, if they are there
  argcheck$valid <- NULL
  argcheck$nom.cutoff <- NULL
  # if the list only got shorter by 1, then one of valid or nom.cutoff was not specified
  if (start.length-length(argcheck) == 1) {
    stop(" You must specify arguments nom.cutoff and valid for two-stage system; see ?q_identified")}
  # remove mu if it was specified
  argcheck$mu <- NULL
  # there should be three arguments left
  if (length(argcheck) != 3) {stop("Incorrect arguments supplied; see ?q_identified")}

  if (percentile <= 0 | percentile >= 1) {
    stop("\nThe value of percentile must be between zero and one. It is the percentile of the score you wish to identify.")
  }

  zero.at.percentile <- function(true.score)
    p_identified(true.score, mu=mu, ...)-percentile
  return(uniroot(zero.at.percentile, interval=c(-10, 10))$root)
}
