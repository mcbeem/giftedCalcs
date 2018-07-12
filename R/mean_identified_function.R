#' Mean of the conditional distribution of ability for identified students
#'
#' \code{mean_identified} calculates the expected value (mean) of the ability true
#'   score distribution for identified students.
#'
#' @usage \code{mean_identified(relyt, test.cutoff, valid, nom.cutoff, mu=0)}
#'
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
#' # mean on the z-score metric
#' mean_identified(relyt=.9, valid=.8,
#'   test.cutoff=.9, nom.cutoff=.5, mu=0)
#'
#' # mean on the IQ-metric
#' 100+(mean_identified(relyt=.9, valid=.8,
#' test.cutoff=.9, nom.cutoff=.5, mu=0)*15)
#' @export

mean_identified <- function(...) {

  # this code checks the arguments supplied for errors
  #check for the correct number of arguments
  if (!nargs() %in% c(2, 3, 4, 5)) {stop("Incorrect arguments supplied; see ?mean_identified")}

  arguments <- as.list(match.call()[-1])

  #check if incorrect arguments are supplied
  if (!(("relyt") %in% names(arguments)) |
      !(("test.cutoff") %in% names(arguments))) {
    stop("Incorrect arguments supplied; see ?mean_identified")}

  argcheck <- arguments
  # how many arguments were supplied?
  start.length <- length(arguments)
  # remove valid and nom.cutoff from the set, if they are there
  argcheck$valid <- NULL
  argcheck$nom.cutoff <- NULL
  # if the list only got shorter by 1, then one of valid or nom.cutoff was not specified
  if (start.length-length(argcheck) == 1) {
      stop(" You must specify arguments nom.cutoff and valid for two-stage system; see ?mean_identified")}
  # remove mu if it was specified
  argcheck$mu <- NULL
  # there should be two arguments left
  if (length(argcheck) != 2) {stop("Incorrect arguments supplied; see ?mean_identified")}

  # expectation
  f1 <- function(true.score, ...) {
    return(true.score * d_identified(true.score=true.score, ...))
  }

  return(integrate(f1, ..., lower=-Inf, upper=Inf)[[1]])
}
