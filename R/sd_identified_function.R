#' Standard deviation of the conditional distribution of ability for identified students
#'
#' \code{sd_identified} calculates the standard deviation of the ability true
#'   score distribution for identified students.
#'
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
#' # std dev on the z-score metric
#' sd_identified(relyt=.9, valid=.8,
#'   test.cutoff=.9, nom.cutoff=.5, mu=0)
#'
#' # sd on the IQ-metric
#' 15*(sd_identified(relyt=.9, valid=.8,
#' test.cutoff=.9, nom.cutoff=.5, mu=0))
#' @export

sd_identified <- function(...) {

  # this code checks the arguments supplied for errors
  #check for the correct number of arguments
  if (!nargs() %in% c(2, 3, 4, 5)) {stop("Incorrect arguments supplied; see ?sd_identified")}

  arguments <- as.list(match.call()[-1])

  #check if incorrect arguments are supplied
  if (!(("relyt") %in% names(arguments)) |
      !(("test.cutoff") %in% names(arguments))) {
    stop("Incorrect arguments supplied; see ?sd_identified")}

  argcheck <- arguments
  # how many arguments were supplied?
  start.length <- length(arguments)
  # remove valid and nom.cutoff from the set, if they are there
  argcheck$valid <- NULL
  argcheck$nom.cutoff <- NULL
  # if the list only got shorter by 1, then one of valid or nom.cutoff was not specified
  if (start.length-length(argcheck) == 1) {
    stop(" You must specify arguments nom.cutoff and valid for two-stage system; see ?sd_identified")}
  # remove mu if it was specified
  argcheck$mu <- NULL
  # there should be two arguments left
  if (length(argcheck) != 2) {stop("Incorrect arguments supplied; see ?sd_identified")}

  f2 <- function(true.score, ...) {
    return(true.score^2 * d_identified(true.score, normalize=T, ...))
  }

  v <- integrate(f2, ..., lower=-Inf, upper=Inf)[[1]]-
    mean_identified(...)^2
  return(sqrt(v))
}
