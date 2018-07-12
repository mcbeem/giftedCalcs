#' Conditional density of true scores for identified students
#'
#' \code{d_identified} is the conditional probability density function (pdf) for
#' identified students.
#'
#' See also \code{p_identified} for the cumulative density, \code{q_identified}
#' for the quantile function, and \code{r_identified} for random generation.
#'
#' Warning: use explicitly named arguments only; do not rely on position.
#'  e.g., use \code{d_identified(true.score=1.5, relyt=.9, test.cutoff=.9)}
#'  rather than \code{d_identified(1.5, .9, .9)}
#'
#' @usage \code{d_identified(true.score, relyt, test.cutoff, valid,
#'  nom.cutoff, mu=0, normalize=T)}
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
#' @param normalize Logical. Should the density be normalized to have a total area of one?
#'  Defaults to TRUE.
#'
#' @examples
#' # un-normalized density for t=1.0
#' d_identified(relyt=.9, true.score=1, test.cutoff=.9,
#'   nom.cutoff=.9, valid=.5, mu=0, normalize=F)
#'
#' # normalized density for t=1.0
#' d_identified(relyt=.9, true.score=1, test.cutoff=.9,
#'   nom.cutoff=.9, valid=.5, mu=0, normalize=T)
#'
#' # compare the density of identified students for universal
#' # screening vs. a poor-performing nomination stage
#' #
#' # area of each curve is proportion to the identification rate
#' # under each system
#'
#' # create vector of true scores
#' Tscores <- seq(0,4, length.out=200)
#'
# # plot the un-normed density for universal screening
#' p.universal <- sapply(Tscores, d_identified, relyt=.9,
#'   test.cutoff=.9, normalize=F)
#'
#' plot(x=Tscores, y=p.universal, type="l", xlab="true score",
#'   col="blue")
#'
#' # add the un-normed density for the bad system
#' p.bad <- sapply(Tscores, d_identified, relyt=.9,
#'   test.cutoff=.9, nom.cutoff=.9, valid=.5, normalize=F)
#'
#' points(x=Tscores, y=p.bad, type="l", col="red")
#' @export

d_identified <- function(normalize=T, true.score, mu=0, ...) {

  # this code checks the arguments supplied to determine if the one-stage
  #   or two-stage version of the calculation should commence. If improper
  #   arguments are supplied, the function exits with an error.

  #check argument normalize
  if (is.logical(normalize) == FALSE) {stop("argument normalize must be set to TRUE or FALSE")}

  #check for the correct number of arguments
  if (!nargs() %in% c(2, 3, 4, 5, 6, 7)) {stop("Incorrect arguments supplied; see ?d_identified")}

  arguments <- as.list(match.call()[-1])

  #check if incorrect arguments are supplied
  if (!(("true.score") %in% names(arguments)) |
      !(("relyt") %in% names(arguments)) |
      !(("test.cutoff") %in% names(arguments))) {
    stop("Incorrect arguments supplied; see ?d_identified")}

  argcheck <- arguments
  # how many arguments were supplied?
  start.length <- length(arguments)
  # remove valid and nom.cutoff from the set, if they are there
  argcheck$valid <- NULL
  argcheck$nom.cutoff <- NULL
  # if the list only got shorter by 1, then one of valid or nom.cutoff was not specified
  if (start.length-length(argcheck) == 1) {
    stop(" You must specify arguments nom.cutoff and valid for two-stage system; see ?d_identified")}
  # remove mu if it was specified
  argcheck$mu <- NULL
  argcheck$normalize <- NULL
  # there should be three arguments left
  if (length(argcheck) != 3) {stop("Incorrect arguments supplied; see ?d_identified")}

  d_identified_unnormed <- function(true.score, mu, ...) {
    p.id <- conditional_p_id(true.score, ...)
    return(p.id*dnorm(true.score, mean=mu))
  }

  if (normalize==F) {
    return(d_identified_unnormed(true.score=true.score, mu, ...))
  }

  if (normalize==T) {
    return(d_identified_unnormed(true.score=true.score, mu, ...) /
             integrate(d_identified_unnormed, mu, ..., lower=-Inf, upper=Inf)[[1]])
  }
}
