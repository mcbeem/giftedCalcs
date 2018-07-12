#' Random generation from the distribution of true scores for identified students
#'
#' \code{r_identified} samples random variates from the distribution of true scores for
#'   identified students.
#'
#' See also \code{d_identified} for the normalized density, \code{p_identified}
#' for the cumulative density function, and \code{q_identified} for the quantile
#' function.
#'
#' @usage \code{r_identified(n, relyt, test.cutoff, valid, nom.cutoff, mu=0)}
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
#' draws <- r_identified(n=100000, relyt=.99, valid=.6,
#'  test.cutoff=.95, nom.cutoff=.9, mu=0)
#' hist(draws, breaks=80, freq=F, xlab="True score")
#'
#' # superimpose the theoretical density
#'
#' # create vector of true scores
#' Tscores <- seq(0,4, length.out=200)
#'
#' # add the density to the histogram
#' p.id <- sapply(Tscores, d_identified, relyt=.99,
#'   test.cutoff=.95, nom.cutoff=.9, valid=.6)
#'
#' points(x=Tscores, y=p.id, type="l", col="red")
#' @export

r_identified <- function(n, normalize=T, ...) {

  #check for the correct number of arguments
  if (!nargs() %in% c(3, 4, 5, 6)) {stop("aIncorrect arguments supplied; see ?r_identified")}

  arguments <- as.list(match.call()[-1])

  #check if incorrect arguments are supplied
  if (!(("n") %in% names(arguments)) |
      !(("relyt") %in% names(arguments)) |
      !(("test.cutoff") %in% names(arguments))) {
    stop("bIncorrect arguments supplied; see ?r_identified")}

  argcheck <- arguments
  # how many arguments were supplied?
  start.length <- length(arguments)
  # remove valid and nom.cutoff from the set, if they are there
  argcheck$valid <- NULL
  argcheck$nom.cutoff <- NULL
  # if the list only got shorter by 1, then one of valid or nom.cutoff was not specified
  if (start.length-length(argcheck) == 1) {
    stop(" You must specify arguments nom.cutoff and valid for two-stage system; see ?r_identified")}
  # remove mu if it was specified
  argcheck$mu <- NULL
  # there should be three arguments left
  if (length(argcheck) != 3) {stop("Incorrect arguments supplied; see ?r_identified")}

  if (n <= 0) {
    stop("\ncThe value of n must be greater than zero.")
  }

  errortrapping(...)

  M <- d_identified(
    true.score=mean_identified(...), normalize=T, ...)*3

  df <- function(true.score) {d_identified(true.score, normalize=T, ...)}

  dg <-function(x) {dnorm(x, mean=mean_identified(...),
                          sd=sd_identified(...))}

  rg <- function(n) {
    m <- mean_identified(...)
    s <- sd_identified(...)
    return(rnorm(n, mean=m, sd=s))
  }

  return(SimDesign::rejectionSampling(n+20, df=df, dg=dg, rg=rg, M=M)[1:n])
}
