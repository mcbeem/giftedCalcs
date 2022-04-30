#' Estimate the nomination validity of an identification system given x
#'  of the identified students
#'
#' \code{estimate_valid} estimates the nomination validity of an identification system
#'  given the observed scores of the identified students, the confirmatory test
#'  cutoff, the nomination cutoff, and the proportion of students that are
#'  identified. The test cutoff, if not known, is inferred from the minimum score.
#'  The nomination cutoff is inferred from the proportion of students who are nominated
#'  under the assumption that the nomination x follow a standard normal distribution.
#'
#'  The function uses the Levenburg-Marquardt algorithm to minimize the discrepancy
#'   between the density of the x and the theoretical unnormalized density of the
#'   data. See \code{d_identified} for details.
#'
#' @param x Numeric vector of observed scores.
#' @param id.rate The proportion of students who have been identified. Range (0, 1). Must
#'  be less than or equal to \code{nom.rate}.
#' @param nom.rate The proportion of students who have been nominated. Range (0, 1). Used to
#'  calculate the nomination cutoff.
#' @param pop.mean The known general population mean of the x. Defaults to 0.
#' @param pop.sd The known general population standard deviation of the x.
#'  Defaults to 1.
#' @param adjust Controls the bandwidth of the density estimator. Defaults to 1.0, which
#'  has been found to perform well in simulation.
#'
#' @examples
#' # generate some observed scores
#' # (note the lack of a relyt argument)
#' # true validity is .6
#' set.seed(1)
#' x <- r_identified(
#'   n = 500, test.cutoff = .9, valid = .6,
#'   nom.cutoff = .85
#' )
#'
#' # calculate the identification rate implied by the system parameters
#' id.rate <- marginal_psychometrics(
#'   test.cutoff = .9, valid = .6,
#'   nom.cutoff = .85
#' )$identification.rate
#'
#' # calculate the nomination rate implied by the system parameters
#' nom.rate <- marginal_psychometrics(
#'   test.cutoff = .9, valid = .6,
#'   nom.cutoff = .85
#' )$nom.rate
#'
#' # estimate the system parameters from the data
#' estimate_valid(x = x, id.rate = id.rate, nom.rate = nom.rate)
#' @export


estimate_valid <- function(x, nom.rate, id.rate, pop.mean = 0,
                           pop.sd = 1, adjust = 1) {

  # remove any missing values in x and convert it to a vector
  x <- as.numeric(unlist(na.omit(x)))

  # standardize the scores wrt the population mean and sd
  x <- (x - pop.mean) / pop.sd

  # calculate nomination cutoff
  nom.cutoff <- 1 - nom.rate
  # nom.cutoff <- pnorm(qnorm(1-nom.rate-mu))

  # calculate test cutoff
  test.cutoff <- pnorm(min(x))

  # remove missing values
  x <- na.omit(x)

  # estimate the density
  dens <- density(x, n = 256, from = min(x), to = max(x), adjust = adjust, window = "g")
  # dens <- scdensity::scdensity(x, adjust=adjust, constraint="unimodal", n=256)

  # search for parameters of mixture distribution using
  #   the Levenburg-Marquardt algorithm

  data <- matrix(cbind(dens$x, dens$y * id.rate), ncol = 2)
  # drop data near cutoff
  data <- data[data[, 1] > qnorm(test.cutoff) + .1, ]

  parms <- NULL

  try(parms <- summary(
    minpack.lm::nlsLM(data[, 2] ~ d_identified_v(
      x = data[, 1],
      test.cutoff = test.cutoff,
      valid = valid,
      nom.cutoff = nom.cutoff,
      mu = 0,
      normalize = F
    ),
    lower = c(.1),
    upper = c(.999),
    start = list(valid = .6),
    control = minpack.lm::nls.lm.control(maxiter = 200)
    )
  )$coef,
  silent = TRUE
  )

  if (!is.null(parms)) {
    results <- parms[, 1]
  } else {
    results <- c(NA)
  }

  names(results) <- c("valid")
  return(results)
}
