% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d_identified_v_function.R
\name{d_identified_v}
\alias{d_identified_v}
\title{Conditional density of true or observed scores for identified students (vectorized version)}
\usage{
d_identified_v(
  x,
  relyt = 1,
  test.cutoff,
  mu = 0,
  valid = 1e-07,
  nom.cutoff = 1e-07,
  normalize = TRUE
)
}
\arguments{
\item{x}{The student's score on a standardized (z-score) metric. Interpreted
as a true score if a value is specified for \code{relyt}, otherwise intepreted
as an observed score.}

\item{relyt}{Confirmatory test reliability coefficient. Range (0, 1].
Must not be exactly 0. Defaults to 1; in this case, x is assumed
to be an observed score. If an alternative value is supplied for
\code{relyt}, x is assumed to be a true score.}

\item{test.cutoff}{Confirmatory test cutoff percentile. Range (0, 1).
Must not be exactly 0 or 1.}

\item{mu}{Population mean true score on a standardized (z-score) metric.
Defaults to zero.}

\item{valid}{Nomination validity coefficient. Controls the relatedness of the nomination
scores and the confirmatory test scores. Range (0, 1). Must not be exactly 0 or 1, and
must be less than the square root of the test reliability.}

\item{nom.cutoff}{Nomination cutoff percentile. Range (0, 1).
Must not be exactly 0 or 1.}

\item{normalize}{Logical. Should the density be normalized to have a total area of one?
Defaults to TRUE.}
}
\description{
\code{\link{d_identified_v}} is the conditional probability density function (pdf) for
identified students. Unlike \code{\link{d_identified}}, it is vectorized.
}
\details{
See also \code{\link{p_identified}} for the cumulative density, \code{\link{q_identified}}
for the quantile function, and \code{\link{r_identified}} for random generation.
}
\examples{
# un-normalized density for t=1.0
d_identified(
  relyt = .9, x = 1, test.cutoff = .9,
  nom.cutoff = .9, valid = .5, mu = 0, normalize = FALSE
)

# normalized density for t=1.0
d_identified(
  relyt = .9, x = 1, test.cutoff = .9,
  nom.cutoff = .9, valid = .5, mu = 0, normalize = TRUE
)

# compare the density of identified students for universal
# screening vs. a poor-performing nomination stage
#
# area of each curve is proportion to the identification rate
# under each system

# create vector of true scores
Tscores <- seq(0, 4, length.out = 200)

# # plot the un-normed density for universal screening
p.universal <- sapply(Tscores, d_identified,
  relyt = .9,
  test.cutoff = .9, normalize = FALSE
)

plot(
  x = Tscores, y = p.universal, type = "l", xlab = "true score",
  col = "blue"
)

# add the un-normed density for the bad system
p.bad <- sapply(Tscores, d_identified,
  relyt = .9,
  test.cutoff = .9, nom.cutoff = .9, valid = .5, normalize = FALSE
)

points(x = Tscores, y = p.bad, type = "l", col = "red")
}
