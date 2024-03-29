% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/var_mean.R
\name{var_mean}
\alias{var_mean}
\title{The variance of the weighted mean of assessments}
\usage{
var_mean(r, w = NA)
}
\arguments{
\item{r}{Either a correlation matrix or a vector of unique correlations. If the weights
are not all equal, it is recommended to specify the correlations as a matrix to avoid
erronous pairings of assessment correlations and weights, since this can be confusing
if the correlations are supplied as a vector.}

\item{w}{A vector of weights. Will be internally normalized to sum to 1 and presumes the
same order of assessments as the correlation matrix. If omitted, it is assumed that all
assessments have the same weight.}
}
\description{
This function calculates the variance of the weighted mean of a set of unit-standardized
assessments. When the assessments are imperfectly correlated, this variance will
be less than one. The identification cut score must be adjusted accordingly to
maintain the desired percentile cutoff.
}
\details{
The value returned by this function can be interpreted as the shrinkage factor
of the variance of the weighted mean. The square root of this value is the shrinkage
factor of the standard deviation of the weighted mean.
}
\examples{
var_mean(r = c(.4, .7, .9), w = c(1, 2, 3))

var_mean(r = matrix(c(
  1, .4, .7,
  .4, 1, .9,
  .7, .9, 1
), 3, 3, byrow = TRUE))
}
