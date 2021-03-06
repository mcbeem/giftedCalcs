% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marginal_psychometrics_MC_function.R
\name{rowwise_compare}
\alias{rowwise_compare}
\title{rowwise_compare: function for comparing two vectors}
\usage{
rowwise_compare(datarow, bounds)
}
\arguments{
\item{datarow}{numeric vector; the row to be compared}

\item{bounds}{numveric vector; the values to compare against. same length as datarow}
}
\value{
boolean value; TRUE if all elements of datarow are larger than the corresponding
  elements of bounds
}
\description{
rowwise_compare: function for comparing two vectors
}
