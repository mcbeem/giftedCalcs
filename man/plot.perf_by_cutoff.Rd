% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_perf_by_cutoff_method.R
\name{plot.perf_by_cutoff}
\alias{plot.perf_by_cutoff}
\title{Plot method for class perf_by_cutoff}
\usage{
\method{plot}{perf_by_cutoff}(
  x,
  colors = c("#377eb8", "#e41a1c"),
  show.optimal = TRUE,
  show.cost = TRUE
)
}
\arguments{
\item{x}{an object of class perf_by_cutoff to be plotted}

\item{colors}{a vector of two colors to be used in the plot for the sensitivity curve and cost line, respectively.
Defaults to \code{c("#377eb8","#e41a1c")}}

\item{show.optimal}{logical; should the optimally cost-effective nomination cutoff be plotted? Defaults to TRUE}

\item{show.cost}{logical; should the proportional testing cost be plotted? defaults to TRUE}
}
\description{
Plot method for class perf_by_cutoff
}
