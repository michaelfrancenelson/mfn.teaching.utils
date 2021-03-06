% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_confidence_interval.R
\name{sample_confidence_interval}
\alias{sample_confidence_interval}
\title{Create simulated confidence intervals}
\usage{
sample_confidence_interval(x, alpha = 0.05, pop_sd = NA)
}
\arguments{
\item{x}{The sample data}

\item{alpha}{Confidence level}

\item{pop_sd}{Population standard deviation (if known).
If this is provided, the interval is based on a z-score rather
than a t-value}
}
\description{
Simulation of Frequentist confidence intervals of the mean of
normally-distributed data.
}
\examples{

sample_confidence_interval(rnorm(100), pop_sd = 1)

}
