% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norm_density_plot.R
\name{plot_norm_tails}
\alias{plot_norm_tails}
\title{Plot a normal distribution with one or both tails shaded}
\usage{
plot_norm_tails(
  lower_tail = 0.025,
  upper_tail = 0.975,
  pop_mean = 0,
  pop_sd = 1,
  xmin = NULL,
  xmax = NULL,
  len = 1000,
  fill_lower = rgb(0, 0.3, 0.8, 0.25),
  fill_middle = rgb(0, 0, 0, 0),
  fill_upper = rgb(0, 0.3, 0.8, 0.25),
  y_lab = "f(x)",
  x_lab = "x"
)
}
\arguments{
\item{lower_tail}{lower tail to plot}

\item{upper_tail}{upper tail to plot}

\item{pop_mean}{mean for the population}

\item{pop_sd}{standard deviation for the plot}

\item{xmin}{lower limit of x values to include}

\item{xmax}{upper limit of x values to include}

\item{len}{how many points to generate?}

\item{fill_lower}{fill color for the lower tail}

\item{fill_middle}{fill color for the non-tail part of the pdf}

\item{fill_upper}{fill color for the upper tail}

\item{y_lab}{label for x axis}

\item{x_lab}{label for y axis}
}
\description{
Plot a normal distribution with one or both tails shaded
}
