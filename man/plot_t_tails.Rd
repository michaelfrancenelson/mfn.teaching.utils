% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norm_density_plot.R
\name{plot_t_tails}
\alias{plot_t_tails}
\title{Plot a t distribution with one or both tails shaded}
\usage{
plot_t_tails(
  lower_tail = 0.025,
  upper_tail = 0.925,
  df_sample = 30,
  ncp_sample = 0,
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

\item{df_sample}{sample degrees of freedom}

\item{ncp_sample}{non-centrality parameter}

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
Plot a t distribution with one or both tails shaded
}
