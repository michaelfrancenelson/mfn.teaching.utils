#' Create simulated confidence intervals
#'
#' Simulation of Frequentist confidence intervals of the mean of
#' normally-distributed data.
#'
#' @param n_samples The sample size for the confidence intervals.
#' @param n_intervals The number of intervals to generate.
#' @param alpha Confidence level
#' @param pop_mean Population
#' @param pop_sd population standard deviation
#'
#' @import ggplot2
#'
#' @export

ggplot_confints = function(
  n_intervals,
  n_samples,
  alpha = 0.05,
  pop_mean = 0,
  pop_sd = 1)
{
  dat_out =
    simulated_confidence_intervals_of_mean(
      n_samples = n_samples,
      n_intervals,
      alpha = alpha,
      pop_sd = pop_sd,
      pop_mean = pop_mean)

  return(dat_out)
}
