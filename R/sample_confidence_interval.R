#' Create simulated confidence intervals
#'
#' Simulation of Frequentist confidence intervals of the mean of
#' normally-distributed data.
#'
#' @param x The sample data
#' @param alpha Confidence level
#' @param pop_sd Population standard deviation (if known).
#'               If this is provided, the interval is based on a z-score rather
#'               than a t-value
#' @export
#'
#' @examples
#'
#' sample_confidence_interval(rnorm(100), pop_sd = 1)
#'
sample_confidence_interval = function(
  x,
  alpha = 0.05,
  pop_sd = NA)
{

  if (!is.na(pop_sd))
  {
    test_stat = qnorm(1 - (alpha * 0.5))
    standard_deviation = pop_sd
    standard_error = pop_sd / sqrt(length(x))
    sample_err = standard_error * test_stat
  } else
  {
    test_stat = qt(1 - (alpha * 0.5), df = length(x))
    standard_deviation = sd(x)
    standard_error = sample_standard_error(x)
    sample_err = standard_error * test_stat
  }

  sample_mean = mean(x)

  # Confidence intervals
  lower = sample_mean - sample_err
  upper = sample_mean + sample_err

  return(c(
    lower = lower,
    upper = upper,
    standard_error = standard_error,
    sample_error = sample_err,
    sample_mean = sample_mean,
    test_statistic = test_stat))
}



# simulated_confidence_intervals_of_mean = function(
#   n_samples,
#   n_intervals = 1,
#   alpha = 0.05,
#   pop_mean = 0,
#   pop_sd = 1)
# {
#   if (FALSE)
#   {
#     n_intervals = 1
#     n_samples = 3000
#     alpha = 0.05
#     pop_mean = 0
#     pop_sd = 1
#     i = 1
#   }
#
#   dat_out = data.frame(matrix(
#     0,
#     nrow = n_intervals,
#     ncol = 15))
#
#   # significance test stats
#   t_crit = qt(1 - (alpha * 0.5), df = n_samples)
#   z_crit = qnorm(1 - (alpha * 0.5))
#
#   pop_s_err = pop_sd / sqrt(n_samples)
#   pop_err = pop_s_err * z_crit
#
#   for (i in 1:n_intervals)
#   {
#     # random data
#     x_i = rnorm(n_samples, mean = pop_mean, sd = pop_sd)
#
#     # sample stats
#     sample_mean = mean(x_i)
#     sample_sd = sd(x_i)
#     sample_se = sample_sd / sqrt(n_samples)
#
#     sample_err = sample_se * t_crit
#
#     # Confidence intervals
#     lower_t = sample_mean - sample_err
#     upper_t = sample_mean + sample_err
#     lower_z = sample_mean - pop_err
#     upper_z = sample_mean + pop_err
#
#     (sample_mean < upper_z) & (sample_mean > lower_z)
#     (sample_mean < upper_t) & (sample_mean > lower_t)
#
#     dat_out[i, ] =
#       c(
#         i,
#
#         pop_mean,
#         pop_sd,
#         pop_s_err,
#
#         sample_mean,
#         sample_sd,
#         sample_se,
#
#
#         z_crit,
#         t_crit,
#
#         lower_t,
#         upper_t,
#
#         lower_z,
#         upper_z,
#
#
#         (pop_mean < upper_z) & (pop_mean > lower_z),
#         (pop_mean < upper_t) & (pop_mean > lower_t)
#       )
#
#   }
#
#   names(dat_out) = c(
#     "simulation",
#     "population_mean",
#     "population_standard_deviation",
#     "population_standard_error",
#     "sample_mean",
#     "sample_standard_deviation",
#     "sample_standard_error",
#     "critical_z",
#     "critical_t",
#     "upper_t",
#     "lower_t",
#     "upper_z",
#     "lower_z",
#     "in_ci",
#     "in_sample_ci")
#
#   dat_out$in_ci = as.logical(dat_out$in_ci)
#   dat_out$in_sample_ci = as.logical(dat_out$in_sample_ci)
#
#   return(dat_out)
# }
#
#
# n_intervals = 1000
# ddd = simulated_confidence_intervals_of_mean(n_samples = 30, n_intervals = n_intervals)
#
# sum(ddd$in_ci) / n_intervals
# sum(ddd$in_sample_ci) / n_intervals

