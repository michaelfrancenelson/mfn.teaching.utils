#' Sample standard error of the mean
#'
#'
#' @param x The data for which to calculate the SSE
#' @param pop_sd population standard deviation
#'
#' @export
#'
#' @examples
#'
#' # This should usually be around 0.1
#' sample_standard_error(rnorm(100))

sample_standard_error = function(
  x,
  pop_sd = NULL)
{
  sample_mean = mean(x, na.rm = TRUE)
  sample_sd   = sd(x, na.rm = TRUE)

  sample_sd =
    ifelse(
      is.null(pop_sd),
      sd(x, na.rm = TRUE),
      pop_sd
    )

  # must adjust sample size for missing data
  n = sum(!is.na(x))


    sample_se   = sample_sd / sqrt(n)
  return(sample_se)
}
