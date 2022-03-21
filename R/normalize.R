#' Builds a one-way linear model that you can use for predicting new data values with desired properties.
#'
#' @param x values to normalize
#' @param x_min min value of the normalized output
#' @param x_max max value of the normalized output
#'
#' @export
#'

normalize = function(x, min = 0, max = 1)
{
  x_min = min(x, na.rm = TRUE)
  x_max = max(x, na.rm = TRUE)
  x_range = x_max - x_min
  x_2 = (x - x_min) / x_range
  out_range = max - min
  return(min + x_2 * out_range)
}
