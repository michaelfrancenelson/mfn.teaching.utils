#' Parameterize a linear function from a point and slope
#'
#' @param x x-values for which to calculate corresponding y-values
#' @param x1 known x coordinate
#' @param y1 known y coordinate
#' @param slope known slope
#'
#'
#' @export


# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept =
    function(x1, y1, slope)
      return(-(x1 * slope) + y1)

  linear =
    function(x, yint, slope)
      return(yint + x * slope)

  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}
