#' Add line segment representation of residuals using base graphics
#'
#' @param x x-values
#' @param y_observed the observed y values
#' @param y_expected the expected (model-based) y values
#' @param lwd how wide should the residual lines be?
#' @param lty What type of line should they be?
#' @param col what color should they be?
#'
#' @import graphics
#'
#' @export

plot_resids = function(
  x,
  y_observed,
  y_expected,
  lwd = 2, lty = 1,
  col = "red")
{
  # x = x[i], guess_x, guess_y, guess_slope),
  # y0 = line_point_slope(
  #   x = x[i], guess_x, guess_y, guess_slope),
  for (i in 1:n)
  {
    segments(
      x0 = x[i],
      y0 = y_observed[i],
      x1 = x[i],
      y1 = y_expected[i],
      col = col, lwd = lwd,
      lty = lty)
  }
}
