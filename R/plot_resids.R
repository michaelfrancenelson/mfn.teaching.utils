#' Add line segment representation of residuals using base graphics
#'
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
