#' Plot one or more t-distributions
#'
#' Shows the shape of different t-distributions with differing degrees of freedom.
#' Can also include a plot of the normal as a reference.
#'
#' @param df either a single value or a vector of degrees of freedom to plot
#' @param xlim limits of the x-values to plot
#' @param n_x how many points to simulate.  Larger number = smoother curve
#' @param col colors for the t curves
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main_fmt main title formatter
#' @param hl_y height of the horizontal line to draw
#' @param hl_lty type of horizontal line
#' @param hl_col color for horizontal line
#' @param oma outer margin
#' @param mar main figure margins
#' @param ylim y-limits for plotting
#' @param norm should the standard normal be plotted as a reference?
#' @param col_norm color for normal line
#' @param lty_norm line style for normal
#'
#' @import graphics
#'
#' @export

plot_t_dist = function(
  df,
  xlim = c(-3, 3), n_x = 500,
  col = rep(1, length(df)),
  xlab = "", ylab = "Probability Density",
  main_fmt = "Degrees of Freedom = %d",
  hl_y = 0.05, hl_lty = 3, hl_col = 2,
  oma = c(0, 0, 0, 0),
  mar = c(3, 4, 2, 0.5),
  ylim = c(0, 0.4),
  # type = "l",
  norm = TRUE,
  col_norm = 2,
  lty_norm = 2,
  ...
)
{
  par(mar = mar, oma = oma)
  x = seq(from = xlim[1], to = xlim[2], length.out = n_x)

  plot(
    x, dt(x, df = df[1]),
    col = col[1],
    ...,
    type = "l",
    xlab = xlab,
    ylab = ylab,
    ylim = ylim,
    main = sprintf(main_fmt, df[1]))

  if (length(df) > 1)
    for (i in 2:length(df))
    {
      plot(
        x, dt(x, df = df[i]),
        col = col[i],
        type = "l",
        ...)
      # type = type, col = col[i], lwd = lwd,
      # las = las, bty = bty)
    }

  if (norm)
    lines(
      x, dnorm(x),
      type = "l",
      lty = lty_norm,
      col = col_norm,
      # lty = lty_norm,
      ...)


  abline(h = hl_y, lty = hl_lty, col = hl_col)

  # if (!is.null(pdf) | !is.null(png)) dev.off()
}
