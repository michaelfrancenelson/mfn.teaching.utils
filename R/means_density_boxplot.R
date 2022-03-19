#' Draw a figure showing two distributions via density and boxplots.
#'
#' Inspired for Chris Sutherland's figures for Intro to
#' Quantitiative Ecology at UMass Amherst
#'
#'
#'
#' @export

means_density_boxplot = function(
  x1, x2, xlm, ylm,
  col_dens = 1, col_1 = 2, col_2 = 4,
  col_adj = 0.3, lwd_mean = 2, lwd_dens = 1)
{
  par(mfrow = c(2, 1), oma = c(0, 0, 0, 0), mar = c(2, 2, 1, 1))
  plot(density(x1), lwd = lwd_dens, xlim = xlm, ylim = ylm,
       main = "", axes = F, xlab = "", ylab = "", col = 0)
  abline(v = mean(x1), lwd = lwd_mean, col = col_1)
  abline(v = mean(x2), lwd = lwd_mean, col = col_2)
  lines(density(x1), col = col_dens, lwd = lwd_dens)
  lines(density(x2), col = col_dens, lwd = lwd_dens)
  axis(1)
  box(bty = "l")
  boxplot(cbind(x1, x2), ylim = xlm, horizontal = TRUE, pch = "",
          col = adjustcolor(c(col_1, col_2), col_adj), axes = F)
  axis(1)
  abline(v = mean(x1), lwd = 2, col = 2)
  abline(v = mean(x2), lwd = 2, col = 4)
  boxplot(cbind(x1, x2), ylim = xlm, horizontal = TRUE, pch = "",
          col = adjustcolor(c(col_1, col_2), col_adj), axes = F, add = T)
  box(bty = "l")
}
