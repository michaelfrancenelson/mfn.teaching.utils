#' Make a nice plot of a normal PDF
#'
#' @param x_1 x-values to plot
#' @param pop_mean mean of the normal distribution to plot
#' @param pop_sd standard deviation of the normal distribution to plot
#' @param xmin minimum x value for the plot
#' @param xmax maximum x value for the plot
#' @param len number of points to calculate for the curve
#' @param fill_cdf Color to fill the density area
#' @param x_lab label for the x-axis
#' @param y_lab label for the y-axis#'
#' @param digits the number of siginifcant digits to display in the title
#' @param lty_density the line type for the density curve
#' @param title_fmt The format string, to be passed to sprintf() for the title
#'
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @export
#'

norm_density_plot = function(
  x_1,
  pop_mean = 0,
  pop_sd = 1,
  xmin = NULL,
  xmax = NULL,
  len = 1000,
  fill_cdf = rgb(0, 0.3, 0.8, 0.15),
  digits = 2,
  lty_density = 2,
  x_lab = "x",
  y_lab = "f(x)",
  title_fmt =
    paste0(
      "x = %1$s\n",
      "probability density (height of the curve at x) = %2$s\n",
      "cumulative density (area of shaded region) = %3$s"))
{


  if (is.null(xmin)) xmin = pop_mean - 3 * pop_sd
  if (is.null(xmax)) xmax = pop_mean + 3 * pop_sd

  norm_dat = build_dnorm_dat(
    xmin = xmin,
    xmax = xmax,
    len = len,
    pop_mean = pop_mean,
    pop_sd = pop_sd)

  y_intercept = pnorm(x_1, mean = pop_mean, sd = pop_sd)

  return(
    ggplot(norm_dat) +
      geom_line(aes(x, y1)) +
      geom_ribbon(
        data = subset(norm_dat, x < x_1),
        mapping = aes(x = x, ymin = y0, ymax = y1),
        fill = fill_cdf) +
      geom_hline(yintercept = y_intercept, lty = lty_density) +
      ylab(y_lab) + xlab(x_lab) +
      ggtitle(paste0(
        sprintf(
          title_fmt,
          round(x_1, digits),
          round(y_intercept, digits),
          round(pnorm(x_1, mean = pop_mean, sd = pop_sd), digits)
        )))
  )
}


#' Make a nice example plot for a normal cumulative distribution function
#'
#' @param x_1 x-values to plot
#' @param pop_mean mean of the normal distribution to plot
#' @param pop_sd standard deviation of the normal distribution to plot
#' @param xmin minimum x value for the plot
#' @param xmax maximum x value for the plot
#' @param len number of points to calculate for the curve
#' @param fill_cdf Color to fill the density area
#' @param x_lab label for the x-axis
#' @param y_lab label for the y-axis
#' @param digits the number of siginifcant digits to display in the title
#' @param lty_density the line type for the density curve
#' @param title_fmt The format string, to be passed to sprintf() for the title
#'
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @export


norm_cdf_plot = function(
  x_1,
  pop_mean = 0,
  pop_sd = 1,
  xmin = NULL,
  xmax = NULL,
  len = 1000,
  fill_cdf = rgb(0, 0.3, 0.8, 0.15),
  digits = 2,
  lty_density = 2,
  x_lab = "x",
  y_lab = "f(x)",
  title_fmt = "x: %1$s\nquantile: %2$s")
{
  requireNamespace("ggplot2")

  if (is.null(xmin)) xmin = pop_mean - 3 * pop_sd
  if (is.null(xmax)) xmax = pop_mean + 3 * pop_sd

  norm_dat = build_pnorm_dat(
    xmin = xmin,
    xmax = xmax,
    len = len,
    pop_mean = pop_mean,
    pop_sd = pop_sd)

  # norm_dat = data.frame(
  #   x = x1 <- seq(xmin, xmax, length.out = len),
  #   y0 = 0 * x1,
  #   y1 = pnorm(x1, mean =pop_mean, sd = pop_sd)
  # ); rm(x1)

  y_intercept = pnorm(x_1, mean = pop_mean, sd = pop_sd)

  return(
    ggplot(norm_dat) +
      geom_line(aes(x, y1)) +
      geom_segment(
        mapping = aes(
          x = x_1, xend = x[1],
          y = y_intercept, yend = y_intercept),
        arrow = arrow(length = unit(0.03, "npc"))) +
      geom_segment(
        mapping = aes(
          x = x_1, xend = x_1,
          y = y_intercept, yend = 0),
        arrow = arrow(length = unit(0.03, "npc"))) +
      ylab(y_lab) + xlab(x_lab) +
      ggtitle(paste0(
        sprintf(
          title_fmt,
          round(x_1, digits),
          round(pnorm(x_1, mean = pop_mean, sd = pop_sd), digits)
        )))
  )
}
if(FALSE)
{
  source(here::here("function_scripts", "norm_density_plot.R"))
  source(here::here("function_scripts", "norm_quantile_plot.R"))
  source(here::here("function_scripts", "norm_cdf_plot.R"))
  dev.off()
  norm_density_plot(0.3)
  norm_cdf_plot(0.3)
  norm_quantile_plot(0.3)

  cdf_fmt = "x: %1$s\nquantile: %2$s"
  norm_cdf_plot(10.2, mean = 11.2, sd = 3, title_fmt = cdf_fmt)

  x1 = 10.0
  x2 = 0.3

  plot_grid(
    norm_density_plot(x1),
    norm_cdf_plot(x1),
    norm_quantile_plot(pnorm(x1)),
    norm_density_plot(x2),
    norm_cdf_plot(x2),
    norm_quantile_plot(pnorm(x2))

  )
}




#'
#' A nice demonstration plot of a normal quantile function
#'
#'
#' @param p_1 p
#' @param pop_mean mean of the normal distribution to plot
#' @param pop_sd standard deviation of the normal distribution to plot
#' @param xmin minimum x value for the plot
#' @param xmax maximum x value for the plot
#' @param len number of points to calculate for the curve
#' @param fill_cdf Color to fill the density area
#' @param x_lab label for the x-axis
#' @param y_lab label for the y-axis
#' @param digits the number of siginifcant digits to display in the title
#' @param lty_density the line type for the density curve
#' @param title_fmt The format string, to be passed to sprintf() for the title
#'
#' @import ggplot2
#'
#' @return A ggplot object
#'
#' @export

norm_quantile_plot = function(
  p_1,
  pop_mean = 0,
  pop_sd = 1,
  xmin = 0.001,
  xmax = 0.999,
  len = 1000,
  fill_cdf = rgb(0, 0.3, 0.8, 0.15),
  digits = 2,
  lty_density = 2,
  y_lab = "x",
  x_lab = "quantile",
  title_fmt = "quantile: %1$s\nx: %2$s\n")
{

  requireNamespace("ggplot2")

  norm_dat = build_qnorm_dat(
    xmin = xmin,
    xmax = xmax,
    len = len,
    pop_mean = pop_mean,
    pop_sd = pop_sd)

  y_intercept = qnorm(p_1, mean = pop_mean, sd = pop_sd)

  return(
    ggplot(norm_dat) +
      geom_line(aes(x, y1)) +
      geom_segment(
        mapping = aes(
          x = p_1, xend = x[1],
          y = y_intercept, yend = y_intercept),
        arrow = arrow(length = unit(0.03, "npc"))) +
      geom_segment(
        mapping = aes(
          x = p_1, xend = p_1,
          y = y_intercept, yend = y1[1]),
        arrow = arrow(length = unit(0.03, "npc"))) +
      ylab(y_lab) + xlab(x_lab) +
      ggtitle(paste0(
        sprintf(
          title_fmt,
          round(p_1, digits),
          round(y_intercept, digits))))
  )
}



#'
#'
#' @export
#'
build_dnorm_dat = function(
  xmin,
  xmax,
  len,
  pop_mean,
  pop_sd
)
{
  x = seq(xmin, xmax, length.out = len)
  norm_dat = data.frame(
    x = x,
    y0 = 0 * x,
    y1 = dnorm(x, mean = pop_mean, sd = pop_sd)
  )
  return(norm_dat)
}

#'
#'
#' @export
#'
build_qnorm_dat = function(
  xmin,
  xmax,
  len,
  pop_mean,
  pop_sd
)
{
  x = seq(xmin, xmax, length.out = len)
  norm_dat = data.frame(
    x = x,
    y0 = 0 * x,
    y1 = qnorm(x, mean = pop_mean, sd = pop_sd)
  )
  return(norm_dat)
}

#'
#'
#' @export
#'
build_pnorm_dat = function(
  xmin,
  xmax,
  len,
  pop_mean,
  pop_sd
)
{
    x = seq(xmin, xmax, length.out = len)
  norm_dat = data.frame(
    x = x,
    y0 = 0 * x,
    y1 = pnorm(x, mean = pop_mean, sd = pop_sd)
  )
  return(norm_dat)
}
