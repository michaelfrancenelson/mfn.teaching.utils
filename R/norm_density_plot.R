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
#' @import stats
#'
#' @return A ggplot object
#'
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
    fill_cdf = rgb(0, 0.3, 0.8, 0.25),
    # fill_bkg = rgb(0, 0, 0, 0),
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

  if (FALSE)
  {
    x_1 = 0.1
    pop_mean = 0
    pop_sd = 1
    xmin = NULL
    xmax = NULL
    len = 1000
    fill_cdf = rgb(0, 0.3, 0.8, 0.25)
    digits = 2
    lty_density = 2
    x_lab = "x"
    y_lab = "f(x)"
    title_fmt =
      paste0(
        "x = %1$s\n",
        "probability density (height of the curve at x) = %2$s\n",
        "cumulative density (area of shaded region) = %3$s")
  }

  if (is.null(xmin)) xmin = pop_mean - 3 * pop_sd
  if (is.null(xmax)) xmax = pop_mean + 3 * pop_sd

  norm_dat = build_dnorm_dat(
    xmin = xmin,
    xmax = xmax,
    len = len,
    pop_mean = pop_mean,
    pop_sd = pop_sd)

  y_intercept = dnorm(x_1, mean = pop_mean, sd = pop_sd)

  out =  ggplot(norm_dat) +
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

  return(out)
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
#' @import stats
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
    fill_cdf = rgb(0, 0.3, 0.8, 0.25),
    # fill_bkg = rgb(0, 0, 0, 0),
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




#' A nice demonstration plot of a normal quantile function
#'
#' @param p_1 p
#' @param pop_mean mean of the normal distribution to plot
#' @param pop_sd standard deviation of the normal distribution to plot
#' @param xmin minimum x value for the plot
#' @param xmax maximum x value for the plot
#' @param len number of points to calculate for the curve
#' @param fill_cdf Color to fill the density area
#' @param fill_cdf Color to fill background
#' @param x_lab label for the x-axis
#' @param y_lab label for the y-axis
#' @param digits the number of siginifcant digits to display in the title
#' @param lty_density the line type for the density curve
#' @param title_fmt The format string, to be passed to sprintf() for the title
#'
#' @import ggplot2
#' @import stats
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
    fill_cdf = rgb(0, 0.3, 0.8, 0.25),
    # fill_bkg = rgb(0, 0, 0, 0),
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



#' Build data for a pdf curve for a normal dist
#'
#' @param xmin minimum x-value
#' @param xmax maximum x-value
#' @param len how many points to create for the curve
#' @param pop_mean mean for the population
#' @param pop_sd standard deviation for the population
#'
#' @import stats
#'
#' @export
#'

build_dnorm_dat = function(
    xmin = -2,
    xmax = 2,
    len = 100,
    pop_mean = 0,
    pop_sd = 1
)
{
  if (FALSE)
  {
    xmin = -2
    xmax = 2
    len = 100
    pop_mean = 0
    pop_sd = 1
  }
  x = seq(xmin, xmax, length.out = len)
  norm_dat = data.frame(
    x = x,
    x1 = rev(x),
    y0 = 0 * x,
    y1 = dnorm(x, mean = pop_mean, sd = pop_sd)
  )
  return(norm_dat)
}


#' Build data for a quantile curve for a normal dist
#'
#' @param xmin minimum x-value
#' @param xmin minimum x-value
#' @param xmax maximum x-value
#' @param len how many points to create for the curve
#' @param pop_mean mean for the population
#' @param pop_sd standard deviation for the population
#'
#' @import stats
#'
#' @export
#'
build_qnorm_dat = function(
    xmin,
    xmax,
    len = 100,
    pop_mean = 0,
    pop_sd = 1
)
{
  x = seq(xmin, xmax, length.out = len)
  norm_dat = data.frame(
    x = x,
    y0 = 0 * x,
    y1 = qnorm(x, mean = pop_mean, sd = pop_sd),
    x1 = rev(x)
  )
  return(norm_dat)
}

#' Build data for a cdf curve for a normal dist
#'
#' @param xmin minimum x-value
#' @param xmin minimum x-value
#' @param xmax maximum x-value
#' @param len how many points to create for the curve
#' @param pop_mean mean for the population
#' @param pop_sd standard deviation for the population
#'
#' @import stats
#'
#' @export
#'
build_pnorm_dat = function(
    xmin = -2,
    xmax = 2,
    len = 100,
    pop_mean = 0,
    pop_sd = 1
)
{
  x = seq(xmin, xmax, length.out = len)
  norm_dat = data.frame(
    x = x,
    y0 = 0 * x,
    y1 = pnorm(x, mean = pop_mean, sd = pop_sd),
    x1 = rev(x)
  )
  return(norm_dat)
}


#' Plot a normal distribution with one or both tails shaded
#'
#' @param lower_tail lower tail to plot
#' @param upper_tail upper tail to plot
#' @param pop_mean mean for the population
#' @param pop_sd standard deviation for the plot
#' @param xmin lower limit of x values to include
#' @param xmax upper limit of x values to include
#' @param len how many points to generate?
#' @param fill_lower fill color for the lower tail
#' @param fill_middle fill color for the non-tail part of the pdf
#' @param fill_upper fill color for the upper tail
#' @param y_lab label for x axis
#' @param x_lab label for y axis
#'
#' @export

plot_norm_tails = function(
    lower_tail = 0.025,
    upper_tail = 0.975,
    pop_mean = 0,
    pop_sd = 1,
    xmin = NULL,
    xmax = NULL,
    len = 1000,
    fill_lower = rgb(0, 0.3, 0.8, 0.25),
    fill_middle = rgb(0, 0, 0, 0),
    fill_upper = rgb(0, 0.3, 0.8, 0.25),
    y_lab = "f(x)",
    x_lab = "x"

)
{

  if (FALSE)
  {
    lower_tail = 0.05
    upper_tail = 0.925
    upper_tail = NULL
    x_1 = 0.1
    pop_mean = 0
    pop_sd = 1
    xmin = NULL
    xmax = NULL
    len = 1000
    fill_upper = rgb(0, 0.3, 0.8, 0.25)
    fill_lower = rgb(0, 0.3, 0.8, 0.25)
    fill_middle = rgb(0, 0, 0, 0)
    digits = 2
    lty_density = 2
    x_lab = "x"
    y_lab = "f(x)"
    title_fmt =
      paste0(
        "x = %1$s\n",
        "probability density (height of the curve at x) = %2$s\n",
        "cumulative density (area of shaded region) = %3$s")
  }

  if (is.null(xmin)) xmin = pop_mean - 3 * pop_sd
  if (is.null(xmax)) xmax = pop_mean + 3 * pop_sd

  norm_dat = build_dnorm_dat(
    xmin = xmin,
    xmax = xmax,
    len = len,
    pop_mean = pop_mean,
    pop_sd = pop_sd)

  lower_crit = upper_crit = NULL

  if (!is.null(upper_tail))
    upper_crit = qnorm(upper_tail, mean = pop_mean, sd = pop_sd)

  if (!is.null(lower_tail))
    lower_crit = qnorm(lower_tail, mean = pop_mean, sd = pop_sd)

  ribbon_dat = build_tail_dat(
    norm_dat,
    lower_x = lower_crit,
    upper_x = upper_crit)

  gg_ribbons = build_ribbons(
    ribbon_dat,
    fill_lower = fill_lower,
    fill_middle = fill_middle,
    fill_upper = fill_upper)

  out =   ggplot(norm_dat) +
    geom_line(aes(x, y1)) +
    gg_ribbons$middle +
    gg_ribbons$tails$lower +
    gg_ribbons$tails$upper +
    ylab(y_lab) + xlab(x_lab)


  return(out)
}


#' Build data for a pdf curve for a t-distribution
#'
#' @param df_sample sample degrees of freedom
#' @param ncp_sample non-centrality parameter
#' @param xmin lower limit of x values to include
#' @param xmax upper limit of x values to include
#' @param len how many points to generate?
#'
#' @import stats
#'
#' @export
#'
build_dt_dat = function(
    df_sample,
    ncp_sample,
    xmin,
    xmax,
    len
)
{
  x = seq(xmin, xmax, length.out = len)
  t_dat = data.frame(
    x = x,
    y0 = 0 * x,
    y1 = dt(x, df = df_sample, ncp = ncp_sample)
  )
  return(t_dat)
}




#' Plot a t distribution with one or both tails shaded
#'
#' @param lower_tail lower tail to plot
#' @param upper_tail upper tail to plot
#' @param df_sample sample degrees of freedom
#' @param ncp_sample non-centrality parameter
#' @param xmin lower limit of x values to include
#' @param xmax upper limit of x values to include
#' @param len how many points to generate?
#' @param fill_lower fill color for the lower tail
#' @param fill_middle fill color for the non-tail part of the pdf
#' @param fill_upper fill color for the upper tail
#' @param y_lab label for x axis
#' @param x_lab label for y axis
#'
#'
#' @export

plot_t_tails = function(
    lower_tail = 0.025,
    upper_tail = 0.925,
    df_sample = 30,
    ncp_sample = 0,
    xmin = NULL,
    xmax = NULL,
    len = 1000,
    fill_lower = rgb(0, 0.3, 0.8, 0.25),
    fill_middle = rgb(0, 0, 0, 0),
    fill_upper = rgb(0, 0.3, 0.8, 0.25),
    y_lab = "f(x)",
    x_lab = "x"#,
    # t_crit = 0.05
)
{

  if (FALSE)
  {
    lower_tail = 0.05
    upper_tail = 0.975
    upper_tail = NULL

    df_sample = 30
    ncp_sample = 0

    xmin = NULL
    xmax = NULL
    len = 1000

    fill_lower = rgb(0, 0.3, 0.8, 0.25)
    fill_middle = rgb(0, 0, 0, 0)
    fill_upper = rgb(0, 0.3, 0.8, 0.25)

    x_lab = "t"
    y_lab = "f(x)"
    t_crit = 0.05
    title_fmt =
      paste0(
        "x = %1$s\n",
        "probability density (height of the curve at x) = %2$s\n",
        "cumulative density (area of shaded region) = %3$s")
  }

  if (is.null(xmin)) xmin = -3
  if (is.null(xmax)) xmax =  3

  t_dat = build_dt_dat(
    df_sample = df_sample,
    ncp_sample = ncp_sample,
    xmin = xmin,
    xmax = xmax,
    len = len)

  lower_crit = upper_crit = NULL

  if (!is.null(upper_tail))
    upper_crit = qt(upper_tail, df = df_sample, ncp = ncp_sample)

  if (!is.null(lower_tail))
    lower_crit = qt(lower_tail, df = df_sample, ncp = ncp_sample)

  ribbon_dat = build_tail_dat(
    t_dat,
    lower_x = lower_crit,
    upper_x = upper_crit)

  gg_ribbons = build_ribbons(
    ribbon_dat,
    fill_lower = fill_lower,
    fill_middle = fill_middle,
    fill_upper = fill_upper)

  return(

    ggplot(t_dat) +
      geom_line(aes(x, y1)) +
      gg_ribbons$middle +
      gg_ribbons$tails$lower +
      gg_ribbons$tails$upper +
      ylab(y_lab) + xlab(x_lab)
  )
}



#' Separate the lower and upper tails for a pdf dataset
#'
#' @param dat data from which to build the tails
#' @param lower_x lower x-limit for the tail
#' @param upper_x upper x-limit for the tail
#'
#'
#' @export

build_tail_dat = function(
    dat,
    lower_x = -1.96,
    upper_x = NULL)
{
  if (FALSE)
  {
    dat = build_dnorm_dat(-3, 3, 1000, 0, 1)
    lower_x = -1.96
    upper_x = 1
    upper_x = NULL
  }

  middle_dat = dat
  lower_dat = upper_dat = NULL

  if (!is.null(lower_x))
  {
    lower_dat = subset(dat, x < lower_x)
    middle_dat = subset(middle_dat, x > lower_x)
  }

  if (!is.null(upper_x))
  {
    upper_dat = subset(dat, x > upper_x)
    middle_dat = subset(middle_dat, x < upper_x)
  }

  return(
    list(
      tails = list(
        lower = lower_dat,
        upper = upper_dat),
      middle = middle_dat)
  )
}


#' Build geom_ribbon objects for plotting a continuous distribution function
#'
#' @param ribbon_dat data from which to beuld the ribbons
#' @param fill_lower fill color for the lower tail
#' @param fill_middle fill color for the non-tail part of the pdf
#' @param fill_upper fill color for the upper tail
#' @param col_lower border color for the lower tail
#' @param col_middle border color for the non-tail part
#' @param col_upper border color for the upper tail
#'
#' @import ggplot2
#'
#' @export


build_ribbons = function(
    ribbon_dat,
    fill_lower = rgb(0, 0.3, 0.8, 0.25),
    fill_middle = rgb(0, 0, 0, 0),
    fill_upper = rgb(0, 0.3, 0.8, 0.25),
    col_lower = "black",
    col_middle = "black",
    col_upper = "black")
{
  if(is.null(ribbon_dat$tails$lower))
  {
    ribbon_lower = NULL
  } else
  {
    ribbon_lower =
      geom_ribbon(
        data = ribbon_dat$tails$lower,
        mapping = aes(x = x, ymin = y0, ymax = y1),
        fill = fill_lower)
  }
  if(is.null(ribbon_dat$tails$upper))
  {
    ribbon_upper = NULL
  } else
  {
    ribbon_upper =
      geom_ribbon(
        data = ribbon_dat$tails$upper,
        mapping = aes(x = x, ymin = y0, ymax = y1),
        fill = fill_upper)
  }

  ribbon_middle =
    geom_ribbon(
      data = ribbon_dat$middle,
      mapping = aes(x = x, ymin = y0, ymax = y1),
      fill = fill_middle)

  return(
    list(
      tails = list(
        lower = ribbon_lower,
        upper = ribbon_upper),
      middle = ribbon_middle,
      lower = ribbon_lower,
<<<<<<< HEAD
      upper = ribbon_upper),
    middle = ribbon_middle,
    lower = ribbon_lower,
    upper = ribbon_upper))
=======
      upper = ribbon_upper))
>>>>>>> 8b975854549e70c58662d09ca2c5f2626b59247f
}

gg_norm_conf_int()
#' Plot a confidence interval on a normal curve
#'
#' @param alpha alpha for the fill
#' @param pop_mean mean for the pop
#' @param pop_sd sd for the pop
#' @param xmin min x
#' @param xmax max s
#' @param len how many points to make for the curve.  Higher number results in smoother curve
#' @param fill_lower fill color for the lower tail
#' @param fill_middle fill color for the non-tail part of the pdf
#' @param fill_upper fill color for the upper tail
#' @param y_lab labe for y axis
#' @param x_lab label for x asxis
#' @param lty_v line type for the upper and lower quantile limits
#' @param title_fmt template for title
#' @param arrow_label_fmt template for arrow lables
#' @param arrow_size size for the horizontal arrow for the CI width label
#' @param digits_label how many digits to round for the label
#' @param digits_axis how many digits to round for the axes
#' @param x_auto_breaks
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @export
#'
gg_norm_conf_int = function(
    alpha = 0.05,
    pop_mean = 0,
    pop_sd = 1,
    xmin = NULL,
    xmax = NULL,
    len = 1000,
    fill_upper = rgb(0, 0.3, 0.8, 0.25),
    fill_lower = rgb(0, 0.3, 0.8, 0.25),
    fill_middle = rgb(0, 0, 0, 0),
    y_lab = "f(x)",
    x_lab = "x",
    lty_v = 2,
    title_fmt = "Interval contains %.1f%s of probabiltiy density.",
    arrow_label_fmt = "$%1$0.1f \\pm %2$0.2f \\times \\sigma$",
    arrow_size = 0.3,
    digits_label = 0,
    digits_axis = 3,
    x_auto_breaks = -1:1
)
{
  if (FALSE)
  {
    alpha = 0.05
    pop_mean = 0
    pop_sd = 1
    xmin = NULL
    xmax = NULL
    len = 1000
    fill_upper = rgb(0, 0.3, 0.8, 0.25)
    fill_lower = rgb(0, 0.3, 0.8, 0.25)
    fill_middle = rgb(0, 0, 0, 0)
    y_lab = "f(x)"
    x_lab = "x"
    lty_v = 2
    title_fmt = "Interval contains %.1f%s of probabiltiy density."
    arrow_label_fmt = "$%1$0.1f \\pm %2$0.2f \\times \\sigma$"
    arrow_size = 0.3
    digits_label = 0
    digits_axis = 3
    x_auto_breaks = -1:1
  }


  # Convenience variables
  {
    pct_interval = round(100 * (1 - alpha), digits = digits_label)
    alpha_low = alpha / 2
    alpha_hi = 1 - alpha / 2
    q_low = qnorm(alpha_low)
    q_hi = qnorm(alpha_hi)
    y_intercept = dnorm(q_low)
  }

  g_title = sprintf(title_fmt, pct_interval, "%")
  g_arrow_label = TeX(sprintf(arrow_label_fmt, pop_mean, abs(q_low)))

  x_breaks = c(round(q_low, digits_axis), round(q_hi, digits_axis), x_auto_breaks)


  arrow_dat = data.frame(
    x = q_low,
    xend = q_hi,
    y = y_intercept,
    yend = y_intercept)

  gg_t = plot_norm_tails(
    lower_tail = alpha_low,
    upper_tail = alpha_hi,
    pop_mean = pop_mean,
    pop_sd = pop_sd,
    xmin = xmin,
    xmax = xmax,
    len = len,
    fill_middle = rgb(0, 0.3, 0.8, 0.25),
    fill_lower = rgb(.7, 0, 0, 0.3),
    fill_upper = rgb(.7, 0, 0, 0.3),
    y_lab = y_lab,
    x_lab = x_lab
  )

  out =
    gg_t +
    geom_vline(xintercept = q_low, lty = lty_v) +
    geom_vline(xintercept = q_hi, lty = lty_v) +
    geom_segment(
      data = arrow_dat, mapping = aes(x = x, xend = xend, y = y, yend = yend),
      arrow = arrow(length=unit(arrow_size ,"cm"), ends="both", type = "closed")) +
    ylab(y_lab) + xlab(x_lab) +
    scale_x_continuous(breaks = x_breaks) +
    ggtitle(g_title) +
    annotate("label", x = 0, y = arrow_dat$y, label = g_arrow_label)

  return(out)
}
