#' Create simulated confidence intervals
#'
#' Add line segment representation of residuals using base graphics
#'
#' @param n_per_group how many observations in each group
#' @param group_means means for the cells (groups)
#' @param group_sd sd for the groups
#' @param cols colors for the groups
#' @param pt_alpha how transparent shoudl the points be?
#'
#' @import data.table
#'
#' @return a data.table with columns for TODO
#'
#' @export
#'

mk_ss_data = function(
  n_per_group = 30,
  group_means = c(2, 6, 4),
  group_sd = 0.5,
  cols = NULL,
  pt_alpha = 0.6)
{
  if (FALSE)
  {
    n_per_group = c(3, 4, 2)
    group_means = c(2, 4, 6)
    group_sd = 0.2
    cols = 2:4
    pt_alpha = 0.6
  }

  if (length(n_per_group) > 1)
  {
    stopifnot(
      length(n_per_group) == length(group_means)
    )
  }

  if (length(group_sd) > 1)
  {
    stopifnot(
      length(group_means) == length(group_sd)
    )
  }

  if (length(group_sd) == 1)
    group_sd = rep(group_sd, length(group_means))

  if (length(n_per_group) != length(group_means))
    n_per_group = rep(n_per_group, length(group_means))

  if(is.null(cols)) cols = 1:length(group_means)

  dat_ss_plots = data.table(
    y = rnorm(
      sum(n_per_group),
      mean = rep(group_means, times = n_per_group),
      sd = rep(group_sd, times = n_per_group)),
    x = 1:sum(n_per_group),
    group = rep(letters[1:length(n_per_group)], times = n_per_group),
    col = rep(adjustcolor(cols, pt_alpha), times = n_per_group),
    col_mean = rep(cols, times = n_per_group))

  dat_ss_plots[, group_mean := mean(y), by = list(group)]

  return(dat_ss_plots)
  # dat_ss_plots[, plot(y ~ x, col = col, pch = 16)]
}




ss_par = function() par(oma = c(0,0,0,0),mar = c(1,3,2,.1))


#' Overlay points for the sums of squares on a base R plot
#'
#' @param dat data for which to draw the points
#' @param pt_cex size of the points
#' @param lwd width of the point border
#' @param pch_1 plotting character for main point
#' @param pch_2 plotting character for border point
#'
#'


ss_pts = function(
  dat,
  pt_cex = 1.5,
  lwd = 0.5,
  pch_1 = 16,
  pch_2 = 1)
{
  points(dat$y,pch = pch_1, col = dat$col, cex = pt_cex)
  points(dat$y,pch = pch_2, col = dat$col, cex = pt_cex, lwd = lwd)
}



#'
#'
p1 = function(
  dat,
  ylab = "", xlab = "",
  grand_mean_col = "black",
  grand_mean_lwd = 2)
{
  # plot(dat$y ,type = "n",bty = "l",xlab = xlab, ylab = ylab,axes = F, ylim = c(20,130))
  plot(
    dat$y, type = "n",
    bty = "l",
    xlab = xlab,
    ylab = ylab,
    axes = F,
    ylim = range(dat$y))
  dat[, segments(
    x0 = min(x),
    x1 = max(x),
    y0 = mean(y),
    lwd = grand_mean_lwd,
    col = grand_mean_col)]
  axis(2,las = 1); box(bty = "l")
}


#' Create a graphical representation of the total sum of squares in an ANOVA style analysis.
#'
#' @param dat data for which to draw.  If null, data with 3 groups will be generated
#' @param seg_col color for the vertical line segments
#' @param seg_lwd = width for vertical line segments
#' @param grand_mean_col color for grand mean horizontal line
#' @param group_mean_col line color for the vertical lines
#' @param grand_mean_lwd line width for the grand mean horizontal line
#' @param group_mean_lwd line width for the group mean colors
#' @param seed random seed to be used if dat is NULL
#' @param show_label whether or not to print the SS quantity
#' @param label_fmt The text format for the sum of squares label (if )
#' @param digits how many digits to round the SS to?
#'
#' @inheritParams ss_pts
#'
#' @import data.table
#' @import grDevices
#' @import graphics
#'
#' @return a data.table with columns for TODO
#'
#' @export

# total
plot_anova_sst = function(
  dat = NULL,
  seg_col = gray(0.75),
  seg_lwd = 1,
  group_mean_col = "black",
  group_mean_lwd = 2,
  grand_mean_col = "black",
  grand_mean_lwd = 2,
  show_label = TRUE,
  label_fmt = "SST = %0.1f",
  label_cex = 1.3,
  digits = 1,
  pt_cex = 1.5,
  pt_lwd = 0.5,
  pch_1 = 16,
  pch_2 = 1,
  seed = 12345, ...)
{
  if(is.null(dat))
  {
    set.seed(seed)
    dat = mk_ss_data(cols = c(1, 2, 4))
  }

  p1(dat, grand_mean_col = grand_mean_col, grand_mean_lwd = grand_mean_lwd)
  dat[, segments(
    x0 = x, y0 = mean(y),
    x1 = x, y1 = y,
    col = seg_col,
    lwd = seg_lwd)]
  dat[,
      segments(
        x0 = min(x), x1 = max(x),
        y0 = mean(y), y1 = mean(y),
        col = group_mean_col,
        lwd = group_mean_lwd)]
  ss_pts(dat, pt_cex = pt_cex, lwd = pt_lwd, pch_1 = pch_1, pch_2 = pch_2)

  if(show_label)
  {
    ss = calc_ss(dat, digits = digits)
    mtext(text = sprintf(label_fmt, ss$sst), cex = label_cex)
  }

}



#' Create a graphical representation of the within-group sum of squares in an ANOVA style analysis.
#'
#' @inheritParams plot_anova_sst
#' @inheritParams ss_pts
#'
#' @import data.table
#' @import grDevices
#' @import graphics
#'
#' @export

plot_anova_ssw = function(
  dat = NULL,
  seg_col = gray(0.75),
  seg_lwd = 1,
  group_mean_lwd = 2,
  grand_mean_col = "black",
  grand_mean_lwd = 2,
  show_label = TRUE,
  label_fmt = "SSW = %0.1f",
  label_cex = 1.3,
  digits = 1,
  pt_cex = 1.5,
  pt_lwd = 0.5,
  pch_1 = 16,
  pch_2 = 1,
  seed = 12345)
{

  if(is.null(dat))
  {
    set.seed(seed)
    dat = mk_ss_data(cols = c(1, 2, 4))
  }


  if(FALSE)
  {
    dat = NULL
    seg_col = gray(0.75)
    seg_lwd = 1
    group_mean_lwd = 2
    grand_mean_col = "black"
    grand_mean_lwd = 2
    show_label = TRUE
    label_fmt = "SSWS = %0.1f"
    label_cex = 1.3
    digits = 1
    seed = 12345
  }

  p1(dat, grand_mean_col = grand_mean_col, grand_mean_lwd = grand_mean_lwd)
  dat[, segments(
    x0 = x, y0 = group_mean,
    x1 = x, y1 = y,
    col = seg_col,
    lwd = seg_lwd)]
  dat[,
      segments(
        x0 = min(x), x1 = max(x),
        y0 = mean(y), y1 = mean(y),
        col = col_mean[1],
        lwd = group_mean_lwd),
      by = group]
  ss_pts(dat, pt_cex = pt_cex, lwd = pt_lwd, pch_1 = pch_1, pch_2 = pch_2)

  if(show_label)
  {
    ss = calc_ss(dat, digits = digits)
    mtext(text = sprintf(label_fmt, ss$ssw), cex = label_cex)
  }

}


#' Create a graphical representation of the between-group sum of squares in an ANOVA style analysis.
#'
#' @inheritParams plot_anova_sst
#' @inheritParams ss_pts
#'
#' @import data.table
#' @import grDevices
#' @import graphics
#'
#' @export

plot_anova_ssb = function(
  dat = NULL,
  seg_col = gray(0.75),
  seg_lwd = 1,
  grand_mean_col = "black",
  grand_mean_lwd = 0.8,
  group_mean_lwd = 2,
  show_label = TRUE,
  label_fmt = "SSB = %0.1f",
  label_cex = 1.3,
  digits = 1,
  pt_cex = 1.5,
  pt_lwd = 0.5,
  pch_1 = 16,
  pch_2 = 1,
  seed = 12345)
{


  if(is.null(dat))
  {
    set.seed(seed)
    dat = mk_ss_data(cols = c(1, 2, 4))
  }

  p1(dat, grand_mean_col = grand_mean_col, grand_mean_lwd = grand_mean_lwd)

  if(show_label)
  {
    ss = calc_ss(dat, digits = digits)
    mtext(text = sprintf(label_fmt, ss$ssb), cex = label_cex)
  }
  dat[,
      segments(
        x0 = x, y0 = mean(y),
        x1 = x, y1 = group_mean,
        col = seg_col,
        lwd = seg_lwd)]
  dat[,
      segments(
        x0 = min(x), x1 = max(x),
        y0 = mean(y), y1 = mean(y),
        col = col_mean[1],
        lwd = group_mean_lwd),
      by = group]
  ss_pts(dat, pt_cex = pt_cex, lwd = pt_lwd, pch_1 = pch_1, pch_2 = pch_2)
  dat[,
      segments(
        x0 = min(x), x1 = max(x),
        y0 = mean(y),
        lwd = grand_mean_lwd,
        col = grand_mean_col)]

}



#' Calculate the various sums of squares
#'
#' @param dat data for which to calculate ss
#' @param digits how many digits to round for the calculation?
#'
#' @export

calc_ss = function(dat, digits = -1)
{
  return(list(
    sst = round(dat[, sum((y - mean(y))^2)], digits = digits),
    ssw = round(dat[, sum((y - group_mean)^2)], digits = digits),
    ssb = round(dat[, sum((group_mean - mean(y))^2)], digits = digits)
  ))
}



#' Calculate the various mean sums of squares
#'
#' @param dat data for which to calculate ss
#' @param digits how many digits to round for the calculation?
#'
#' @export

calc_ms = function(dat, digits = -1)
{

  n = nrow(dat)
  g = length(unique(dat$group))

  df_tot = n - 1
  df_within = n - g
  df_between = g - 1

  list(
    mst = round(dat[, sum((y - mean(y))^2)] / df_tot, digits = digits),
    msw = round(dat[, sum((y - group_mean)^2)] / df_within, digits = digits),
    msb = round(dat[, sum((group_mean - mean(y))^2)] / df_between, digits = digits)
  )
}



# Test cases ----
if (FALSE)
{


  if (FALSE)
  {
    ss_height = 3; ss_width = 7
    ylab = ""; xlab = ""

    seg_col = gray(0.75)
    seg_lwd = 1

    group_mean_col = "black"
    group_mean_lwd = 2

    pt_cex = 1.5

    grand_mean_col = "black"
    grand_mean_lwd = 0.8
  }

  require(data.table)
  n_per_group = c(12, 2, 5)
  n_per_group = 30
  group_means = c(40, 100, 87)
  group_sd = 6.8

  cols = c(col = rgb(1, 0, 0), col = rgb(0, 1, 0), col = rgb(0, 0, 1))
  pt_alpha = 0.45

  set.seed(666)
  dat_1 = mk_ss_data(n_per_group, group_means, group_sd, cols, pt_alpha)

  set.seed(666)
  dat_2 = mk_ss_data(n_per_group, c(70, 72, 69), 16, cols, pt_alpha)



  # ylab = ""; xlab = ""

  # seg_col = gray(0.75)
  # seg_lwd = 1
  #
  # group_mean_col = "black"
  # group_mean_lwd = 2
  #
  # pt_cex = 1.5
  #
  # grand_mean_col = "black"
  # grand_mean_lwd = 0.8


  ss_dat_1 = calc_ss(dat_1)
  ss_dat_2 = calc_ss(dat_2)
  ms_dat_1 = calc_ms(dat_1)
  ms_dat_2 = calc_ms(dat_2)
  ss_dat_1$ssb
  ss_dat_1$ssw
  ss_dat_1$sst


  plot_sst(dat_1)
  plot_ssw(dat_1)
  plot_ssb(dat_1)


}

