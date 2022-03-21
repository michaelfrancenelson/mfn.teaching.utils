#' Plot a single-predictor linear model highlighting relevant components
#'
#' @param pch_pt = 21
#' @param cex_pt = 3.1
#' @param pt_col = adjustcolor("steelblue", 0.3)
#' @param pt_border = 1
#' @param plot_step whether to plot the step representing the rise/run components of the slope
#' @param step_col colour of the slope step
#' @param step_lwd width of the step line
#' @param step_min relative x-position of the start of the step
#' @param step_max relative x-position of the end of the step'
#' @param plot_h1 Whether to plot the regression line
#' @param h1_col color for the regression line
#' @param h1_lty line type for regression line
#' @param h1_lwd width for the regression line
#' @param plot_h0 whether to plot the null hypothesis line
#' @param h0_col color for the null hypothesis line
#' @param h0_lwd line width for the null hypothesis line
#' @param h0_lty line type for the null hypothesis line
#' @param th overall plot theme
#'
#' @export
#'

plot_one_way_model = function(
  sim_dat,
  pch_pt = 21, cex_pt = 3.1,
  pt_col = adjustcolor("steelblue", 0.3),
  pt_border = 1,
  plot_step = TRUE,
  step_col = "red",
  step_lwd = 0.8,
  step_min = 0.3,
  step_max = 0.7,
  # ab_lwd = 1,
  # int_cex = 3.1,
  plot_h1 = TRUE,
  h1_col = "darkblue",
  h1_lty = 1,
  h1_lwd = 1,
  plot_h0 = TRUE,
  h0_col = "darkblue",
  h0_lwd = 1,
  h0_lty = 2,
  th = theme_minimal()

)
{
  if (FALSE)
  {
    require(mfn.teaching.utils)
    n_sim_dat = 15
    slope_sim_dat = 1
    intercept_sim_dat = 10
    sigma_sim_dat = 1.1
    sigma_sim_dat2 = 1.1
    x_min = 0
    x_max = 10
    xlm = xlim(c(x_min, x_max))
    ylm = ylim(c(6, 21))

    sim_dat =
      build_one_way_data(
        n_sim_dat,
        x_name = "X",
        x_lim = c(x_min, x_max),
        b1 = slope_sim_dat,
        b0 = intercept_sim_dat,
        error_sd = sigma_sim_dat,
        seed = 5)
    sim_dat1 =
      build_one_way_data(
        n_sim_dat,
        x_lim = c(x_min, x_max),
        b1 = slope_sim_dat,
        b0 = intercept_sim_dat,
        error_sd = 3,
        seed = 5)

    plot_one_way_model(sim_dat, plot_step = F, plot_h0 = F)
    plot_one_way_model(sim_dat, plot_h1 = F, plot_step = F, plot_h0 = F)
    plot_one_way_model(sim_dat, plot_h1 = F, plot_step = T, plot_h0 = F)
    plot_one_way_model(sim_dat, plot_h1 = T, plot_step = T, plot_h0 = T)

    pch_pt = 21
    cex_pt = 3.1
    pt_col = adjustcolor("steelblue", 0.3)
    pt_border = 1
    plot_step = TRUE
    step_col = "red"
    step_lwd = 0.8
    step_min = 0.3
    step_max = 0.7
    ab_lwd = 1
    # int_cex = 3.1
    plot_h1 = TRUE
    h1_col = "darkblue"
    h1_lty = 1
    h1_lwd = 1
    plot_h0 = TRUE
    h0_col = "darkblue"
    h0_lwd = 1
    h0_lty = 2
    th = theme_minimal()


    }

  # Plot components
  dat = sim_dat$sim_data
  fit = sim_dat$sim_fit
  names_tmp = names(dat)
  names(dat) = c("x", "y")

  gg_fun = ggplot(dat, aes(x, y))
  gg_pt = geom_point(pch = pch_pt, size = cex_pt, fill = pt_col, col = pt_border)

  gg_h1 = NULL
  if(plot_h1)
    gg_h1 =
    geom_abline(
      intercept = fit$coefficients[1],
      slope     = fit$coefficients[2],
      lwd = h1_lwd, col = h1_col)

  gg_h0 = NULL
  if(plot_h0)
    gg_h0 = geom_abline(
      intercept = mean(dat$y),
      slope     = 0,
      col = h0_col, lwd = h0_lwd, lty = h0_lty)

  gg_step = NULL
  if (plot_step)
  {
    step_min = 0.3
    step_max = 0.7

    x1 = diff(range(dat$x)) * step_min + min(dat$x)
    x2 = diff(range(dat$x)) * step_max + min(dat$x)

    Explanatory = c(x1, x2)
    newdata = data.frame(Explanatory = c(x1, x2))
    names(newdata) = names_tmp[1]
    df = data.frame(
      Explanatory,
      Response = predict(fit, newdata))
    gg_step = geom_step(
      aes(Explanatory, Response), data = df,
      col = step_col,
      lwd = step_lwd)
  }

  return(gg_fun + gg_pt + th + gg_h1 + gg_h0 + gg_step + xlab(names_tmp[1]) + ylab(names_tmp[2]))
}




