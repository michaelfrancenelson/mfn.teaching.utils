#' Create a fitted on-way lm object with desired parameters
#'
#' Builds a one-way linear model that you can use for predicting new data values with desired properties.
#' @param n_training_pts number of points to simulate for the model residuals
#' @param x_name name of the predictor variable
#' @param y_name name of the response variable
#' @param b0 desired intercept parameter
#' @param b1 desired x1 slope parameter for x1
#' @param seed random seed for training data
#' @param desired standard deviation of the residuals
#'
#' @export
#'

build_one_way_model = function(
  n_training_pts = 20,
  x_name = "Explanatory",
  y_name = "Response",
  b0 = 1, b1 = 1,
  seed = NA,
  mod_sd = 1
)
{
  if (FALSE)
  {
    n_training_pts = 20
    x_name = "Explanatory"
    y_name = "Response"
    b0 = 1
    b1 = 1
    seed = NA
    mod_sd = 2
  }

  if (!is.na(seed)) set.seed(seed)

  lm_fmt = "%s ~ %s"
  lm_formula = formula(sprintf(lm_fmt, y_name, x_name))

  x1 = rnorm(n_training_pts)
  noise = rnorm(n_training_pts)
  noise = scale(noise)
  noise = mod_sd * noise

  # noise = rnorm(n_training_pts, sd = 0.001)
  # noise = rnorm(n_training_pts, sd = mod_sd)


  # sd(noise); mean(noise)

  y1 = b0 + b1 * x1 + noise
  training_dat = data.frame(x1, y1)
  names(training_dat) = c(x_name, y_name)
  fit1 = lm(lm_formula, data = training_dat)

  fit1$coefficients[1:2] = c(b0, b1)

  fit1$call$formula = formula(sprintf(lm_fmt, y_name, x_name))

  summary(fit1)
  return(fit1)
}



#' Parameterize a linear function from a point and slope
#'
#' @param x1_name name of the x1 variable
#' @param x2_name name of the x2 variable
#' @param b2 desired slope parameter for x2
#' @param b3 desired interaction parameter
#' @param interaction_term whether to include an interaciton term in the model
#'
#' @inheritParams build_one_way_model
#'
#' @export

build_two_way_model = function(
  n_training_pts = 20,
  x1_name = "x1",
  x2_name = "x2",
  y_name = "Response",
  b0 = 1, b1 = 1, b2 = 1, b3 = 0.5,
  interaction_term = TRUE,
  seed = NA,
  mod_sd = 1
)
{
  if (!is.na(seed)) set.seed(seed)

  lm_fmt = "%s ~ %s + %s"
  if (interaction_term) lm_fmt = "%s ~ %s * %s"

  lm_formula = formula(sprintf(lm_fmt, y_name, x1_name, x2_name))

  x1 = rnorm(n_training_pts)
  x2 = rnorm(n_training_pts)

  noise = rnorm(n_training_pts)
  noise = scale(noise)
  noise = mod_sd * noise

  y1 = b0 + b1 * x1 + b2 * x2 + noise

  if (interaction_term) y1 = y1 + b3 * (x2 * x1)

  training_dat = data.frame(x1, x2, y1)
  names(training_dat) = c(x1_name, x2_name, y_name)
  fit1 = lm(lm_formula, data = training_dat)

  if (interaction_term)  fit1$coefficients[1:4] = c(b0, b1, b2, b3)
  if (!interaction_term) fit1$coefficients[1:3] = c(b0, b1, b2)

  fit1$call$formula = formula(sprintf(lm_fmt, y_name, x1_name, x2_name))

  return(fit1)
}



#' Build simulated data with desired properties
#'
#' @inheritParams build_one_way_model
#'
#' @param x_lim
#' @param error_mean
#' @param error_sd
#'
#' @export
#'


build_one_way_data = function(
  n_obs = 20,
  n_training_pts = 20,
  x_name = "Explanatory",
  y_name = "Response",
  b0 = 1, b1 = 1,
  x_lim = c(0, 1),
  error_mean = 0.01, error_sd = 10.2,
  seed = NA,
  mod_sd = 1
)
{

  if (FALSE)
  {
    n_obs = 20
    n_training_pts = 20
    x_name = "Explanatory"
    y_name = "Response"
    b0 = 1
    b1 = 1
    x_lim = c(0, 1)
    error_mean = 0.01
    error_sd = 0.2
    seed = NA
  }

  fit1 = build_one_way_model(
    n_training_pts = n_training_pts,
    x_name = x_name, y_name = y_name,
    b0 = b0, b1 = b1, seed = seed, mod_sd = error_sd)

  new_dat = data.frame(
    x1 = sort(normalize(
      rnorm(n_obs),
      x_lim[1], x_lim[2])))
  names(new_dat) = x_name

  sim_dat = within(
    new_dat,
    {
      y = predict(
        fit1,
        newdata = new_dat) +
        rnorm(n_obs, mean = error_mean, sd = error_sd)
    })

  names(sim_dat) = c(x_name, y_name)
  sim_mod = lm(fit1$call$formula, data = sim_dat)

  return(list(
    sim_data = sim_dat,
    sim_fit = sim_mod,
    ref_fit = fit1
  ))
}


#' Build simulated data with desired properties
#'
#' @inheritParams build_one_way_data
#' @inheritParams build_two_way_model
#'
#' @param x1_lim = c(0, 10)
#' @param x2_lim = c(2000, 3000),
#' @param x1_mean = NULL
#' @param x2_mean = NULL,
#' @param x1_sd = NULL
#' @param x2_sd = NULL,
#'
#'

build_two_way_data = function(
  n_obs = 20,
  n_training_pts = 20,
  x1_name = "x1",
  x2_name = "x2",
  y_name = "Response",
  b0 = 1, b1 = 1, b2 = 1, b3 = 0.5,
  mod_sd = 1,
  x1_lim = c(0, 10),
  x2_lim = c(2000, 3000),
  x1_mean = NULL, x2_mean = NULL,
  x1_sd = NULL, x2_sd = NULL,
  interaction_term = TRUE,
  error_mean = 0.01, error_sd = 0.2,
  seed = NA,
  x1_unif = FALSE, x2_unif = FALSE
)
{

  if (FALSE)
  {
    n_obs = 20
    n_training_pts = 20
    x1_name = "x1"
    x2_name = "x2"
    y_name = "Response"
    b0 = 1
    b1 = 1
    b2 = 1
    b3 = 0.5
    x1_lim = c(0, 10)
    x2_lim = c(1, 11)
    interaction_term = TRUE
    error_mean = 0.01
    error_sd = 0.2
    seed = 1

    x1_unif = FALSE
    x2_unif = FALSE

    x1_lim = NULL


    x1_mean = 1
    x2_mean = 2.3

    x1_mean = NULL
    x2_mean = NULL
    x1_sd = 1.1
    x2_sd = 2.5

  }

  fit1 = build_two_way_model(
    n_training_pts = n_training_pts,
    x1_name = x1_name,
    x2_name = x2_name,
    y_name = y_name,
    b0 = b0, b1 = b1, b2 = b2, b3 = b3,
    interaction_term = interaction_term,
    seed = seed)


  unif_dat = function(n, lims) runif(n, lims[1], lims[2])
  norm_dat = function(n, mn, sd) rnorm(n, mn, sd)

  xdat = function(n, lims, mn, sd, x_unif)
  {
    # Use a uniform distribution if no mean is provided
    # and the uniform tag is TRUE
    if (is.null(mn) & (x_unif))
    {
      x1 = unif_dat(n, lims)
    } else if (!is.null(mn))
    {
      x1 = norm_dat(n, mn, sd)
    } else if (is.null(mn))
    {
      x1 = rnorm(n)
    }
    if (!is.null(lims))
    {
      x1 = normalize(x = x1, min = lims[1], max = lims[2])
    }

    return(x1)
  }



  x1 = xdat(n_obs, x1_lim, x1_mean, x1_sd, x1_unif)
  x2 = xdat(n_obs, x2_lim, x2_mean, x2_sd, x2_unif)

  new_dat = data.frame(x1, x2)

  names(new_dat) = c(x1_name, x2_name)

  sim_dat = within(
    new_dat,
    {
      y = predict(
        fit1,
        newdata = new_dat) +
        rnorm(n_obs, mean = error_mean, sd = error_sd)
    })

  names(sim_dat) = c(x1_name, x2_name, y_name)
  sim_mod = lm(fit1$call$formula, data = sim_dat)

  return(list(
    sim_data = sim_dat,
    sim_fit = sim_mod,
    ref_fit = fit1
  ))
}



