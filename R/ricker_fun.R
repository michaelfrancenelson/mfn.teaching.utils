

#' Ricker function as specified in McGarigal's notes
#'
#' @param x x-values
#' @param a scales the function
#' @param b scales the exponent
#'
#' @export

ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}










