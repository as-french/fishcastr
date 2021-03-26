#' Cauchy curve
#'
#' This function describes a unimodal curve defined by y values as a function of
#' x and two constants. For example, y might define a proxy measure for
#' migration preparedness for marine endurance of diadromous fish migrating into
#' sea water following morphological transformation (e.g., silvering of European
#' eels). The Cauchy curve is heavy-tailed and is suitable for response
#' variables for which outliers occur at smaller and larger values of x than
#' expected under a Gaussian curve.
#'
#' @param x A numeric vector.
#' @param x0 A constant that defines the position of the curve peak.
#' @param gamma A constant that defines the kurtosis, spread and scale of the
#'   curve.
#' @return y A numeric vector.
#' @export
Cauchy_resp <- function(gamma,x0,x){
  y = ((1/pi)*(gamma/10))*(((gamma/10)^2)/((x - x0)^2 + (gamma/10)^2))
  y[y<=0] = 1e-7
  return(y)
}
