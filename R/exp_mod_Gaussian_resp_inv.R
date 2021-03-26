#' An (inverted) exponentially modified Gaussian curve.
#'
#' This function describes a unimodal curve defined by y values as a function of
#' x and four constants. For example, y might define a proxy measure for
#' physiological preparedness for marine endurance of diadromous fish migrating
#' into sea water following physiological transformation (e.g., silvering). The
#' (inverted) exponentially modified Gaussian is left skewed and is suitable for
#' response variables for which outliers occur at small values of x.
#'
#' @param x A numeric vector.
#' @param c A scaling constant.
#' @param mu_exmg A constant that defines the position of the curve peak.
#' @param sigma_exmg A constant that defines the kurtosis of the curve.
#' @param lamb A constant that defines the rate of decline of the left tail of
#'   the curve.
#' @return y A numeric vector.
#' @export
exp_mod_Gaussian_resp_inv <-function(x, c, mu_exmg, sigma_exmg, lamb){
  y = ((100*c)*(((1/lamb)/2)*(exp(((1/lamb)/2)*(-2*mu_exmg + (1/lamb)*(sigma_exmg^2)-2*(-x))))*VGAM::erfc((-mu_exmg+(1/lamb)*(sigma_exmg^2) - (-x))/(sqrt(2)*sigma_exmg))))
  return(y)
}
