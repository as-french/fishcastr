#' Generate summary statistics from a negative binomial type II (quasi-Poisson)
#' distribution
#'
#' @description This function generates summary statistics when supplied with a
#'   vector of conditional means and dispersion parameter. Implemented for
#'   compatibility with bbmle::mle2 (Bolker & R Core Team 2017), which requires
#'   "s" functions associated with "d" functions. The corresponding discrete
#'   density (or probability mass) function, dNBII, of the negative binomial
#'   type II is contained in package gamlss.dist.
#'
#' @param mu A vector.
#' @param sigma A numeric, dispersion parameter.
#' @return A vector of random deviates.
#' @references
#' Bolker, B. & R Core Team. (2017). bbmle: Tools for General Maximum Likelihood
#' Estimation (R package version 1.0.20).
#' https://CRAN.R-project.org/package=bbmle
#' @examples
#' sNBII(mu = fishcastr::exp_mod_Gaussian_resp(x = 1:365,
#'                                             c = 10,
#'                                             mu_exmg = 100,
#'                                             sigma_exmg = 15,
#'                                             lamb = 25),
#'       sigma = 10)
#' @export
sNBII <- function(sigma,mu) {
  v <- mu*(1+sigma)
  list(title="Quasi-Poisson",
       mu=mu,
       sigma=sigma,
       mean= mu,
       median= gamlss.dist::qNBII(0.5,mu=mu,sigma),
       mode=NA,
       variance=v,
       sd=sqrt(v))
}
