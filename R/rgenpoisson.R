#' Generate random deviates from a generalised Poisson distribution (Legrangian
#' Poisson)
#'
#' @description This function generates random deviates when supplied with a
#'   vector of conditional means and dispersion parameter. It is important to
#'   note that the dispersion parameter returned by different models (e.g.,
#'   those implemented using glmmTMB), may have different definitions. For
#'   example, extracting dispersion parameter from genpois family models will
#'   yield "phi-squared" dispersion parameter, which must be square rooted
#'   before input into rgenpoisson (see simulate_glm function for example use
#'   and https://rdrr.io/cran/glmmTMB/man/sigma.glmmTMB.html for details).
#'
#' @param mu A vector.
#' @param disp_param A numeric, dispersion parameter.
#' @return A vector of random deviates.
#' @examples
#' set.seed(123)
#' plot(rgenpoisson(mu = fishcastr::exp_mod_Gaussian_resp(x = 1:365,
#'                                                   c = 60,
#'                                                   mu_exmg = 100,
#'                                                   sigma_exmg = 15,
#'                                                   lamb = 25),
#'             disp_param = 10))
#' @export
rgenpoisson <- function(mu, disp_param){
  old_state <- get_rand_state()
  on.exit(set_rand_state(old_state))

  disp_param_length = length(disp_param)

#  suppressMessages(require(RMKdiscrete))
  # note from (http://finzi.psych.upenn.edu/library/VGAM/html/genpoisson.html)
  # definition of theta, lamba and phi are same references as Consul 1989 and
  # 2006 used by dLGP.

  if(disp_param_length == 1){

  number_rows <- 1
  theta <- as.numeric(mu*(1 - (1 - sqrt(1/disp_param))))
  lambda <- (1 - sqrt(1/disp_param))

  #stratify theta by non-nas and nas then join up again after
  #theta_non_na <- theta[!is.na(theta)]
  #theta_na <- theta[is.na(theta)]
  result <- mapply(FUN = RMKdiscrete::rLGP,
                   n = number_rows,
                   theta = theta,
                   lambda = lambda,
                   SIMPLIFY = T)

  return(result)
  }

  if(disp_param_length != 1){

    # generate random deviates
    theta <- as.numeric(mu*(1 - (1 - sqrt(1/disp_param))))
    lambda <- (1 - sqrt(1/disp_param))
    result <- RMKdiscrete::rLGP(n = 1,theta = theta,lambda = lambda)

    return(result)
  }

}
