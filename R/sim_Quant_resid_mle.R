#' Produce simulated residuals compatible with DHARMa package
#'
#' @description This function produces simulated residuals from a fitted
#'   bbmle::mle2 (Bolker & R Core Team 2017) model object compatible with DHARMa
#'   (Hartig 2019) functions (e.g., testDispersion).
#'
#' @importFrom gamlss.dist dNBII rNBII
#' @param fittedModel A bbmle::mle2 object.
#' @param n Number of simulations of mle2 object.
#' @param count_lab Name of response variable.
#' @param sim_seed A random seed.
#' @param fittedModel_name Default NULL. Experimental.
#' @param fittedModel_curve_name Character string currently "Cauchy",
#'   "exp_mod_Gauss", or "exp_mod_Gauss_inv".
#' @param newdata Default NULL. Experimental.
#' @param ... Other arguments to nested functions.
#' @return A createDHARMa simulated residual object.
#' @references
#' Hartig, F. (2019). DHARMa: Residual Diagnostics for Hierarchical (Multi-Level
#' / Mixed) Regression Models (R package version 0.2.2).
#' https://CRAN.R-project.org/package=DHARMa
#' Bolker, B. & R Core Team. (2017). bbmle: Tools for General Maximum Likelihood
#' Estimation (R package version 1.0.20).
#' https://CRAN.R-project.org/package=bbmle
#' @examples
#' \dontrun{
#' sim_Quant_resid_mle(fittedModel = mle2_mod,
#'                     n = 250,
#'                     count_lab = "ssmolt",
#'                     sim_seed = 123,
#'                     fittedModel_name = NULL,
#'                     newdata = NULL)
#' }
#'
#' @export
sim_Quant_resid_mle <- function(fittedModel,
                                n,
                                count_lab,
                                sim_seed,
                                fittedModel_name = NULL,
                                newdata = NULL,
                                fittedModel_curve_name,
                                ...){

#  dNBII <- gamlss.dist::dNBII
#  dLGP <- RMKdiscrete::dLGP
#  dnbinom <- stats::dnbinom

  form = fittedModel@formula
  fam = qdapRegex::ex_between(form, "~", "(")[[1]]

  sim <- simulate_mle(fittedModel = fittedModel,
                      n = n,
                      count_lab = count_lab,
                      sim_seed = sim_seed,
                      fittedModel_curve_name = fittedModel_curve_name)

  testData = fittedModel@data[[count_lab]]

  if(fam == "dLGP" & fittedModel_curve_name == "exp_mod_Gauss"){
    # ----------------------------------------------------------
    # obtain mean by simulation
#    suppressMessages(require(MASS, quietly = TRUE))
    vmat = MASS::mvrnorm(1000, mu = bbmle::coef(fittedModel), Sigma = bbmle::vcov(fittedModel))
    dist = matrix(nrow = length(fittedModel@data[[2]]), ncol = 1000)
    for (i in 1:1000) {
      dist[,i] =  exp_mod_Gaussian_resp(x = fittedModel@data[[2]],
                                        c = vmat[i, "c"],
                                        mu_exmg = vmat[i, "mu_exmg"],
                                        sigma_exmg = vmat[i, "sigma_exmg"],
                                        lamb = vmat[i, "lamb"])
    }

    dist_quantiles <- cbind(fittedModel@data[[2]],
                            t(apply(dist, MARGIN = 1, FUN = quantile, probs = c(0.025,0.50, 0.975))),
                            (apply(dist, MARGIN = 1, FUN = mean)))
    colnames(dist_quantiles) <- c("x","lwr_CI","median","upr_CI","mean")
    cond_mean <- dist_quantiles[,"mean"]

    DHARMaRes = DHARMa::createDHARMa(simulatedResponse = sim, observedResponse = testData,
                                     fittedPredictedResponse = cond_mean,integerResponse = T)
    return(DHARMaRes)
  }

  if(fam == "dLGP" & fittedModel_curve_name == "exp_mod_Gauss_inv"){
    # ----------------------------------------------------------
    # obtain mean by simulation
    #    suppressMessages(require(MASS, quietly = TRUE))
    vmat = MASS::mvrnorm(1000, mu = bbmle::coef(fittedModel), Sigma = bbmle::vcov(fittedModel))
    dist = matrix(nrow = length(fittedModel@data[[2]]), ncol = 1000)
    for (i in 1:1000) {
      dist[,i] =  exp_mod_Gaussian_resp_inv(x = fittedModel@data[[2]],
                                        c = vmat[i, "c"],
                                        mu_exmg = vmat[i, "mu_exmg"],
                                        sigma_exmg = vmat[i, "sigma_exmg"],
                                        lamb = vmat[i, "lamb"])
    }

    dist_quantiles <- cbind(fittedModel@data[[2]],
                            t(apply(dist, MARGIN = 1, FUN = quantile, probs = c(0.025,0.50, 0.975))),
                            (apply(dist, MARGIN = 1, FUN = mean)))
    colnames(dist_quantiles) <- c("x","lwr_CI","median","upr_CI","mean")
    cond_mean <- dist_quantiles[,"mean"]

    DHARMaRes = DHARMa::createDHARMa(simulatedResponse = sim, observedResponse = testData,
                                     fittedPredictedResponse = cond_mean,integerResponse = T)
    return(DHARMaRes)
  }

  if(fam == "dLGP" & fittedModel_curve_name == "Cauchy"){
    # ----------------------------------------------------------
    # obtain mean by simulation
    #    suppressMessages(require(MASS, quietly = TRUE))
    vmat = MASS::mvrnorm(1000, mu = bbmle::coef(fittedModel), Sigma = bbmle::vcov(fittedModel))
    dist = matrix(nrow = length(fittedModel@data[[2]]), ncol = 1000)
    for (i in 1:1000) {
      dist[,i] =  Cauchy_resp(x = fittedModel@data[[2]],
                                        gamma = vmat[i, "gamma"],
                                        x0 = vmat[i, "x0"])
    }

    dist_quantiles <- cbind(fittedModel@data[[2]],
                            t(apply(dist, MARGIN = 1, FUN = quantile, probs = c(0.025,0.50, 0.975))),
                            (apply(dist, MARGIN = 1, FUN = mean)))
    colnames(dist_quantiles) <- c("x","lwr_CI","median","upr_CI","mean")
    cond_mean <- dist_quantiles[,"mean"]

    DHARMaRes = DHARMa::createDHARMa(simulatedResponse = sim, observedResponse = testData,
                                     fittedPredictedResponse = cond_mean,integerResponse = T)
    return(DHARMaRes)
  }

  if(fam != "dLGP"){
    cond_mean <-  bbmle::predict(fittedModel)
    DHARMaRes = DHARMa::createDHARMa(simulatedResponse = sim, observedResponse = testData,
                                     fittedPredictedResponse = cond_mean,integerResponse = T)
    return(DHARMaRes)
  }


}
