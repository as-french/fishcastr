#' Fit an exponentially modified Gaussian response curve to count data
#'
#' @description This function produces a series of plots that can be used to
#'   evaluate the fit of an exponentially modified Gaussian response curve to
#'   unimodal count data. Specifically, plots are intended for checking the
#'   consistency of curve fit (i.e., inter-decadal variability in a proxy for
#'   attainment of migration preparedness) among multiple decades (e.g., fitting
#'   curves for pooled count data from windows 1990 - 2000, 2011 - 2010, 20011 -
#'   2020). Residual plots use the functionality of the DHARMa package (Hartig
#'   2019). The intended implementation of the exponentially modified Gaussian
#'   curve as a proxy for physiological preparedness for migration of diadromous
#'   fishes assumes that physiological preparedness for sea entry increases with
#'   degree days that reflect an increase in physiological condition for marine
#'   endurance (e.g., gill Na+/K+-ATPase activity in salmonids; Sykes et al.,
#'   2009; Zydlewski et al., 2005). See /data-raw/model_physio_expmG_salmon.R
#'   for usage example.
#'
#' @importFrom gamlss.dist dNBII rNBII
#' @param data_list A list of data frames of ecological counts and environmental
#'   variables.
#' @param species_count_lab Column header for y variable.
#' @param x_variable_lab Column header for x variable.
#' @param error_distribution_name One of: "negative_binomial",
#'   "generalised_Poisson", "quasi_Poisson".
#' @param species_name A character string label for plot legend.
#' @param file_path_plot Character string file path for main figure.
#' @param file_path_resid_fitted_plot Character string file path for fitted vs
#'   residual figure.
#' @param file_path_dispersion_zeroinf_acf_plot Character string file path for
#'   dispersion and acf diagnostic figures.
#' @param file_path_coef_profile_plot Character string file path for bbmle::mle2
#'   model coefficient profile plots
#' @param start_values A list of starting values supplied to L-BFGS-B method.
#' @param lower_lims A list of lower limit values supplied to L-BFGS-B method.
#' @param xlims Concatenated values.
#' @param ylims Concatenated values.
#' @param ... Other arguments to nested functions.
#' @return A bbmle::mle2 model trained on all years of data_list pooled together
#'   (i.e., 5th data_list element)
#' @references
#' Zydlewski, G. B., Haro, A., & McCormick, S. D. (2005). Evidence for
#' cumulative temperature as an initiating and terminating factor in downstream
#' migratory behaviour of Atlantic salmon (Salmo salar) smolts. Canadian Journal
#' of Fisheries and Aquatic Sciences, 62(1), 68–78.
#' https://doi.org/10.1139/f04-179
#' Sykes, G. E., Johnson, C. J., & Shrimpton, J. M. (2009). Temperature and Flow
#' Effects on Migration Timing of Chinook Salmon Smolts. Transactions of the
#' American Fisheries Society, 138(6), 1252–1265.
#' https://doi.org/10.1577/T08-180.1
#' Hartig, F. (2019). DHARMa: Residual Diagnostics for Hierarchical (Multi-Level
#' / Mixed) Regression Models (R package version 0.2.2).
#' https://CRAN.R-project.org/package=DHARMa
#' @export
physio_model_evaluation_exp_mod_Gauss <- function(data_list,
                                                  species_count_lab,
                                                  x_variable_lab,
                                                  error_distribution_name,
                                                  species_name,
                                                  file_path_plot,
                                                  file_path_resid_fitted_plot,
                                                  file_path_dispersion_zeroinf_acf_plot,
                                                  file_path_coef_profile_plot,
                                                  start_values,
                                                  lower_lims,
                                                  ylims,
                                                  xlims,
                                                  ...){

#  suppressMessages(require(gamlss.dist))
#  suppressMessages(require(RMKdiscrete))
#  dLGP <- RMKdiscrete::dLGP
#  sLGP <- RMKdiscrete::sLGP
#  dNBII <- gamlss.dist::dNBII
#  sNBII <- fishcastr::sNBII
#  dnbinom <- stats::dnbinom

  cols_list <- list("purple","red","green3","blue", "black")

  col_list <- list(col2rgb("purple"),col2rgb("red"),col2rgb("green3"),col2rgb("blue"), col2rgb("black"))
  col_alpha_list <- list(rgb(red = col_list[[1]][1], green = col_list[[1]][2], blue = col_list[[1]][3],
                             alpha = 30, maxColorValue = 255),
                         rgb(red = col_list[[2]][1], green = col_list[[2]][2], blue = col_list[[2]][3],
                             alpha = 30, maxColorValue = 255),
                         rgb(red = col_list[[3]][1], green = col_list[[3]][2], blue = col_list[[3]][3],
                             alpha = 30, maxColorValue = 255),
                         rgb(red = col_list[[4]][1], green = col_list[[4]][2], blue = col_list[[4]][3],
                             alpha = 30, maxColorValue = 255),
                         rgb(red = col_list[[5]][1], green = col_list[[5]][2], blue = col_list[[5]][3],
                             alpha = 30, maxColorValue = 255))

  col_alpha_list_poly <- list(rgb(red = col_list[[1]][1], green = col_list[[1]][2], blue = col_list[[1]][3],
                             alpha = 70, maxColorValue = 255),
                         rgb(red = col_list[[2]][1], green = col_list[[2]][2], blue = col_list[[2]][3],
                             alpha = 70, maxColorValue = 255),
                         rgb(red = col_list[[3]][1], green = col_list[[3]][2], blue = col_list[[3]][3],
                             alpha = 70, maxColorValue = 255),
                         rgb(red = col_list[[4]][1], green = col_list[[4]][2], blue = col_list[[4]][3],
                             alpha = 70, maxColorValue = 255),
                         rgb(red = col_list[[5]][1], green = col_list[[5]][2], blue = col_list[[5]][3],
                             alpha = 70, maxColorValue = 255))

  grDevices::png(filename = file_path_plot, width=1200, height=2000, res=300)
  par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,4,1,1), mgp = c(2,0.5,0))
  plot(data_list[[1]][[x_variable_lab]][data_list[[1]][[species_count_lab]] != 0],
       data_list[[1]][[species_count_lab]][data_list[[1]][[species_count_lab]] != 0],
       xlab = "Photoperiod weighted degree days",
       ylab = "Scaled counts",
       type = "p",
       col = NULL,
       xlim = xlims,
       ylim = ylims,
       cex = 0.5,
       tck = 0.02,
       yaxt = 'n')
  axis(side = 2,
       labels = c(1800,1200,800,400,200,100,0,100,200,400,800,1200,1800),
       at = c(-1800,-1200,-800,-400,-200,-100,0,100,200,400,800,1200,1800))

#x = 1
  physio_model_eval <- plyr::llply(.data = 1:(length(data_list)), .fun = function(x){

    # variance = mu + (mu^2)*sigma_nb,
    # where size = 1/sigma_nb
    if(error_distribution_name == "negative_binomial"){
      formula_physio = formula(paste0(species_count_lab,"~dnbinom(mu = (100*c)*(((1/lamb)/2)*(exp(((1/lamb)/2)*(2*mu_exmg + (1/lamb)*(sigma_exmg^2)-2*",x_variable_lab,")))*pracma::erfc((mu_exmg+(1/lamb)*(sigma_exmg^2) - ",x_variable_lab,")/(sqrt(2)*sigma_exmg))),size=1/sigma_nb)"))
      starting_values = start_values
      lower_limits = lower_lims
    }

    # note that dLGP is parameterised by theta and lambda, not the mean
    # mu as for dnbinom etc.,.
    # For dLGP,
    # variance = mu*phi.
    # mu =  theta/(1-lambda), where lambda is related to the dispersion parameter phi
    # by the following equation:
    # lambda = 1 - sqrt(1/phi), so
    # theta = mu*(1-lambda), and
    # theta = mu*(1-(1-sqrt(1/phi))), where mu is the curve response "y" value, i.e., the mean.
    if(error_distribution_name == "generalised_Poisson"){
      formula_physio = formula(paste0(species_count_lab,"~dLGP(theta = (1 - (1-sqrt(1/phi)))*((100*c)*(((1/lamb)/2)*(exp(((1/lamb)/2)*(2*mu_exmg + (1/lamb)*(sigma_exmg^2)-2*",x_variable_lab,")))*pracma::erfc((mu_exmg+(1/lamb)*(sigma_exmg^2) - ",x_variable_lab,")/(sqrt(2)*sigma_exmg)))), lambda=1-sqrt(1/phi))"))
      starting_values = start_values
      lower_limits = lower_lims
    }

    # quasi poisson is also known as a negative binomial type II, hence reuse of sigma_nb notation, but notice reciprocal in to size argument for dnbinom...
    # variance = mu*(1 + sigma_nb)
    # sigma = sigma_nb
    if(error_distribution_name == "quasi_Poisson"){
      formula_physio = formula(paste0(species_count_lab,"~dNBII(mu = (100*c)*(((1/lamb)/2)*(exp(((1/lamb)/2)*(2*mu_exmg + (1/lamb)*(sigma_exmg^2)-2*",x_variable_lab,")))*pracma::erfc((mu_exmg+(1/lamb)*(sigma_exmg^2) - ",x_variable_lab,")/(sqrt(2)*sigma_exmg))),sigma=sigma_nb)"))
      starting_values = start_values
      lower_limits = lower_lims
    }

    mod_physio_expmG = bbmle::mle2(formula_physio,
                            start = starting_values,
                            method = "L-BFGS-B",
                            lower=lower_limits,
                            control = list(maxit = 1000000000),
                            data=data_list[[x]])

    #plot(bbmle::profile(mod_physio_expmG))

    # replace elements in mle2 object required for predict...
    mod_physio_expmG@call$minuslogl <- formula_physio
    mod_physio_expmG@call$start <- starting_values
    mod_physio_expmG@call$lower <- lower_limits

    mod_physio_expmG@call.orig$minuslogl <-formula_physio
    mod_physio_expmG@call.orig$start <-starting_values
    mod_physio_expmG@call.orig$lower <-lower_limits

    # 95% population prediction intervals
    vmat = MASS::mvrnorm(1000, mu = bbmle::coef(mod_physio_expmG), Sigma = bbmle::vcov(mod_physio_expmG))

    dist = matrix(nrow = 3500, ncol = 1000)
    for (i in 1:1000) {
      dist[,i] =  exp_mod_Gaussian_resp(x = seq(from = 1, to = 3500, by = 1),
                                        c = vmat[i, "c"],
                                        mu_exmg = vmat[i, "mu_exmg"],
                                        sigma_exmg = vmat[i, "sigma_exmg"],
                                        lamb = vmat[i, "lamb"])
    }

    dist_quantiles <- cbind(as.vector(seq(from = 1, to = 3500, by = 1)),
                            t(apply(dist, MARGIN = 1, FUN = quantile, probs = c(0.025,0.50, 0.975))),
                            apply(dist, MARGIN = 1, FUN = mean))
    colnames(dist_quantiles) <- c(x_variable_lab,"lwr_CI","median","upr_CI","mean")

    # add data points for stratified years, uniquely coloured...
    if(x %in% 1:(length(data_list)-1)){
      points(data_list[[x]][[x_variable_lab]][data_list[[x]][[species_count_lab]] != 0], data_list[[x]][[species_count_lab]][data_list[[x]][[species_count_lab]] != 0], col = col_alpha_list[[x]], cex = 0.6)


      # add polygons of population prediction intervals of means (which take into account model parameter coefficient uncertainties)
      polygon(x = c(dist_quantiles[,x_variable_lab], rev(dist_quantiles[,x_variable_lab])),
              y = c(dist_quantiles[,"lwr_CI"],
                    rev(dist_quantiles[,"upr_CI"])),
              col=col_alpha_list_poly[[x]],
              lty = 0)

      # add mvrnorm simulated means
      #lines(dist_quantiles[,x_variable_lab], dist_quantiles[,"mean"], lty = 1, col = cols_list[[x]], lwd = 2)
    }
    return(list(mod_physio_expmG, dist_quantiles))
  })

  # add lines
  for(i in 1:(length(physio_model_eval)-1)){
  lines(physio_model_eval[[i]][[2]][,x_variable_lab], physio_model_eval[[i]][[2]][,"mean"], col = cols_list[[i]], lwd = 1)
  }

  # note that currently, 12-10-2020, time series must be stratified into four windows and a complete series
  # get labels from list

  legend("topright", legend = names(data_list)[1:(length(data_list)-1)],
         pch = rep(1, times = (length(data_list)-1)),
         pt.cex = rep(1, times = (length(data_list)-1)),
         bty = "n",
         col = as.vector(do.call(c,cols_list)[c(1:(length(data_list)-1))]),
         lty = rep(1, times = (length(data_list)-1)),
         pt.lwd = rep(1, times = (length(data_list)-1)),
         lwd = rep(2, times = (length(data_list)-1)),
         cex = 0.80, inset = c(0,0.1), xpd = NA, title = species_name)

  # --------------------------------------------------------------------------------------------------
  # SIMULATE AND PLOT MIRRORING OBS DATA FOR VISUAL VALIDATION TO ILLUSTRATE "CAPTURED DYNAMICS" FOR MODEL TRAINED ON ALL YEARS

  sims_physio <- simulate_mle(fittedModel = physio_model_eval[[length(data_list)]][[1]],
                              n = 1,
                              count_lab = species_count_lab,
                              sim_seed = 123,
                              fittedModel_curve_name = "exp_mod_Gauss")
  preds_physio <- physio_model_eval[[length(data_list)]][[2]][,"mean"]

  col_sims <- col2rgb("black")
  col_sims_alpha <- rgb(red = col_sims[1], green = col_sims[2], blue = col_sims[3], alpha = 30, maxColorValue = 255)

  # plot simulated data from fitted model
  points(physio_model_eval[[length(data_list)]][[1]]@data[[x_variable_lab]],-1*(sims_physio), cex = 0.6, col = col_sims_alpha)

  # add polygons of population prediction intervals of means (which take into account model parameter coefficient uncertainties)
  col_sims <- col2rgb("black")
  col_sims_alpha <- rgb(red = col_sims[1], green = col_sims[2], blue = col_sims[3], alpha = 70, maxColorValue = 255)
  polygon(x = c(physio_model_eval[[length(data_list)]][[2]][,x_variable_lab], rev(physio_model_eval[[length(data_list)]][[2]][,x_variable_lab])),
          y = -1*c(physio_model_eval[[length(data_list)]][[2]][,"lwr_CI"],
                   rev(physio_model_eval[[length(data_list)]][[2]][,"upr_CI"])),
          col=col_sims_alpha,
          lty = 0)

  # plot simulated mean from model using mvrnorm
  lines(physio_model_eval[[length(data_list)]][[2]][,x_variable_lab], -1*physio_model_eval[[length(data_list)]][[2]][,"mean"], col = "black", lwd = 1)
  legend("bottomright", legend = c(paste0("Simulated","\n",names(data_list)[length(data_list)]), paste0(names(data_list)[length(data_list)])), pch = c(1,1), pt.cex = c(1,1), bty = "n", col = c("black","black"), lty = c(0,1), pt.lwd = c(1, 1),lwd = c(2, 2), cex = 0.80, inset = c(0,0.1), xpd = NA)

  invisible(dev.off())
  # --------------------------------------------------------------------------------------------------
  # MODEL VALIDATION (NOT GREAT, some underdispersion, BUT OK, IMPROVE...., dgenpois...) ---------------------
  dharma_resids_expmG <- sim_Quant_resid_mle(fittedModel = physio_model_eval[[length(data_list)]][[1]],
                                             n = 250,
                                             count_lab = species_count_lab,
                                             sim_seed = 123,
                                             fittedModel_curve_name = "exp_mod_Gauss")

  png(filename = file_path_resid_fitted_plot, width=2100, height=1200, res=300)
  par(mfrow = c(1,1), oma = c(0,0,0,0), mar = c(4,3.8,3,0))
  plot(dharma_resids_expmG, quantreg = TRUE, smoothScatter = FALSE)
  invisible(dev.off())

  png(filename = file_path_dispersion_zeroinf_acf_plot, width=1200, height=2100, res=300)
  par(mfrow = c(3,1), oma = c(0,0,0,0), mar = c(4,4,5,4))
  DHARMa::testDispersion(dharma_resids_expmG)
  DHARMa::testZeroInflation(dharma_resids_expmG)
  acf(dharma_resids_expmG$scaledResiduals)
  invisible(dev.off())

  # plot model coef profiles to check parameter space has been exhausted
  png(filename = file_path_coef_profile_plot, width=2100, height=2100, res=300)
  par(mfrow = c(2,3), oma = c(0,0,0,0), mar = c(4,4,5,4))
  bbmle::plot(bbmle::profile(physio_model_eval[[length(data_list)]][[1]]))
  invisible(dev.off())

  # return model trained on all years of time series
#  detach("package:gamlss.dist", character.only = TRUE)
#  detach("package:RMKdiscrete", character.only = TRUE)

  return(list(mod_physio_expmG = physio_model_eval[[length(data_list)]][[1]]))
}
