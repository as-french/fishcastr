#' Air to water temperature model validation plot.
#'
#' @description This function returns a variety of validation plots and
#'   validation statistics for the air two water temperature model that builds
#'   on the three-parameter equation defined by A. Ducharne (2008). Ducharne's
#'   equation describes the statistical relationship between lagged rolling
#'   average air temperature and water temperature in streams (whereby lag
#'   period is a constant in days). We extended Ducharne's equation to a
#'   four-parameter model to account for stratification in lakes, whereby the
#'   lag period between air temperature and water temperature shortens during
#'   the summer owing to the shallower mixed layer depth during stratification.
#'   These validation plots are intended for use on out-of-sample air and water
#'   temperature data. See Piccolroaz et al., (2013) for further discussion of
#'   lake thermal dynamics in relation to meteorological forcing, and see and
#'   Calderó-Pascual et al., (2020) for description of Lough Feeagh,
#'   Burrishoole, Ireland example. A bias adjustment multiplication factor that
#'   varies with year day was included to reduce residual hysteresis present in
#'   the basic lagged air temperature model.
#'
#' @param Tw A numeric vector of water temperatures.
#' @param Ta A numeric vector of air temperatures.
#' @param Yday A numeric vector of year days (derivable from lubridate::yday).
#' @param dates A vector of corresponding dates.
#' @param mod_air_to_water_mle A fitted bbmle::mle2 object that models the
#'   relationship between air temperature and lake surface water temperature
#'   (load .rds model found in vignettes).
#' @return A matrix of validation figures and a list of summary statistics.
#' @references
#' Calderó-Pascual, M., de Eyto, E., Jennings, E., Dillane, M., Andersen, M. R.,
#' Kelly, S., Wilson, H. L., & McCarthy, V. (2020). Effects of Consecutive
#' Extreme Weather Events on a Temperate Dystrophic Lake: A Detailed Insight
#' into Physical, Chemical and Biological Responses. Water, 12(5), 1411.
#' https://doi.org/10.3390/w12051411
#'
#' Ducharne, A. (2008). Importance of stream temperature to climate change
#' impact on water quality. Hydrology and Earth System Sciences, 12(3), 797–810.
#'
#' Piccolroaz, S., Toffolon, M., & Majone, B. (2013). A simple lumped model to
#' convert air temperature into surface water temperature in lakes. Hydrology
#' and Earth System Sciences, 17(8), 3323–3338.
#' https://doi.org/10.5194/hess-17-3323-2013
#' @export
plot_air_to_water_validation <- function(Tw, Ta, Yday, dates, mod_air_to_water_mle){

  # --------------------------------------------------------------------------- #
  # define months and seasons for use in plotting
  months <- lubridate::month(dates)
  seasons <- ifelse(months %in% c(12,1,2),"black",
                                        ifelse(months %in% c(3,4,5),"red",
                                               ifelse(months %in% c(6,7,8),"green","blue")))

  season_col <- grDevices::col2rgb(col = seasons)
  season_col_alpha <- grDevices::rgb(
    red = season_col[1,],
    green = season_col[2,],
    blue = season_col[3,],
    alpha = 50,
    maxColorValue = 255
  )

  # --------------------------------------------------------------------------- #
  # produce predictions and PPI based confidence intervals ----
  vmat = MASS::mvrnorm(1000, mu = bbmle::coef(mod_air_to_water_mle),
                       Sigma = bbmle::vcov(mod_air_to_water_mle))
  dist = matrix(nrow = length(Ta), ncol = 1000)
  for (i in 1:1000) {
    dist[,i] =  air_to_water_model(Ta = Ta,
                                   Yday = Yday,
                                   ac = vmat[i, "ac"],
                                   b = vmat[i, "b"],
                                   B = vmat[i, "B"])
  }
  dist_quantiles <- cbind(Ta,
                          t(apply(dist, MARGIN = 1, FUN = stats::quantile,
                                  probs = c(0.025,0.50, 0.975),na.rm = TRUE)),
                          (apply(dist, MARGIN = 1, FUN = mean, na.rm = TRUE)))
  colnames(dist_quantiles) <- c("x","lwr_CI","median","upr_CI","mean")
  preds <- dist_quantiles[,"mean"]

  # --------------------------------------------------------------------------- #
  # Check number of NAs at start of preds based on rolling
  # average lag (this can be incorporated into a minimum warm-up period? For
  # Feeagh at least 40 days...)
  ##which(is.na(preds))
  # and check they are consecutive and start at 1
  ##min(which(is.na(preds))) == 1 # TRUE
  ##any(diff(which(is.na(preds))) != 1) # FALSE
  length_NA_start <- length(which(is.na(preds))) + 1 # 39

  # --------------------------------------------------------------------------- #
  # simulate one set of Gaussian random deviates using mean estimated from
  # mvrnorm estimated mean and the rnorm deviate generator to check plausibility
  # of magnitude of fitted/estimated standard deviation using bbmle.
  # In theory gamma errors might be more appropriate...
  sim_values_list <- lapply(1, function(z){
    set.seed(as.integer(123)+z)
    result <- stats::rnorm(n = length(preds[length_NA_start:length(preds)]),
                    mean = preds[length_NA_start:length(preds)],
                    sd = exp(bbmle::coef(mod_air_to_water_mle)[["sigma"]]))
    return(result)
  })
  sim <- do.call(cbind, sim_values_list)
  sim_NA <- c(rep(NA,times = (length_NA_start-1)),sim)

  # ----------------------------------------------------------------------------------------------- #
  # plots
  graphics::layout(matrix(c(1,1,1,1,1,1,2,3,4,2,3,5), 4, 3, byrow = TRUE))
  graphics::par(mar = c(4,4,1,1))
  graphics::plot(dates,
       sim_NA, col = NULL,
       type = "p", cex = 0.5, ylim = c(0,25),axes = FALSE,xlab = "time", ylab = "temperature")
  graphics::axis(2, tck = 0.02)
  dateseq = dates[lubridate::mday(dates) == 1]
  graphics::axis(1, at = dateseq, labels = format(dateseq, "%b"), tck = 0.02)

  # year seq for plot text placed at 22 deg C on y axis
  year_seq <- as.Date(paste0(c(unique(lubridate::year(dates))),"-01-01"))
  graphics::abline(v = year_seq, lty = 3)
  graphics::text(x = year_seq,labels = format(year_seq,"%Y"),
       rep(22,times = length(year_seq)),
       srt = 90,
       adj = c(0,2),
       xpd = NA)
  graphics::lines(dates, Tw, col = "black", lwd = 2)
  graphics::lines(dates, preds, col = "red", lwd = 2)

  # plot legend
  graphics::legend("topright", legend = c("observed","predicted"),
         col = c("black", "red"), lty = rep(1, times = 2),bty = 'n')

  # plot dynamic lag value to see if trough coincides with stratification period
  dynamic_lag <- function(Yday, ac, b){
    result <- as.vector(round(30 - ((ac) * exp(-1 * ((Yday - (b)) ^ 2) / (2 * ((400 / (ac)) ^ 2))))))
    return(result)
  }

  dy_lag_est <- dynamic_lag(Yday = Yday[1:366],
                            ac = bbmle::coef(mod_air_to_water_mle)["ac"],
                            b = bbmle::coef(mod_air_to_water_mle)["b"])

  # plot only first 366 days
  graphics::plot(dates[1:366],dy_lag_est, col = "grey", type = "l", ylab = "lag", xlab = "DOY")

  # plot predicted vs observed (should be approximately 1:1 with minimal hysteresis)
  graphics::plot(preds,Tw, cex = 1,col = season_col_alpha, xlab = "predicted", ylab = "observed")
  graphics::abline(b=1,a=0,lty=1,col="black", lwd = 2)
  graphics::legend("bottomright", legend = c("Winter","Spring","Summer", "Autumn"),
         col = c(1:4), pch = rep(1, times = 4), cex = 1,bty = 'n')

  # check goodness of fit stats ----
  PearsonsR <- stats::cor.test(preds,
                        Tw) # 0.9673769
  NSE <- hydroGOF::NSE(preds,
                       Tw)[[1]] # 0.9274653
  KGE <- hydroGOF::KGE(preds,
                       Tw)[[1]] # 0.8803185
  RMSE <- hydroGOF::rmse(preds,
                         Tw)[[1]] # 1.197935

  graphics::legend("topleft",
         legend = paste0("Pearson's R: ",
                         signif(PearsonsR[[4]],digits = 2),
                         "\nNSE: ",signif(NSE,digits = 2),
                         "\nKGE: ",signif(KGE,digits = 2),
                         "\nRMSE: ",signif(RMSE,digits = 2)),cex = 1,bty = 'n')

  # plot residuals histogram
  resids_temp <- Tw - preds
  graphics::hist(resids_temp, main = "")

  # plot residuals vs. time
  graphics::plot(Yday, resids_temp, cex = 0.5,col = "black", xlab = "YDay", ylab = "raw residual")
  graphics::abline(h = 0, col = "red", lty = 3, lwd = 2)

  # predictions and simulations data to return
  df <- data.frame("Tw_obs" = Tw,
                   "Ta_obs" = Ta,
                   "Yday" = Yday,
                   "dates" = dates,
                   "Tw_pred" = preds,
                   "Tw_sim" = sim_NA)

  return(list("data" = df,
              "PearsonsR" = PearsonsR,
              "NSE" = NSE,
              "KGE" = KGE,
              "rmse" = RMSE))
}
