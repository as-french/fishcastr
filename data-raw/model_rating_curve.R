# ----------------------------------------------------------------------------------------------------------#
# RATING CURVE MODEL FITTING
# ----------------------------------------------------------------------------------------------------------#
# The data used for this rating curve fit were collected by Sean Kelly.

data_rating_curve_Feeagh <- as.data.frame(readr::read_csv(paste0(system.file("extdata", package = "fishcastr"),
                                                                 "/SK_Feeagh_rating_curve/data_rating_curve_feeagh.csv"),
                                                          col_types = readr::cols(date = readr::col_date(format = "%Y-%m-%d"),Feeagh_EPA_ht_m = readr::col_double())))
usethis::use_data(data_rating_curve_Feeagh, overwrite = TRUE)

plot(data_rating_curve_Feeagh$Feeagh_EPA_ht_m,data_rating_curve_Feeagh$Total_Q_m3_s,
     xlim = c(0,1.5),
     ylim = c(0.01,30),
     xlab = "water level (m)", ylab = "flow (m3 per s)")

# fit Rating curve by mle assuming gamma errors because Q values must be > 0.
# L-BFGS-B bounded to reduce warnings (all parameter estimates must be positive)
model_rating_curve_Feeagh = bbmle::mle2(Total_Q_m3_s~dgamma(shape = (c*Feeagh_EPA_ht_m^Beta)/st_dev,
                                                            scale=st_dev),
                                        start = list(c = 20,
                                                     Beta = 1.5,
                                                     st_dev = 0.1),
                                        method = "L-BFGS-B",
                                        lower = list(c = 0.1,
                                                     Beta = 1,
                                                     st_dev = 0.001),
                                        upper = list(c = 100,
                                                     Beta = 3,
                                                     st_dev = 0.5),
                                        control = list(maxit = 1000000000),
                                        data=data_rating_curve_Feeagh)

bbmle::summary(model_rating_curve_Feeagh)
# might need to load bbmle package for profile function to work
# require(bbmle)
bbmle::plot(bbmle::profile(model_rating_curve_Feeagh))
usethis::use_data(model_rating_curve_Feeagh, overwrite = TRUE)

rating_curve_function <- function(x,c, Beta){
  result = c*(x^Beta)
  return(result)
}

# plot curve, uncertainty in mean (from mvrnorm based parameter uncertainty, 95%
# CIs (i.e., confidence intervals calculated using the population prediction
# interval PPI method - see B Bolker's, Ecological Models and data with R book),
# observations, and simulation based 95% PIs (prediction intervals)
level_vals <- seq(from = 0.01, to = 1.6, by = 0.1) # set seq of lake levels
calculated_Q <- rating_curve_function(c = bbmle::coef(model_rating_curve_Feeagh)["c"],
                                      x = level_vals,
                                      Beta = bbmle::coef(model_rating_curve_Feeagh)["Beta"]) # estimate Q for range of lake levels

# add 95% CIs of mean by PPI method, and PIs from simulation rgamma adjusting
# for uncertainty in mean illustrated by CIs (note sd can be negative here and
# can throw errors, perhaps rmvgamma in future?)
set.seed(23456)
vmat = MASS::mvrnorm(1000,
                     mu = bbmle::coef(model_rating_curve_Feeagh),
                     Sigma = bbmle::vcov(model_rating_curve_Feeagh))
dist = matrix(nrow = length(level_vals), ncol = 1000)
vardist = matrix(nrow = length(level_vals), ncol = 1000)
for (i in 1:1000) {
  dist[,i] =  rating_curve_function(x = level_vals,
                                    c = vmat[i, "c"],
                                    Beta = vmat[i, "Beta"])
  set.seed(as.integer(123)+i)
  vardist[,i] <- rgamma(n = length(dist[,i]),
                        shape = (dist[,i])/(vmat[i, "st_dev"]),
                        scale = vmat[i, "st_dev"])
}

dist_quantiles <- cbind(level_vals,
                        t(apply(dist,
                                MARGIN = 1,
                                FUN = quantile, probs = c(0.025,0.50, 0.975))),
                        apply(dist, MARGIN = 1, FUN = mean))
colnames(dist_quantiles) <- c("wlevel_m","lwr_CI","median","upr_CI","mean")
#preds <- dist_quantiles[,"mean"] # mean expectation

# PIs (adjusts for uncertainty in point estimate for mean; i.e., takes into
# account model parameter uncertainty (shown by CIs); uncertainty illustrated by
# PIs is based on gamma conditional distribution AND uncertainty in the mean
# estimate)
pred_intervals <- cbind(level_vals,
                        t(apply(vardist,
                                MARGIN = 1,
                                FUN = quantile, probs = c(0.025,0.50, 0.975), na.rm = TRUE)),
                        apply(vardist, MARGIN = 1, FUN = mean))
colnames(pred_intervals) <- c("wlevel_m","lwr_CI","median","upr_CI","mean")

dirName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/SK_Feeagh_rating_curve/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

png(paste0(dirName,"Rating_curve.png"), height = 1500, width = 1500, res = 300)
par(mgp = c(2,1,0))
plot(level_vals,
     calculated_Q,
     type = "l",
     xlab = "water level (m)",
     ylab = expression(paste("Catchment discharge (","m"^"3", "/s)")),
     xlim = c(0,1.5),
     ylim = c(0,32),
     tck = 0.02)

# add observations
points(data_rating_curve_Feeagh$Feeagh_EPA_ht_m,
       data_rating_curve_Feeagh$Total_Q_m3_s)
lines(dist_quantiles[,"wlevel_m"],
      dist_quantiles[,"lwr_CI"], type = "l", lty = 3)
lines(dist_quantiles[,"wlevel_m"],
      dist_quantiles[,"upr_CI"], type = "l", lty = 3)
lines(pred_intervals[,"wlevel_m"],
      pred_intervals[,"lwr_CI"], type = "l", lty = 2)
lines(pred_intervals[,"wlevel_m"],
      pred_intervals[,"upr_CI"], type = "l", lty = 2)
invisible(dev.off())
