# ----------------------------------------------------------------------------------------------- #
# A script for deriving plausible parameter estimates for a 4-parameter
# statistical air to water temperature model based on: A Ducharne, 2008.
# ----------------------------------------------------------------------------------------------- #
# The following parameter derivation relates specifically to Lough Feeagh
# outflow water temperature calibrated using Lough Feeagh surface 2m temperature
# recorded by Automatic Water Quality Monitoring Station (AWQMS) from 2004 to
# 2019. Air temperatures are Bias corrected ERA5 reanalysis available in the
# fishcastr package. Additional validation of the model can be carried
# out using 1960 to 2019 Mill Race temperature recordings at the outflow of
# Lough, but these are based on a variety of recorders and unreliable in places.
# ----------------------------------------------------------------------------------------------- #
# An automatic calibration routine is desirable, but for now (18-03-2021), we
# are estimating a parameter set based on hypothesised time lags between air
# temperature and water temperature. The lag is shortest during the the
# stratified period of the lake, owing to a shallower mixed layer depth. For
# streams, the lag period may be constant. Some residual hysteresis
# remains in the Lough Feeagh example.
# ----------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------- #
# LOAD DATA FOR CALIBRATION ----
# ----------------------------------------------------------------------------------------------- #
# load MR water temp
data_MR_temp <- fishcastr::data_MR_temp
colnames(data_MR_temp)<- c("date","Water_Temp_MR")
# load ERA5 BC air Temp
grid_ERA5_1979_2019_Jan_bc <- fishcastr::grid_ERA5_1979_2019_Jan_bc
# convert grid reanalysis to two column table: date, variable
data_ERA5_1979_2019_Jan_bc <- fishcastr::convert_grid_to_dataframe(grid_obj = grid_ERA5_1979_2019_Jan_bc)[,-2]
names(data_ERA5_1979_2019_Jan_bc)[which(names(data_ERA5_1979_2019_Jan_bc) == "dates1")] <- "date"
data_air_temp <- data_ERA5_1979_2019_Jan_bc[,c("date", "tas")]
rm(data_ERA5_1979_2019_Jan_bc)
rm(grid_ERA5_1979_2019_Jan_bc)
gc()

# Load Feeagh 2m depth water temperature
data_Feeagh_temp_2004_2019 <- fishcastr::data_Feeagh2m_temp

# ----------------------------------------------------------------------------------------------- #
# MERGE TEMPERATURE DATA ----
# ----------------------------------------------------------------------------------------------- #
data_air_water_merge <- merge(data_air_temp,
                              data_MR_temp, by = "date", all = TRUE)

# define additional columns used to illustrate validation plots to look for bias by season etc., ----
data_air_water_merge$month <- lubridate::month(data_air_water_merge$date)
data_air_water_merge$year <- lubridate::year(data_air_water_merge$date)
data_air_water_merge$season <- ifelse(data_air_water_merge$month %in% c(12,1,2),"black",
                                      ifelse(data_air_water_merge$month %in% c(3,4,5),"red",
                                             ifelse(data_air_water_merge$month %in% c(6,7,8),"green","blue")))
data_air_water_merge$day <- lubridate::yday(data_air_water_merge$date)

season_col <- col2rgb(col = data_air_water_merge$season)
data_air_water_merge$season_col_alpha <- rgb(
  red = season_col[1,],
  green = season_col[2,],
  blue = season_col[3,],
  alpha = 50,
  maxColorValue = 255
)

data_air_water_merge_Feeagh <- merge(data_air_water_merge,
                                     data_Feeagh_temp_2004_2019,
                                     by = "date")

# ----------------------------------------------------------------------------------------------- #
# define calibration dataset (lough feeagh and ERA5 2004 - 2012)
cal_dates <- c(as.Date("2004-01-01"),as.Date("2014-12-31"))
cal_dataset <-
  data_air_water_merge_Feeagh[data.table::between(data_air_water_merge_Feeagh$date,
                                                  lower = cal_dates[1],
                                                  upper = cal_dates[2]),
                              c("date","Water_Temp_2m","tas","day","season_col_alpha") ]
names(cal_dataset) <- c("date","Tw","Ta","day","season_col_alpha")

# ----------------------------------------------------------------------------------------------- #
# define validation dataset (lough feeagh and ERA5 2013 - 2019)
val_dates_LF <- c(as.Date("2015-01-01"),as.Date("2019-01-31"))
val_dataset_LF <-
  data_air_water_merge_Feeagh[data.table::between(data_air_water_merge_Feeagh$date,
                                                  lower = val_dates_LF[1],
                                                  upper = val_dates_LF[2]),
                              c("date","Water_Temp_2m","tas","day","season_col_alpha") ]
names(val_dataset_LF) <- c("date","Tw","Ta","day","season_col_alpha")

# ----------------------------------------------------------------------------------------------- #
# define validation dataset (Mill Race and ERA5 1979 - 2004 - paper chart)
val_dates_MR1 <- c(as.Date("1979-01-01"),as.Date("2004-12-31"))
val_dataset_MR1 <-
  data_air_water_merge[data.table::between(data_air_water_merge$date,
                                           lower = val_dates_MR1[1],
                                           upper = val_dates_MR1[2]),
                       c("date","Water_Temp_MR","tas","day","season_col_alpha") ]
names(val_dataset_MR1) <- c("date","Tw","Ta","day","season_col_alpha")

# ----------------------------------------------------------------------------------------------- #
# define validation dataset (Mill Race and ERA5 2004 - 2009 - tidbit)
val_dates_MR2 <- c(as.Date("2005-01-01"),as.Date("2009-12-31"))
val_dataset_MR2 <-
  data_air_water_merge[data.table::between(data_air_water_merge$date,
                                           lower = val_dates_MR2[1],
                                           upper = val_dates_MR2[2]),
                       c("date","Water_Temp_MR","tas","day","season_col_alpha") ]
names(val_dataset_MR2) <- c("date","Tw","Ta","day","season_col_alpha")

# ----------------------------------------------------------------------------------------------- #
# define validation dataset (Mill Race and ERA5 2010 - 2019 - orpheus)
val_dates_MR3 <- c(as.Date("2010-01-01"),as.Date("2019-01-31"))
val_dataset_MR3 <-
  data_air_water_merge[data.table::between(data_air_water_merge$date,
                                           lower = val_dates_MR3[1],
                                           upper = val_dates_MR3[2]),
                       c("date","Water_Temp_MR","tas","day","season_col_alpha") ]
names(val_dataset_MR3) <- c("date","Tw","Ta","day","season_col_alpha")

# ----------------------------------------------------------------------------------------------- #
# PLOT THE RAW DATA
# ----------------------------------------------------------------------------------------------- #
plot(cal_dataset$date[1:1000], cal_dataset$Tw[1:1000], type = "l")
lines(cal_dataset$date[1:1000], cal_dataset$Ta[1:1000], col = c("red"))

# plot moving average for 10 days
plot(cal_dataset$date[1:1000], cal_dataset$Tw[1:1000], type = "l")
# 40 day moving average
lines(cal_dataset$date[1:1000], data.table::frollmean(x = cal_dataset$Ta[1:1000],
                                              n = 40,
                                              align = "right",
                                              fill = NA,
                                              adaptive = FALSE), col = c("red"))
# 10 day moving average
lines(cal_dataset$date[1:1000], data.table::frollmean(x = cal_dataset$Ta[1:1000],
                                              n = 10,
                                              align = "right",
                                              fill = NA,
                                              adaptive = FALSE), col = c("blue"))
# 30 day moving average
lines(cal_dataset$date[1:1000], data.table::frollmean(x = cal_dataset$Ta[1:1000],
                                                      n = 30,
                                                      align = "right",
                                                      fill = NA,
                                                      adaptive = FALSE), col = c("green"))

# ----------------------------------------------------------------------------------------------- #
# Use bbmle::mle2 likelihood to fine tune parameter estimates AND derive vcov
# matrix for simulation based validation ----
# ----------------------------------------------------------------------------------------------- #

# plot daily mean data in one plot to guage parameter starting values
mean_daily_Tw <- tapply(X = data_air_water_merge_Feeagh$Water_Temp_2m, data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)
# simple daily mean
mean_daily_Ta <- tapply(X = data_air_water_merge_Feeagh$tas, data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)
# add rolling 10 day right aligned mean
rollmean_Ta_10 <- data.table::frollmean(x = data_air_water_merge_Feeagh$tas,
                      n = rep(10,times = length(data_air_water_merge_Feeagh$tas)),
                      align = "right",
                      fill = NA,
                      adaptive = TRUE)
rollmean_Ta_10_daily <- tapply(X = rollmean_Ta_10, data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)
# add rolling 20 day right aligned mean
rollmean_Ta_20 <- data.table::frollmean(x = data_air_water_merge_Feeagh$tas,
                                        n = rep(20,times = length(data_air_water_merge_Feeagh$tas)),
                                        align = "right",
                                        fill = NA,
                                        adaptive = TRUE)
rollmean_Ta_20_daily <- tapply(X = rollmean_Ta_20, data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)
# add rolling 30 day right aligned mean
rollmean_Ta_30 <- data.table::frollmean(x = data_air_water_merge_Feeagh$tas,
                                        n = rep(30,times = length(data_air_water_merge_Feeagh$tas)),
                                        align = "right",
                                        fill = NA,
                                        adaptive = TRUE)
rollmean_Ta_30_daily <- tapply(X = rollmean_Ta_30, data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)

plot(1:length(mean_daily_Tw), mean_daily_Tw, type = "l", col = "blue", lwd = 2)
abline(v = c(seq(from = 0, to = 370, by = 10)), lty = 3, col = "grey")
abline(h = c(seq(from = 0, to = 20, by = 1)), lty = 3, col = "grey")
#lines(1:length(mean_daily_Ta), mean_daily_Ta, type = "l", col = "red")
lines(1:length(rollmean_Ta_10_daily), rollmean_Ta_10_daily, type = "l", col = "lightgrey", lwd = 2)
lines(1:length(rollmean_Ta_20_daily), rollmean_Ta_20_daily, type = "l", col = "darkgrey", lwd = 2)
lines(1:length(rollmean_Ta_30_daily), rollmean_Ta_30_daily, type = "l", col = "black", lwd = 2)

# the above plot indicates that the dynamic lagged air temperature has potential
# for estimating surface water temperature. Here, the lag maximum between year
# days 1 and 100 - the lag shortens to a minimum on day 160 before increasing
# again to maximum by day 260. This suggests an inverted Gaussian function is
# suitable.

# in the first instance we can plot a model with just dynamic lag:
# set plausible starting values
# A is gradient of y = Ax + B line, where x is lagged air temperature, which we
# assume is 1.
# x is a function of ac, b and year day, which defines the change of lag with
# day of the year (specific to lake)
# ac is the difference in lag between minimum lag periods late spring and early
# winter and max lags autumn and early spring (and we assume lag in winter is 30
# days here)
# b is the location of the peak in the Gaussian function, which should be
# approximately day 160
# B is intercept of y = Ax + B
# we could add the maximum lag-time 30 as an additional parameter, but this may
# lead to model convergence problems. we could also add a parameter to define
# the width of the function curve - here we have set the width to scale with the
# maximum difference in lags observed throughout the year.
theta <- c("ac" = 25, "b" = 170, "B" = 0,"sigma" = 0.1)
year_days <- 1:366
estims <- as.vector(round(30 - ((theta[1])*exp(-1*((year_days - (theta[2]))^2)/(2*((400/(theta[1]))^2))))))
# plot plausible lag time vs year day
plot(1:366, estims, ylim = c(0,30), type = "l")
abline(v = c(seq(from = 0, to = 370, by = 10)), lty = 3, col = "grey")
abline(h = c(seq(from = 0, to = 30, by = 1)), lty = 3, col = "grey")

# in this case, we would predict water temperature as follows
mean_pred <- (1) * (data.table::frollmean(x = data_air_water_merge_Feeagh$tas,
                                                 n = as.vector(round(30 - ((theta[1])*exp(-1*((data_air_water_merge_Feeagh$day - (theta[2]))^2)/(2*((400/(theta[1]))^2)))))),
                                                 align = "right",
                                                 fill = NA,
                                                 adaptive = TRUE)) + (theta[3])

plot(1:length(mean_daily_Tw), mean_daily_Tw, type = "l", col = "blue", lwd = 2, xlim = c(1,365))
abline(v = c(seq(from = 0, to = 370, by = 10)), lty = 3, col = "grey")
abline(h = c(seq(from = 0, to = 20, by = 1)), lty = 3, col = "grey")

# prediction from dynamic lag model
mean_pred_daily <- tapply(X = mean_pred, data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)
lines(1:length(mean_pred_daily), mean_pred_daily, type = "l", col = "red", lwd = 2)

# from the above plot, we can see that using dynamic lag alone results in
# residual hysteresis between year days 1 and 100 (over-estimate) and year
# days 160 and 260 (under-estimate). To correct for hysteresis, we can use using a
# multiplication factor that changes with year day...

# basic sine wave based correction
# wave_params <- c("magn" = 0.05, "phi" = 0.95*pi)
# test <- 1 + wave_params[1]*cos(((2*pi)/365.25)*(year_days) + wave_params[2])
# plot(1:length(test),test, type = "l", ylim = c(0.8,1.2))
# abline(h = 1, lty = 3)

# sine wave with changing frequency to adjust for asymmetric (with year day) residual hysteresis
wave_params <- c("magn" = 0.05, "phi" = 1.25*pi)
test <- 0.985 + wave_params[1]*cos(4*pi*(2^(-1*(year_days/365.25))) + wave_params[2])
plot(1:length(test),test, type = "l", ylim = c(0.8,1.2))
abline(h = 1, lty = 3)
c(test[1],test[length(test)]) # check consistency of start and end year days
mean(test) # on average bias shift of x0.9878571

# crude check for residual hysteresis
mean_daily_Tw <- tapply(X = data_air_water_merge_Feeagh$Water_Temp_2m,
                        data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)
upper_95_daily_Tw <- tapply(X = data_air_water_merge_Feeagh$Water_Temp_2m,
                            data_air_water_merge_Feeagh$day, FUN = quantile, probs = c(0.975), na.rm = TRUE)
lower_95_daily_Tw <- tapply(X = data_air_water_merge_Feeagh$Water_Temp_2m,
                            data_air_water_merge_Feeagh$day, FUN = quantile, probs = c(0.025), na.rm = TRUE)

mean_pred_daily.scale <- test*mean_pred_daily
plot(1:length(mean_daily_Tw), mean_daily_Tw, type = "l", col = "blue", lwd = 2, ylim = c(min(lower_95_daily_Tw),max(upper_95_daily_Tw)))
lines(1:length(upper_95_daily_Tw), upper_95_daily_Tw, type = "l", col = "blue", lwd = 1)
lines(1:length(lower_95_daily_Tw), lower_95_daily_Tw, type = "l", col = "blue", lwd = 1)
abline(v = c(seq(from = 0, to = 370, by = 10)), lty = 3, col = "grey")
abline(h = c(seq(from = 0, to = 20, by = 1)), lty = 3, col = "grey")
lines(1:length(mean_pred_daily.scale), mean_pred_daily.scale, type = "l", col = "red", lwd = 2)

mean_pred.hyst_corr_mean <- tapply(X = mean_pred.hyst_corr,
                                   data_air_water_merge_Feeagh$day, FUN = mean, na.rm = TRUE)

lines(1:length(mean_pred.hyst_corr_mean), mean_pred.hyst_corr_mean, col = "red", lwd = 2)

# individual day check
multip_factor <- function(ydays, magn, phi){
  result <- 0.985 + wave_params[1]*cos(4*pi*(2^(-1*(ydays/365.25))) + wave_params[2])
  return(result)
}
# assume intercept is zero and that dynamic A is sufficient to adjust for any residual hysteresis

# in this case, we would predict water temperature as follows
mean_pred.hyst_corr <- (multip_factor(ydays = data_air_water_merge_Feeagh$day,
                                      magn = 0.05,
                                      phi = 1.25*pi))*((data.table::frollmean(x = data_air_water_merge_Feeagh$tas,
                                                 n = as.vector(round(30 - ((theta[1])*exp(-1*((data_air_water_merge_Feeagh$day - (theta[2]))^2)/(2*((400/(theta[1]))^2)))))),
                                                 align = "right",
                                                 fill = NA,
                                                 adaptive = TRUE))) + theta[3]
#plot(1000:2000, data_air_water_merge_Feeagh$Water_Temp_2m[1000:2000], type = "l")
#lines(1000:2000, mean_pred.hyst_corr[1000:2000], col = "red")

# define negative log likelihood to be minimised (4 free parameters)
dnormNLL = function(theta) {

  mean_pred <- (multip_factor(ydays = cal_dataset$day,
                               magn = 0.05,
                               phi = 1.25*pi))*(((1)*data.table::frollmean(x = cal_dataset$Ta,
                                                      n = as.vector(round(30 - ((theta[1])*exp(-1*((cal_dataset$day - (1*theta[2]))^2)/(2*((400/(theta[1]))^2)))))),
                                                      align = "right",
                                                      fill = NA,
                                                      adaptive = TRUE))) + theta[3]

  -sum(dnorm(x = cal_dataset$Tw, mean = mean_pred, sd = (exp(theta[4])), log = TRUE),na.rm = TRUE)
}

# set parameter names to be passed with NLL to mle2 ----
bbmle::parnames(dnormNLL) <- c("ac", "b", "B", "sigma")

# fit model to gauge rough starting params ----
mtest1 = bbmle::mle2(dnormNLL,
                    start = c(ac = 20, b = 150,sigma = 0.5, B = 0),
                    method = "Nelder-Mead",
                    control = list(maxit = 1000000000),
                    skip.hessian = FALSE)
bbmle::coef(mtest1) # is ac parameter implausible as it exceeds 30?

# confine param ranges to plausible values
mtest = bbmle::mle2(dnormNLL,
                    start = c(ac = 15, b = 150,sigma = 0.5, B = 0),
                    lower = c(ac = 10, b = 140,sigma = -2, B = -1),
                    upper = c(ac = 25, b = 200,sigma = 2, B = 1),
                    method = "L-BFGS-B",
                    control = list(maxit = 1000000000,
                                   parscale = abs(bbmle::coef(mtest1))))
bbmle::coef(mtest) # ac now plausible?

model_air_to_water_Feeagh <- mtest
usethis::use_data(model_air_to_water_Feeagh, overwrite = TRUE)

# ----------------------------------------------------------------------------------------------- #
# plot validation

dirName <- paste0(getwd(),"/vignettes/vignette_figures", "/Fig_Burr_air_water_cal", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = TRUE, mode = "0777")

#dev.new()
# calibration data Feeagh
png(paste0(getwd(),
           "/vignettes/vignette_figures/Fig_Burr_air_water_cal/air_water_cal_Fe.png"),
    height = 3000, width = 4000, res =300)
cal_stats_Fe <- plot_air_to_water_validation(Tw = cal_dataset$Tw,
                                             Ta = cal_dataset$Ta,
                                             Yday = cal_dataset$day,
                                             dates = cal_dataset$date,
                                             mod_air_to_water_mle = mtest)
invisible(dev.off())

# validation data Feeagh
png(paste0(getwd(),
           "/vignettes/vignette_figures/Fig_Burr_air_water_cal/air_water_val_Fe.png"),
    height = 3000, width = 4000, res =300)
val_stats_Fe <- fishcastr::plot_air_to_water_validation(Tw = val_dataset_LF$Tw,
                                             Ta = val_dataset_LF$Ta,
                                             Yday = val_dataset_LF$day,
                                             dates = val_dataset_LF$date,
                                             mod_air_to_water_mle = mtest)
invisible(dev.off())

# validation data Mill Race (paper chart)
png(paste0(getwd(),
           "/vignettes/vignette_figures/Fig_Burr_air_water_cal/air_water_val_MRpap.png"),
    height = 3000, width = 4000, res =300)
val_stats_MR1 <- fishcastr::plot_air_to_water_validation(Tw = val_dataset_MR1$Tw,
                                              Ta = val_dataset_MR1$Ta,
                                              Yday = val_dataset_MR1$day,
                                              dates = val_dataset_MR1$date,
                                              mod_air_to_water_mle = mtest)
invisible(dev.off())

# validation data Mill Race (tidbit)
png(paste0(getwd(),
           "/vignettes/vignette_figures/Fig_Burr_air_water_cal/air_water_val_MRtid.png"),
    height = 3000, width = 4000, res =300)
val_stats_MR2 <- fishcastr::plot_air_to_water_validation(Tw = val_dataset_MR2$Tw,
                                              Ta = val_dataset_MR2$Ta,
                                              Yday = val_dataset_MR2$day,
                                              dates = val_dataset_MR2$date,
                                              mod_air_to_water_mle = mtest)
invisible(dev.off())

# validation data Mill Race (orpheus)
png(paste0(getwd(),
           "/vignettes/vignette_figures/Fig_Burr_air_water_cal/air_water_val_MRorp.png"),
    height = 3000, width = 4000, res =300)
val_stats_MR3 <- fishcastr::plot_air_to_water_validation(Tw = val_dataset_MR3$Tw,
                                              Ta = val_dataset_MR3$Ta,
                                              Yday = val_dataset_MR3$day,
                                              dates = val_dataset_MR3$date,
                                              mod_air_to_water_mle = mtest)
invisible(dev.off())

# export parameter estimates as data in package
air_to_water_Feeagh_params_ERA5_bcc <- data.frame("A" = 1,
                                           "ac" = bbmle::coef(model_air_to_water_Feeagh)["ac"],
                                           "b" = bbmle::coef(model_air_to_water_Feeagh)["b"],
                                           "B" = bbmle::coef(model_air_to_water_Feeagh)["B"])

# export calibrated parameter set to use to prepare env data for fish models in vignette.
usethis::use_data(air_to_water_Feeagh_params_ERA5_bcc, overwrite = TRUE)
