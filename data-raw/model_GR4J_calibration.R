# ----------------------------------------------------------------------------------------------------------#
# A method for calibrating and validating rainfall run-off model GR4J
# ----------------------------------------------------------------------------------------------------------#

# import and convert ERA5 grid to data.frame
era5_reanalysis_bcc <- fishcastr::convert_grid_to_dataframe(grid_obj = fishcastr::grid_ERA5_1979_2019_Jan_bc)[,-2]
names(era5_reanalysis_bcc)[which(names(era5_reanalysis_bcc) == "dates1")] <- "date"

# import discharge
Feeagh_disch_1976_2021 <- fishcastr::data_Feeagh_discharge
Feeagh_disch_1976_2021_corr <- fishcastr::data_Feeagh_discharge_corr

# bind discharge and climate data (knowing climate reanalysis is complete)
disch_met_complete <- Reduce(function(x,y) merge(x,y,by="date"),
                              list(Feeagh_disch_1976_2021_corr,
                                   era5_reanalysis_bcc))

# filter to only include longest stretch of non-NA discharge values for calibration
streak <- stats::na.contiguous(disch_met_complete$discharge_m3.s)
lwr_date <- disch_met_complete$date[attr(streak,which = "tsp")[1]] # "2009-05-27"
upr_date <- disch_met_complete$date[attr(streak,which = "tsp")[2]] # "2015-06-17"

# convert m3.s to mm per day
catch.area = 84.353 # data from https://gis.epa.ie/EPAMaps/Water
# flow attenuation from lake is 0.76 (days?)
# catchment average rainfall annually 2084.98754005
# 472mm Potential Evapotranspiration annually
inp <- disch_met_complete[,c("date","discharge_m3.s")]
inp$date <- as.POSIXct(inp$date, tz = 'UTC')
# convert flow from m^3/s to mm/day
inp$flow.mm.day <- (inp$discharge_m3.s *1000* (60*60*24)) / (catch.area *1000000)
# ----------------------------------------------------------------------------------------------------------#


# ----------------------------------------------------------------------------------------------------------#
# set calibration and validation periods
# ----------------------------------------------------------------------------------------------------------#

# need warm up of at least 1 year
# single calibration period
# warm-up 2009-05-27 - 2011-05-27
# run 2011-05-28 - 2015-06-17
# validate against all data pre 2009-05-27
# final two years of validation are warm-up for calibration

# set cal and val periods
inp.cal <- inp[(inp[,1] >= '2011-05-28' & inp[,1] <= '2015-06-17'),]
inp.warmup <- inp[(inp[,1] >= '2009-05-27' & inp[,1] <= '2011-05-27'),]
inp.val <- inp[(inp[,1] >= '1981-01-01' & inp[,1] <= '2011-05-26'),]
inp.val.warmup <- inp[(inp[,1] >= '1979-01-01' & inp[,1] <= '1980-12-31'),]

# extract met forcing data
met <- disch_met_complete[,c("date","pr","petH")]
met$date <- as.POSIXct(met$date, tz = 'UTC')
met$pr_mm <- met$pr
met$petEv <- met$petH
# calculate potential evapotranspiration from temperature data
met$month <- lubridate::month(met$date)
met$yday <- lubridate::yday(met$date)

# check for realistic monthly sums for location
plot(met$yday[366:750], met$petEv[366:750])
testdf_pe <- as.data.frame(met)
testdf_pe$year <- lubridate::year(testdf_pe$date)
tapply(testdf_pe$petEv[testdf_pe$year == 2008],
       INDEX = testdf_pe$month[testdf_pe$year == 2008],
       sum) # monthly
tapply(testdf_pe$petEv,
       INDEX = testdf_pe$year,
       sum) # annual

# ----------------------------------------------------------------------------------------------------------#
# estimate mean annual flow per year to check if realistic vs mean annual precipitation and mean annual PET
# does the water balance look reasonable?
# ----------------------------------------------------------------------------------------------------------#

plot(inp$date[366:750], inp$flow.mm.day[366:750], type = "l")
testdf <- as.data.frame(inp)
testdf$year <- lubridate::year(testdf$date)
#tapply(testdf$flow.mm.day,INDEX = testdf$year, sum)
mean(tapply(testdf$flow.mm.day,
            INDEX = testdf$year, sum, na.rm = TRUE)) # 1329.958mm Q per year

# mean annual precipitation (ERA5)
met$year <- lubridate::year(met$date)
mean(tapply(met$pr,
            INDEX = met$year,
            sum)) # 1651.996mm pr per year
tab_q_pr_pe <- as.data.frame(cbind(tapply(testdf$flow.mm.day,
                                          INDEX = testdf$year,
                                          sum,na.rm = TRUE),
                                   tapply(met$pr,
                                          INDEX = met$year,
                                          sum,na.rm = TRUE),
                                   tapply(testdf_pe$petEv,
                                          INDEX = testdf_pe$year,
                                          sum,na.rm = TRUE))) # flow and pr by year
colnames(tab_q_pr_pe)<- c("Q","pr","PE")
# underestimating rainfall, owing to elevation? Add correction multiplication factor for mean catchment elevation
tab_q_pr_pe # pr - Q should approximately equal PE and it does
summ_tab <- apply(tab_q_pr_pe[1:nrow(tab_q_pr_pe)-1,],
                  MARGIN = 2,
                  FUN = mean)
summ_tab
# estimated run-off per year
# Pr/Q
summ_tab[["Q"]]/summ_tab[["pr"]] # 0.8746454 not implausible
# ----------------------------------------------------------------------------------------------------------#

inputs <- airGR::CreateInputsModel(airGR::RunModel_GR4J,
                                   DatesR = met$date,
                                   Precip = met$pr_mm,
                                   PotEvap = met$petEv)
## run period selection
Ind_Run <- which(met[,1] >= inp.cal[1,1] & met[,1] <= inp.cal[nrow(inp.cal),1])
warm_up <- which(met[,1] >= inp.warmup[1,1] & met[,1] < inp.cal[1,1])

opts <- airGR::CreateRunOptions(airGR::RunModel_GR4J,
                                InputsModel = inputs,
                                IndPeriod_Run = Ind_Run,
                                IndPeriod_WarmUp = warm_up)
#Param = c(208.512710,2.324490,58.556963,2.764264,0.5)
#Param = c(350,0,40,0.5) # From https://wiki.ewater.org.au/display/SD41/GR4J+-+SRG


# # choose starting set of parameters based on similar hydroclimate as defined in Harrigan 2018 et al., sup materials
Harrigan_params <- as.data.frame(read.csv(paste0(getwd(),
                                                        "/inst/extdata/Supplementary_Table_S1_Harrigan2018.csv"),stringsAsFactors = TRUE,check.names = FALSE))

Harrigan_WS_NWENW <- Harrigan_params[Harrigan_params$`UK Hydroclimate Region` %in% c("WS","NWENW"),
                                     c("GR4J X1 (mm)","GR4J X2 (mm d-1)","GR4J X3 (mm)",
                                       "GR4J X4 (d)","Station name","Easting","Northing",
                                       "Area (km2)","Median elevation (masl)","Mean P (mm yr-1)",
                                       "Mean PET (mm yr-1)")]
X1p <- Harrigan_WS_NWENW$`GR4J X1 (mm)`
X2p <- Harrigan_WS_NWENW$`GR4J X2 (mm d-1)`
X3p <- Harrigan_WS_NWENW$`GR4J X3 (mm)`
X4p <- Harrigan_WS_NWENW$`GR4J X4 (d)`

par(mfrow = c(2,2))
h <- hist(X1p, ylim = c(0,40), xlim = c(0,max(X1p)))
xfit <- seq(min(X1p),max(X1p),length=40)
yfit <- dlnorm(xfit,meanlog =mean(log(X1p)),sdlog = sd(log(X1p)))
yfit <- yfit*diff(h$mids[1:2])*length(X1p)
lines(xfit, yfit, col="blue", lwd=2)
h <-hist(X2p)
xfit <- seq(min(X2p),max(X2p),length=40)
yfit <- dnorm(xfit,mean=mean((X2p)),sd = sd((X2p)))
yfit <- yfit*diff(h$mids[1:2])*length(X2p)
lines(xfit, yfit, col="blue", lwd=2)
h <-hist(X3p, ylim = c(0,70), xlim = c(0,max(X3p)))
xfit <- seq(min(X3p),max(X3p),length=40)
yfit <- dlnorm(xfit,mean=mean(log(X3p)),sdlog = sd(log(X3p)))
yfit <- yfit*diff(h$mids[1:2])*length(X3p)
lines(xfit, yfit, col="blue", lwd=2)
h <-hist(X4p, xlim = c(0,max(X4p)))
xfit <- seq(min(X4p),max(X4p),length=40)
yfit <- dlnorm(xfit,mean=mean(log(X4p)),sdlog = sd(log(X4p)))
yfit <- yfit*diff(h$mids[1:2])*length(X4p)
lines(xfit, yfit, col="blue", lwd=2)

# set constraints for parameter search as alternative method?
#Param = cbind(quantile(X1p,probs = c(0.05,0.975)),
#              quantile(X2p,probs = c(0.05,0.975)),
#              quantile(X3p,probs = c(0.05,0.975)),
#              quantile(X4p,probs = c(0.05,0.975)))

# set starting param values
Param_median = c(median(X1p),
                 median(X2p),
                 median(X3p),
                 median(X4p))

# run model
mod1 = airGR::RunModel_GR4J(InputsModel = inputs,
                            RunOptions = opts,
                            Param = Param_median)

# check model fit before calibration
hydroGOF::NSE(mod1$Qsim, inp.cal$flow.mm.day) # 0.479781 un-calibrated
hydroGOF::KGE(mod1$Qsim, inp.cal$flow.mm.day) # 0.6221787 un-calibrated

# Calibration
err_crit <- airGR::CreateInputsCrit(airGR::ErrorCrit_KGE2,
                                    InputsModel = inputs,
                                    RunOptions = opts,
                                    Obs = inp.cal$flow.mm.day,
                                    transfo = "boxcox") # add more weight to errors produced at mid range values of Q (as in Harrigan et al., 2018), or use box cox to add more weight to low flow errors
#"sqrt","log","inv","sort","boxcox"

CalibOptions <- airGR::CreateCalibOptions(FUN_MOD = airGR::RunModel_GR4J,
                                          FUN_CALIB = airGR::Calibration_Michel)

OutputsCalib <- airGR::Calibration_Michel(InputsModel = inputs,
                                          RunOptions = opts,
                                          InputsCrit = err_crit,
                                          CalibOptions = CalibOptions,
                                          FUN_MOD = airGR::RunModel_GR4J)

Param <- OutputsCalib$ParamFinalR
Param # CP boxcox KGE calib 189.815172   2.124977  37.826944   2.120363

# run model with calibrated parameters
OutputsModel <- airGR::RunModel_GR4J(InputsModel = inputs,
                                     RunOptions = opts,
                                     Param = Param)

#Burr_mod_2002_2018 <- data.frame("date" = seq(from = as.Date("2002-01-01"), to = as.Date("2017-12-31"), by = 1), "Qsim" = OutputsModel$Qsim)

#write.csv(Burr_mod_2002_2018,file = "C:\\Users\\afrench.INSTITUTE\\OneDrive - Marine Institute\\FeeaghFlow_GR4J\\Burr_flow_mod_Gr4J.csv")

hydroGOF::NSE(OutputsModel$Qsim, inp.cal$flow.mm.day) # 0.6457835
hydroGOF::KGE(OutputsModel$Qsim, inp.cal$flow.mm.day) # 0.8227817
hydroGOF::rmse(OutputsModel$Qsim, inp.cal$flow.mm.day) # 2.234569

# export validation plots for calibratino period
dirName <- paste0(getwd(),"/vignettes/vignette_figures/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = TRUE, mode = "0777")
dirName <- paste0(getwd(),"/vignettes/vignette_figures", "/Fig_Burr_GR4J_calib", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = TRUE, mode = "0777")

png(paste0(dirName,"Burr_GR4J_ERA5_Calib_results.png"), height = 3000, width = 6500, res =300)
#dev.new()
plot(OutputsModel, Qobs = inp.cal$flow.mm.day)
invisible(dev.off())

OutputsCrit <- airGR::ErrorCrit_NSE(InputsCrit = err_crit,
                                    OutputsModel = OutputsModel) # CP 0.6959
OutputsCrit <- airGR::ErrorCrit_KGE(InputsCrit = err_crit,
                                    OutputsModel = OutputsModel) # CP 0.8500

#Validation data
## run period selection
Ind_Run <- which(met[,1] >= inp.val[1,1] & met[,1] <= inp.val[nrow(inp.val),1])
warm_up <- 1:(Ind_Run[1]-1)
opts <- airGR::CreateRunOptions(airGR::RunModel_GR4J,
                                InputsModel = inputs,
                                IndPeriod_Run = Ind_Run,
                                IndPeriod_WarmUp = warm_up)

OutputsModel2 <- airGR::RunModel_GR4J(InputsModel = inputs,
                                      RunOptions = opts,
                                      Param = Param)

hydroGOF::NSE(OutputsModel2$Qsim, inp.val$flow.mm.day) # 0.5068104 CP
hydroGOF::KGE(OutputsModel2$Qsim, inp.val$flow.mm.day) # 0.7091826 CP
hydroGOF::rmse(OutputsModel2$Qsim, inp.val$flow.mm.day) # 2.458732 CP

png(paste0(dirName,"Burr_GR4J_ERA5_Val_results.png"), height = 2500, width = 6500, res =300)
plot(OutputsModel2,
     Qobs = inp.val$flow.mm.day)
invisible(dev.off())

GR4J_Burr_params_ERA5_bcc <- data.frame(prod.stor.cap_mm = Param[1],
                                        inter.exch.coeff_mm.d = Param[2],
                                        rout.stor.cap_mm = Param[3],
                                        unit.hyd.time.cons_d = Param[4])

# export calibrated parameter set to use to prepare env data for fish models in vignette.
usethis::use_data(GR4J_Burr_params_ERA5_bcc, overwrite = TRUE)
