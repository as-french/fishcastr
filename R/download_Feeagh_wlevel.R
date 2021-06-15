#' Download water level for Lough Feeagh, Burrishoole, Ireland and estimate
#' discharge
#'
#' @description A function to download and pre-process water level recordings
#'   for Lough Feeagh, Burrishoole, Co. Mayo, Ireland to produce estimates for
#'   catchment discharge. Execution of this function results in two .rds
#'   datasets stored in the extdata folder in the package directory:
#'   data_Feeagh_discharge and data_Feeagh_discharge_corr. Each contains rating
#'   curve derived daily mean discharge calculated from the daily mean water
#'   level (delineated at 00:00) that have been aggregated from ~15 minutely
#'   records from the EPA's gauge records 1976-03-03 to present; however, one of
#'   the files data_Feeagh_discharge_corr, contains a step adjustment to the raw
#'   EPA water level data, whereby 0.072 m has been added to all values
#'   pre-1996-08-04. The position of this step adjustment and its magnitude is
#'   calculated using the strucchange package. This function also fills any data
#'   gaps of 2 days or fewer with simple linear interpolation.
#'
#'   The rating curve data and model fit is contained in this package.
#'
#'   Note the raw dataset downloaded when this function is run is continually
#'   updated by the EPA, but most recent records have not been checked for
#'   quality; hence, this function retains only checked values. On 2021-04-26,
#'   most recent checked value was 2020-10-07. To retain reproducibility of the
#'   resultant datasets (and the step change), we subset each dataset at this
#'   date.
#'
#' @return A message indicating successful or unsuccessful download.
#' @source
#' \url{https://epawebapp.epa.ie/hydronet/#32070}
#' @examples
#' \dontrun{
#' download_Feeagh_wlevel()
#' }
#' @export
download_Feeagh_wlevel <- function(){

  # ----------------------------------------------------------------------------------------------------------#
  # DOWNLOAD AND UNZIP DATA FROM EPA WEBSITE ----
  # ----------------------------------------------------------------------------------------------------------#
  dirName <- paste0(system.file("extdata", package = "fishcastr"),
                    "/EPA_Feeagh/")
  dir.create(dirName, showWarnings = TRUE, mode = "0777")

  url = "https://epawebapp.epa.ie/Hydronet/output/internet/stations/CAS/32070/S/complete_15min.zip"
  downloader::download(url,
                       destfile=paste0(dirName, "complete_15min.zip"),
                       mode="wb")
  unzip(paste0(dirName, "complete_15min.zip"), exdir = paste0(dirName, "complete_15min"))

  # ----------------------------------------------------------------------------------------------------------#
  # AGGREGATE TO 15 INTERVALS AND EXTRACT DAILY MEANS (could also be used to
  # extract midnight values for example if using a sub daily hydrologic model;
  # i.e., not GR4J)
  # ----------------------------------------------------------------------------------------------------------#
  # import feeagh data removing first 7 lines and delineating by semi colon
  complete_15min <- read.table(paste0(system.file("extdata", package = "fishcastr"),
                                      "/EPA_Feeagh/complete_15min/complete_15min.csv"), sep=";", quote="\"",
                               stringsAsFactors = FALSE)

  # unique(complete_15min$V4) Levels:  Fair Good Unchecked
  # remove  Unchecked data
  complete_15min <- complete_15min[complete_15min$V4 != "Unchecked",]

  # remove column relative to Malin Head and QC column
  complete_15min <- complete_15min[,-c(3,4)]
  names(complete_15min) <- c("time", "wlevel")
  complete_15min$time <- as.POSIXct(complete_15min$time,tz = "UTC")

  # cut to to 24 hour intervals 00:00 to 00:00
  complete_15min$int_24h = cut(complete_15min$time, breaks="1 day")

  # aggregate intervals to 24 hour mean
  complete_daily_mean = aggregate(wlevel ~ int_24h, FUN=mean, na.rm = TRUE, data=complete_15min)
  complete_daily_mean$int_24h <- as.POSIXct(complete_daily_mean$int_24h,tz = "UTC")
  colnames(complete_daily_mean) <- c("date","wlevel")

  # create sequence of dates to fill na gaps
  date_seq <- data.frame("date" = seq(from = min(complete_daily_mean$date,
                                                 na.rm = TRUE),
                                      to = max(complete_daily_mean$date,
                                               na.rm = TRUE), by = "day"))

  # merge date seq
  feeDaily <- merge(date_seq, complete_daily_mean, by = "date", all.x = TRUE)
  #plot(feeDaily$date,feeDaily$wlevel, type = "l") # note step change during 1990s
  # adjust for step change using Sean Kelly's method by adding 0.065m to all values pre-1997-08-27 ----

  # ----------------------------------------------------------------------------------------------------------#
  # STEP CHANGE ADJUSTMENT
  # ----------------------------------------------------------------------------------------------------------#
  # raw ts plot:
  #plot(feeDaily$date, feeDaily$wlevel,type="l")
  #abline(h=0.2,lty=2)
  #abline(v=as.POSIXct("1995-09-23 00:00:00"),lty=2,col="red") #the shift is apparent after this date

  # logged levels for further clarity
  #plot(feeDaily$date, log(feeDaily$wlevel),type="l")
  #abline(h=-1.7,lty=2)
  #abline(v=as.POSIXct("1995-09-23 00:00:00"),lty=2,col="red") #the shift is apparent after this date

  # format data as time series for use with strucchange package breakpoints
  fee.ts <- ts(feeDaily$wlevel,
               start=c(1976,3,3), frequency = 365.25)

  fee.log.ts <- ts(log(feeDaily$wlevel),
                   start=c(1976,3,3), frequency = 365.25)

  # compute breakpoints
  fee.ts.breakpoints <- strucchange::breakpoints(formula = fee.log.ts ~ 1, breaks = 1)
  # what date?
  date_step_change <- feeDaily$date[fee.ts.breakpoints$breakpoints] # 1996-08-04 UTC

  # estimate mean before and after break point
  before_brk_mean <- mean(feeDaily$wlevel[1:fee.ts.breakpoints$breakpoints],na.rm = TRUE) # 0.378443
  after_brk_mean <- mean(feeDaily$wlevel[fee.ts.breakpoints$breakpoints:length(feeDaily$wlevel)],na.rm = TRUE) # 0.4504703
  # difference
  step_change_val <- after_brk_mean - before_brk_mean # 0.07202734 (very similar to SK's previous estimate (2018) of 0.065m)

  # add difference in means to pre-break period and round all wlevels before exporting
  feeDaily$wlevel_adj <- ifelse(feeDaily$date <= date_step_change,
                                feeDaily$wlevel + step_change_val,
                                feeDaily$wlevel)

  # #plot(feeDaily$date,feeDaily$wlevel, type = "l", col = "black")
  # #lines(feeDaily$date,feeDaily$wlevel_adj, col = "blue")
  # #abline(v = date_step_change, col = "red", lty = 3)
  #
  # # check for long term trends prior to and after adjustment
  # feeagh.stlplus <- stlplus::stlplus(x = fee.log.ts,t = feeDaily$date,s.window = "periodic")
  # #plot(feeagh.stlplus)
  # coef(lm(feeagh.stlplus$data$trend ~ c(1:length(feeagh.stlplus$data$trend))))
  # #stlplus::plot_trend(feeagh.stlplus)
  #
  # fee.ts.adj <- ts(log(feeDaily$wlevel_adj),
  #                  start=c(1976,3,3), frequency = 365.25)
  # feeagh.adj.stlplus <- stlplus::stlplus(x = fee.ts.adj,t = feeDaily$date,s.window = "periodic")
  # #plot(feeagh.adj.stlplus)
  # #coef(lm(feeagh.adj.stlplus$data$trend ~ c(1:length(feeagh.adj.stlplus$data$trend))))
  # #stlplus::plot_trend(feeagh.adj.stlplus)

  # satisfied adjustment is appropriate, now export
  dirName <- paste0(system.file("extdata", package = "fishcastr"),
                    "/EPA_Feeagh/Feeagh_adj/")
  dir.create(dirName, showWarnings = TRUE, mode = "0777")

  write.table(x = feeDaily, file = paste0(system.file("extdata", package = "fishcastr"),
                                      "/EPA_Feeagh/Feeagh_adj/complete_daily_mean_adjusted.csv"),
                                row.names = FALSE,col.names = TRUE,sep = ",")

  # ----------------------------------------------------------------------------------------------------------#
  # LOAD LAKE LEVEL DATA (with gaps raw from EPA)
  # ----------------------------------------------------------------------------------------------------------#
  Feeagh_wlevel_1976_2021_raw <- read.csv(paste0(system.file("extdata", package = "fishcastr"),
                                          "/EPA_Feeagh/Feeagh_adj/complete_daily_mean_adjusted.csv"),
                                          stringsAsFactors = FALSE)

  Feeagh_wlevel_1976_2021_raw$date = as.Date(Feeagh_wlevel_1976_2021_raw$date)

  # linear interpolate any gaps of shorter than 2 days
  Feeagh_wlevel_1976_2021_raw$wlevel.gp = zoo::na.approx(object = Feeagh_wlevel_1976_2021_raw$wlevel, maxgap = 2)

  # filter just post 1990s data (assuming the data underwent a step change during the 1990s)
  Feeagh_wlevel_1976_2021_post_1990s <- Feeagh_wlevel_1976_2021_raw[Feeagh_wlevel_1976_2021_raw$date >= as.Date("2000-01-01"),]

  # check for longest streak for hydrologic calibration
  streak <- stats::na.contiguous(Feeagh_wlevel_1976_2021_post_1990s$wlevel.gp)
  Feeagh_wlevel_1976_2021_post_1990s$date[attr(streak,which = "tsp")[1]] # "2009-04-08"
  Feeagh_wlevel_1976_2021_post_1990s$date[attr(streak,which = "tsp")[2]] # "2015-06-17"
  # use these dates for subsequent calibration of GR4J

  # ----------------------------------------------------------------------------------------------------------#
  # CONVERT HISTORICAL WATER LEVELS TO DISCHARGE (STILL SOME GAPS)
  # ----------------------------------------------------------------------------------------------------------#
  model_rating_curve_Feeagh <- fishcastr::model_rating_curve_Feeagh

  # ----------------------------------------------------------------------------------------------------------#
  # uncorrected for step change
  Feeagh_wlevel_1976_2021_raw$wlevel.gp
  data_Feeagh_discharge <- data.frame("date" = Feeagh_wlevel_1976_2021_raw$date, "discharge_m3.s" =
                                        bbmle::coef(model_rating_curve_Feeagh)["c"]*((Feeagh_wlevel_1976_2021_raw$wlevel.gp)^(bbmle::coef(model_rating_curve_Feeagh)["Beta"])))

  data_Feeagh_discharge <- data_Feeagh_discharge[data_Feeagh_discharge$date <= as.Date("2020-10-07"),]

  saveRDS(object = data_Feeagh_discharge, file = paste0(system.file("extdata", package = "fishcastr"),
                                                     "/data_Feeagh_discharge.rds"))

  # ----------------------------------------------------------------------------------------------------------#
  # with step change adding 0.072m to water level values pre-1996-08-04

    Feeagh_wlevel_1976_2020_corrected <- read.csv(paste0(system.file("extdata", package = "fishcastr"),
                                                   "/EPA_Feeagh/Feeagh_adj/complete_daily_mean_adjusted.csv"),
                                                  stringsAsFactors = FALSE)

    Feeagh_wlevel_1976_2020_corrected$date <- as.Date(Feeagh_wlevel_1976_2020_corrected$date)

  # linear interpolate any gaps of shorter than 2 days
  Feeagh_wlevel_1976_2020_corrected$wlevel_adj.gp = zoo::na.approx(object = Feeagh_wlevel_1976_2020_corrected$wlevel_adj, maxgap = 2)

  model_rating_curve_Feeagh <- fishcastr::model_rating_curve_Feeagh

  data_Feeagh_discharge_corr <- data.frame("date" = Feeagh_wlevel_1976_2020_corrected$date, "discharge_m3.s" =
                                             bbmle::coef(model_rating_curve_Feeagh)["c"]*((Feeagh_wlevel_1976_2020_corrected$wlevel_adj.gp)^(bbmle::coef(model_rating_curve_Feeagh)["Beta"])))

  data_Feeagh_discharge_corr <- data_Feeagh_discharge_corr[data_Feeagh_discharge_corr$date <= as.Date("2020-10-07"),]

  # store downloaded and processed data as .rds file
  saveRDS(object = data_Feeagh_discharge_corr, file = paste0(system.file("extdata", package = "fishcastr"),
                                                     "/data_Feeagh_discharge_corr.rds"))
}
