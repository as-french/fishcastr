#' Download surface water temperature for Lough Feeagh, Burrishoole, Ireland
#'
#' @description A function to download and pre-process water temperature
#'   recordings at 2m depth in Lough Feeagh, Burrishoole, Co. Mayo, Ireland. The
#'   resulting .rds dataset is stored in the extdata folder in the package
#'   directory and contains daily mean water temperature (at 2m depth) that has
#'   been aggregated from 2 minutely records from the Automatic Water Quality
#'   monitoring station (AWQMS) from 2004-01-01 to 2019-12-31.
#'
#' @return A message indicating successful or unsuccessful download.
#' @source
#' \url{https://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.3757}
#' @examples
#' \dontrun{
#' download_Feeagh_water_temp()
#' }
#' @export
download_Feeagh_water_temp <- function(){

  # ------------------------------------------------------------------------------------- #
  # download the csv file
  url = "https://data.marine.ie/data/edd58462-ae36-44b2-bf36-0ef06c6e8357.zip"

  # or
# url = https://erddap.marine.ie/erddap/tabledap/IMINewportBuoys.csv?station_name%2Clatitude%2Clongitude%2Cstation%2Clnstationid%2Ctime%2Cyear%2Cmonth%2Cday%2Cwater_temp_2pt5m&time%3C=2004-01-01T00%3A00%3A00Z&time%3C=2019-12-31T00%3A00%3A00Z

  dirName <- paste0(system.file("extdata", package = "fishcastr"),
                    "/MI_Feeagh_2m_temp/")
  dir.create(dirName, showWarnings = TRUE, mode = "0777")

  downloader::download(url,
                       destfile=paste0(dirName, "edd58462-ae36-44b2-bf36-0ef06c6e8357.zip"),
                       mode="wb")
  unzip(paste0(dirName, "edd58462-ae36-44b2-bf36-0ef06c6e8357.zip"),
        exdir = paste0(dirName, "Feeagh_2m_temp_unzip"))

  #extract 2m temperature from all years of data and aggregate to daily means
  dataset_list <- list.files(path = paste0(dirName, "Feeagh_2m_temp_unzip"),
                             pattern = NULL, all.files = FALSE, full.names = TRUE,
                             recursive = TRUE, ignore.case = FALSE,
                             include.dirs = FALSE, no.. = FALSE)

  # retain only files with FEEAGH_YEAR
  # check file names dataset_list
  dataset_list <- dataset_list[-c(1,2,19)]

  # check file format and column names
  check_format_csv <- readLines(con = dataset_list[1],n = 5)

  # extract 2m water temp data from each year, aggregate to daily data and return result
  data_list_feeagh <- lapply(dataset_list,FUN = function(x){

  # read specified columns
  fee_data_i <- data.table::fread(file = x,select = c("time","Water_Temp_2m"),
                                  stringsAsFactors = FALSE)[-1,]

  # aggregate 2 minutely data to daily means
  fee_data_i$time <- as.POSIXct(fee_data_i$time,tz = "UTC", format ="%d/%m/%Y %H:%M")
  fee_data_i$Water_Temp_2m <- as.numeric(fee_data_i$Water_Temp_2m)

  # cut to to 24 hour intervals 00:00 to 00:00
  fee_data_i$int_24h = cut(fee_data_i$time, breaks="1 day")

  # aggregate to daily mean
  fee_data_i_daily_mean = aggregate(Water_Temp_2m ~ int_24h, FUN=mean, na.rm = TRUE, data=fee_data_i)
  fee_data_i_daily_mean$int_24h <- as.POSIXct(fee_data_i_daily_mean$int_24h,tz = "UTC")
  colnames(fee_data_i_daily_mean) <- c("date","Water_Temp_2m")
  fee_data_i_daily_mean$date <-as.Date(fee_data_i_daily_mean$date)

  # create sequence of dates to fill na gaps
  date_seq <- data.frame("date" = seq(from = as.Date(paste0(lubridate::year(min(fee_data_i_daily_mean$date,
                                                 na.rm = TRUE)),"-01-01")),
                                      to = as.Date(paste0(lubridate::year(min(fee_data_i_daily_mean$date,
                                                                       na.rm = TRUE)),"-12-31")), by = "day"))
  # merge date seq
  fee_data_i_daily_mean_merge <- merge(date_seq, fee_data_i_daily_mean, by = "date", all.x = TRUE)

  # check data looks plausible
  #plot(fee_data_i_daily_mean_merge$date,fee_data_i_daily_mean_merge$Water_Temp_2m, type = "l")
  })
  names(data_list_feeagh) <- substr(dataset_list, nchar(dataset_list[1])-14, nchar(dataset_list)-4)

  # bind all data from first to last year
  data_Feeagh2m_temp <- do.call(rbind,data_list_feeagh)

  # -------------------------------------------------------------------------------------------------- #
  # MANUAL QA QC ----
  # -------------------------------------------------------------------------------------------------- #
  # manually check for odd values by plotting
  # plot(data_Feeagh2m_temp$date,log(data_Feeagh2m_temp$Water_Temp_2m), type = "l") # clearly at least 10 extreme values that need removing
  # data_Feeagh2m_temp$year <- lubridate::year(data_Feeagh2m_temp$date)
  # for(i in unique(data_Feeagh2m_temp$year)){
  #   par(mfrow = c(1,2))
  #   plot(data_Feeagh2m_temp$date[data_Feeagh2m_temp$year == i],
  #        (data_Feeagh2m_temp$Water_Temp_2m[data_Feeagh2m_temp$year == i]), type = "l", main = i)
  #   plot(data_Feeagh2m_temp$date[data_Feeagh2m_temp$year == i],
  #        log(data_Feeagh2m_temp$Water_Temp_2m[data_Feeagh2m_temp$year == i]), type = "l", main = i)
  # }

  # 2004 to 2008 no issues
  # 2009-04-29 20 plus degrees odd
  # 2009-09-07 58 degrees odd
  # 2010 fine
  # 2011 2011-12-14 350000 value
  # 2012 2012-09-20 10^7 value
  # 2013 fine
  # 2014 fine
  # 2015 2015-09-21 2015-09-24 	2015-11-08 2015-11-12
  # 2016 2016-02-18 2016-02-19
  # 2017 fine
  # 2018 2018-11-13
  # 2019 2019-11-05 2019-11-06 2019-11-07 2019-11-08 2019-11-09

  QAQC_remove <- as.Date(c("2009-04-29", "2009-09-07", "2011-12-14", "2012-09-20", "2015-09-21", "2015-09-24", "2015-11-08", "2015-11-12", "2016-02-18", "2016-02-19", "2018-11-13", "2019-11-05", "2019-11-06", "2019-11-07", "2019-11-08", "2019-11-09"))

  data_Feeagh2m_temp$Water_Temp_2m[data_Feeagh2m_temp$date %in% QAQC_remove] <- NA

  # # check again for any missed implausible values
  # for(i in unique(data_Feeagh2m_temp$year)){
  #   par(mfrow = c(1,2))
  #   plot(data_Feeagh2m_temp$date[data_Feeagh2m_temp$year == i],
  #        (data_Feeagh2m_temp$Water_Temp_2m[data_Feeagh2m_temp$year == i]), type = "l", main = i)
  #   plot(data_Feeagh2m_temp$date[data_Feeagh2m_temp$year == i],
  #        log(data_Feeagh2m_temp$Water_Temp_2m[data_Feeagh2m_temp$year == i]), type = "l", main = i)
  # }
  # fine
  # # satisfied remove year column
  # data_Feeagh2m_temp$year <- NULL
  # -------------------------------------------------------------------------------------------------- #

  # store downloaded and processed data as .rds file
  saveRDS(object = data_Feeagh2m_temp, file = paste0(system.file("extdata", package = "fishcastr"),
                                              "/data_Feeagh2m_temp.rds"))
}
