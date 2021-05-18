#' Download Met Éireann's manual and automatic station data for Furnace,
#' Burrishoole.
#'
#' @description A function to download meteorological observations (manual and
#'   automatic) from two stations near the south shore of Lough Feeagh,
#'   Burrishoole, Co. Mayo, Ireland. The manual station data range from
#'   1959-11-01 to present and the auto station data range from 2005-02-22 to
#'   present. This function subsets both the raw manual station data and the
#'   auto station data to 2019-12-31 and calculates mean air temperature from
#'   max and min daily values. Resultant .rds datasets are stored in the extdata
#'   folder of the fishcastr package.
#'
#' @return A message indicating successful or unsuccessful download.
#' @source
#' \url{https://www.met.ie/climate/available-data/historical-data}
#' @examples
#' \dontrun{
#' download_Met_Eireann_station_data()
#' }
#' @export
download_Met_Eireann_station_data <- function(){

  # ----------------------------------------------------------------------------------------------------------#
  # DOWNLOAD AND UNZIP DATA FROM Met Eireann WEBSITE ----
  # ----------------------------------------------------------------------------------------------------------#

  # manual station ----
  dirName <- paste0(system.file("extdata", package = "fishcastr"),
                    "/Met_Eireann_station_data/")
  dir.create(dirName, showWarnings = TRUE, mode = "0777")

  url = "https://cli.fusio.net/cli/climate_data/webdata/dly833.zip"
  downloader::download(url,
                       destfile=paste0(dirName, "dly833.zip"),
                       mode="wb")
  unzip(paste0(dirName, "dly833.zip"), exdir = paste0(dirName, "manual_station"))

  # automatic station ----
  url = "https://cli.fusio.net/cli/climate_data/webdata/dly1175.zip"
  downloader::download(url,
                       destfile=paste0(dirName, "dly1175.zip"),
                       mode="wb")
  unzip(paste0(dirName, "dly1175.zip"), exdir = paste0(dirName, "auto_station"))

  # ----------------------------------------------------------------------------------------------------------#
  # IMPORT DATA AND SAVE AS RDS
  # ----------------------------------------------------------------------------------------------------------#

  # ----------------------------------------------------------------------------------------------------------#
  # import manual data ----
  dir.dat <- paste0(system.file("extdata", package = "fishcastr"),
         "/Met_Eireann_station_data/manual_station/dly833.csv")

  data_manual <- data.table::fread(file = dir.dat,select = c("date","rain","maxt","mint"),
                                   stringsAsFactors = FALSE)
  data_manual$date <- as.Date(data_manual$date, format = c("%d-%b-%Y"))

  # keep only max and min where both present
  data_manual$maxt <- ifelse(is.na(data_manual$mint), NA, data_manual$maxt)
  data_manual$mint <- ifelse(is.na(data_manual$maxt), NA, data_manual$mint)

  # calculate mean temp
  data_manual$tas_manual <- (data_manual$maxt + data_manual$mint)/2

  # create sequence of dates to fill NA gaps if present
  date_seq <- data.frame("date" = seq(from = min(data_manual$date,
                                                 na.rm = TRUE),
                                      to = max(data_manual$date,
                                               na.rm = TRUE), by = "day"))

  data_met_manual_station <- merge(date_seq, data_manual, by = "date", all.x = TRUE)
  data_met_manual_station_1959_2019 <- data.frame("date" = data_met_manual_station$date,
                                                  "tas_manual" = data_met_manual_station$tas_manual,
                                                  "pr_manual" = data_met_manual_station$rain,
                                                  "tas_min_manual" = data_met_manual_station$mint,
                                                  "tas_max_manual" = data_met_manual_station$maxt)

  # subset to end of 2019
  data_met_manual_station_1959_2019 <- data_met_manual_station_1959_2019[data_met_manual_station_1959_2019$date <= as.Date("2019-12-31"),]

  # store downloaded and processed data as .rds file
  saveRDS(object = data_met_manual_station_1959_2019,
          file = paste0(system.file("extdata", package = "fishcastr"),
                                               "/data_met_manual_station_1959_2019.rds"))

  # ----------------------------------------------------------------------------------------------------------#
  # import auto data ----
  dir.dat <- paste0(system.file("extdata", package = "fishcastr"),
                    "/Met_Eireann_station_data/auto_station/dly1175.csv")

  data_auto <- data.table::fread(file = dir.dat,select = c("date","rain","maxtp","mintp"),
                                 stringsAsFactors = FALSE)
  data_auto$date <- as.Date(data_auto$date, format = c("%d-%b-%Y"))

  # keep only max and min where both present
  data_auto$maxtp <- ifelse(is.na(data_auto$mintp), NA, data_auto$maxtp)
  data_auto$mintp <- ifelse(is.na(data_auto$maxtp), NA, data_auto$mintp)

  # calculate mean temp
  data_auto$tas_auto <- (data_auto$maxtp + data_auto$mintp)/2

  # create sequence of dates to fill NA gaps if present
  date_seq <- data.frame("date" = seq(from = min(data_auto$date,
                                                 na.rm = TRUE),
                                      to = max(data_auto$date,
                                               na.rm = TRUE), by = "day"))

  data_met_auto_station <- merge(date_seq, data_auto, by = "date", all.x = TRUE)
  data_met_auto_station_2005_2019 <- data.frame("date" = data_met_auto_station$date,
                                                  "tas_auto" = data_met_auto_station$tas_auto,
                                                  "pr_auto" = data_met_auto_station$rain,
                                                  "tas_min_auto" = data_met_auto_station$mintp,
                                                  "tas_max_auto" = data_met_auto_station$maxtp)

  # subset to end of 2019
  data_met_auto_station_2005_2019 <- data_met_auto_station_2005_2019[data_met_auto_station_2005_2019$date <= as.Date("2019-12-31"),]

  # store downloaded and processed data as .rds file
  saveRDS(object = data_met_auto_station_2005_2019,
          file = paste0(system.file("extdata", package = "fishcastr"),
                        "/data_met_auto_station_2005_2019.rds"))
}
