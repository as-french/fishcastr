#' Download midnight water temperature data for the the Mill Race, Burrishoole,
#' Ireland
#'
#' @description A function to download water temperature recordings for the Mill
#'   Race, Burrishoole, Co. Mayo, Ireland. The raw data records span 1960-01-01 to
#'   2017-12-31.
#'
#' @return A message indicating successful or unsuccessful download.
#' @source
#' \url{https://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.2796}
#' @examples
#' \dontrun{
#' download_MR_temperature()
#' }
#' @export
download_MR_temperature <- function(){

  # ----------------------------------------------------------------------------------------------------------#
  # DOWNLOAD AND UNZIP DATA FROM EPA WEBSITE ----
  # ----------------------------------------------------------------------------------------------------------#
  dirName <- paste0(system.file("extdata", package = "fishcastr"),
                    "/MI_MR_temp/")
  dir.create(dirName, showWarnings = TRUE, mode = "0777")

  url = "https://data.marine.ie/data/8b82e6e3-69f0-4ebb-8ff2-2af0dad04b8a/8b82e6e3-69f0-4ebb-8ff2-2af0dad04b8a.csv"
  downloader::download(url,
                       destfile=paste0(dirName, "8b82e6e3-69f0-4ebb-8ff2-2af0dad04b8a.csv"),
                       mode="wb")
  # ----------------------------------------------------------------------------------------------------------#
  # IMPORT DATA AND SAVE AS RDS FOR COMPARISON WITH WATER TEMPERATURE MODEL PREDICTIONS
  # ----------------------------------------------------------------------------------------------------------#
  # import feeagh data removing first 7 lines and delineating by semi colon
  data_MR <- read.table(paste0(system.file("extdata", package = "fishcastr"),
                               "/MI_MR_temp/8b82e6e3-69f0-4ebb-8ff2-2af0dad04b8a.csv"),
                        sep=",", quote="\"",
                        stringsAsFactors = FALSE)[-c(1:14),]

  names(data_MR) <- c("date","meanT","QCcode","comments")
  #remove any values that have something other than blank in QC column
  data_MR_sub <- data_MR[data_MR$QCcode == "",1:2]
  data_MR_sub$date <- as.Date(data_MR_sub$date,format = "%d/%m/%Y")
  data_MR_sub$meanT <- as.numeric(as.character(data_MR_sub$meanT))

  # create sequence of dates to fill na gaps
  date_seq <- data.frame("date" = seq(from = min(data_MR_sub$date,
                                                 na.rm = TRUE),
                                      to = max(data_MR_sub$date,
                                               na.rm = TRUE), by = "day"))

  data_MR_temp <- merge(date_seq, data_MR_sub, by = "date", all.x = TRUE)

  # store downloaded and processed data as .rds file
  saveRDS(object = data_MR_temp, file = paste0(system.file("extdata", package = "fishcastr"),
                                                             "/data_MR_temp.rds"))
}
