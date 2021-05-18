#' Download fish count data
#'
#' @description A function to download daily counts of downstream migrating
#'   diadromous fishes recorded at the Mill Race and Salmon Leap traps in
#'   Burrishoole, Co. Mayo, Ireland since 1970-01-01. The dataset contains
#'   counts of Atlantic salmon smolts, anadromous brown trout smolts (Sea trout
#'   smolts) and European silver eels.
#'
#' @return A message indicating successful or unsuccessful download.
#' @source
#' \url{https://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.4343}
#' @examples
#' \dontrun{
#' download_fish_data()
#' }
#' @export
download_fish_data <- function(){

  # ------------------------------------------------------------------------------------- #
  # download the csv file

  url = "https://erddap.marine.ie/erddap/tabledap/newport_daily_fish_counts.csv"

  dirName <- paste0(system.file("extdata", package = "fishcastr"),
                    "/MI_fish_counts/")

  dir.create(dirName, showWarnings = TRUE, mode = "0777")

  downloader::download(url,
                       destfile=paste0(dirName, "newport_daily_fish_counts.csv"),
                       mode="wb")

  # store each species downloaded data as .rds files
  # ------------------------------------------------------------------------------------- #
  # SSMOLT
  data_ssmolt <- read.csv(paste0(system.file("extdata", package = "fishcastr"),
                                 "/MI_fish_counts/newport_daily_fish_counts.csv"),
                          header = TRUE,stringsAsFactors = FALSE)[-1,c("time","salmon_smolt_count_per_day")]
  data_ssmolt$time <- as.Date(data_ssmolt$time)
  names(data_ssmolt) <- c("date","ssmolt")
  data_ssmolt$ssmolt <- as.numeric(data_ssmolt$ssmolt)
  # create .rds file in extdata folder for testing code
  saveRDS(object = data_ssmolt, file = paste0(system.file("extdata", package = "fishcastr"),
                                              "/data_ssmolt.rds"))

  # ------------------------------------------------------------------------------------- #
  # STSMOLT
  data_stsmolt <- read.csv(paste0(system.file("extdata", package = "fishcastr"),
                                  "/MI_fish_counts/newport_daily_fish_counts.csv"),
                           header = TRUE,stringsAsFactors = FALSE)[-1,c("time","sea_trout_smolt_count_per_day")]
  data_stsmolt$time <- as.Date(data_stsmolt$time)
  names(data_stsmolt) <- c("date","stsmolt")
  data_stsmolt$stsmolt <- as.numeric(data_stsmolt$stsmolt)
  # create .rds file in extdata folder for testing code
  saveRDS(object = data_stsmolt, file = paste0(system.file("extdata", package = "fishcastr"),
                                               "/data_stsmolt.rds"))

  # ------------------------------------------------------------------------------------- #
  # SEEL
  data_seel <- read.csv(paste0(system.file("extdata", package = "fishcastr"),
                               "/MI_fish_counts/newport_daily_fish_counts.csv"),
                        header = TRUE,stringsAsFactors = FALSE)[-1,c("time","eel_count_per_day")]
  data_seel$time <- as.Date(data_seel$time)
  names(data_seel) <- c("date","seel")
  data_seel$seel <- as.numeric(data_seel$seel)
  # create .rds file in extdata folder for testing code
  saveRDS(object = data_seel, file = paste0(system.file("extdata", package = "fishcastr"),
                                            "/data_seel.rds"))
}
