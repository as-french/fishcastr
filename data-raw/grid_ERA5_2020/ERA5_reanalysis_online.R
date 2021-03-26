require(keyring)
require(ecmwfr)
#devtools::install_github(c("SantanderMetGroup/transformeR", "SantanderMetGroup/downscaleR"))
 # library(loadeR)
 # library(transformeR)
 # library(loadeR.ECOMS)
 # library(visualizeR)
 # library(convertR)
 # library(drought4R)
 # library(downscaleR)
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1")

# CDS ERA5 REanalysis
ecmwfr::wf_set_key(user = "xxxxx",
           key = ecmwfr::wf_get_key(user = "xxxxx",
                                    service = "cds"),
           service = "cds")

#################################### for all 2020 (with new "expver" variable depending on date downloaded) ##
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2020",
  month = "01",
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)
####################################################################

################### 2020 Feb to June request (with new "expver" variable depending on date downloaded) ######
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2020",
  month = c("02", "03", "04", "05", "06"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)
####################################################################

####################################################################
# July to Nov 2020
####################################################################
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2020",
  month = c("07", "08", "09", "10", "11"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)
####################################################################

####################################################################
# December 2020  to January 2021 (expver workaround separate API requests)
####################################################################
# dec 1st 00:00:00 to 06:00:00 am (TEMP)
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature"),
  year = "2020",
  month = "12",
  day = "01",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00","06:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)
# dec 1st 00:00:00 to 06:00:00 am (PRECIP)
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("total_precipitation"),
  year = "2020",
  month = "12",
  day = "01",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00","06:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)

# dec 1st 07:00:00 to end of month
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2020",
  month = "12",
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)
# Jan 1st 00:00:00 onwards to end of month
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2021",
  month = "01",
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset = "reanalysis-era5-single-levels",
  target = "download.nc"
)

# ----------------------------------------------------------------------------------- #
# reanalysis July beginning to November end
dirName <- paste0(getwd(),"/inst/extdata/", "/ERA5_2020_Jul_Nov", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = FALSE, mode = "0777")

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = paste0(getwd(),"/inst/extdata/ERA5_2020_Jul_Nov"),
                             verbose = TRUE)

# ----------------------------------------------------------------------------------- #
# reanalysis December 1st 00:00 to 06:00 t2m
dirName <- paste0(getwd(),"/inst/extdata/", "/ERA5_2020_Dec_00_t2m", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = FALSE, mode = "0777")

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = paste0(getwd(),"/inst/extdata/ERA5_2020_Dec_00_t2m"),
                             verbose = TRUE)

# reanalysis December 1st 00:00 to 06:00 pr
dirName <- paste0(getwd(),"/inst/extdata/", "/ERA5_2020_Dec_00_tp", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = FALSE, mode = "0777")

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = paste0(getwd(),"/inst/extdata/ERA5_2020_Dec_00_tp"),
                             verbose = TRUE)

# reanalysis December 1st 07:00 onwards
dirName <- paste0(getwd(),"/inst/extdata/", "/ERA5_2020_Dec_07", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = FALSE, mode = "0777")

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = paste0(getwd(),"/inst/extdata/ERA5_2020_Dec"),
                             verbose = TRUE)

# ----------------------------------------------------------------------------------- #
# reanalysis January
dirName <- paste0(getwd(),"/inst/extdata/", "/ERA5_2021_Jan", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = FALSE, mode = "0777")
ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = paste0(getwd(),"/inst/extdata/ERA5_2021_Jan"),
                             verbose = TRUE)

# ----------------------------------------------------------------------------------- #

# read nc file and append to ERA5 raw to make operational forecast...
# for salmon...
#dataset_2020 <- paste0(getwd(),"/inst/extdata/ERA5_2020_Jan/download.nc")

#####################
# if separate requests are not possible (no reason to think they wouldn't be) to get this file into correct format for using UNICAN tools DM used following code in command prompt
#cdo -f nc copy download.nc download_ok.nc
# this is based on CDO tools: https://code.mpimet.mpg.de/projects/cdo/
# but there is an r wrapper: https://github.com/markpayneatwork/ClimateOperators
# alternative within r itself? or perhaps donload a grib file from CDS instead
#####################

# --------------------------------------------------------------------------- #
# SALMON
# --------------------------------------------------------------------------- #
# for salmon (usually appending data in January from end of eel forecast, so
# July to Nov, then append Dec day one and all other days, then append
# January.. long winded, but necessary given current setup of CDS API and "expver")
# dataset_old <- readRDS(paste0(getwd(),"/data/ERA5_WATExR_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_petH_raw.rds")) # for 2020

# load data and convert all raw grid data into form for binding grids ----
dataset_old <- readRDS(paste0(getwd(),"/data/ERA5_WATExR_tas_pr_petH_raw_op_2020_Au.rds")) # for 2021
dataset_July_Nov <- paste0(getwd(),"/inst/extdata/ERA5_2020_Jul_Nov/download.nc")
dataset_Dec_00_t2m <- paste0(getwd(),"/inst/extdata/ERA5_2020_Dec_00_t2m/download.nc")
dataset_Dec_00_tp <- paste0(getwd(),"/inst/extdata/ERA5_2020_Dec_00_tp/download.nc")
dataset_Dec_07 <- paste0(getwd(),"/inst/extdata/ERA5_2020_Dec_07/download.nc")
dataset_Jan <- paste0(getwd(),"/inst/extdata/ERA5_2021_Jan/download.nc")

# ------------------------------------------------------------------------------------ #
# Jul - Nov ----
# ------------------------------------------------------------------------------------ #
# t2m and tp ----
variables <- c("t2m","tp")
aggr.fun <- c("mean","sum")
years <- 2020
season <- 7:11
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
require(rJava) # need this for is.jnull function if not loading entire loadeR package
di <- loadeR::dataInventory(dataset = dataset_July_Nov)
dicName <- paste0(getwd(), "/inst/extdata/ERA5_ecmwf.dic")
list_era5_Jul_Nov_t2m_tp <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_July_Nov,
                                                                                 var = variables[x],
                                                                                 years = years,
                                                                                 lonLim = lonLim,
                                                                                 latLim = latLim,
                                                                                 season = season,
                                                                                 time = "DD",
                                                                                 aggr.d = aggr.fun[x],
                                                                                 dictionary = dicName))
names(list_era5_Jul_Nov_t2m_tp) <- c("tas","pr")
list_era5_Jul_Nov_t2m_tp$tas$Variable$varName <- "tas"
list_era5_Jul_Nov_t2m_tp$pr$Variable$varName <- "pr"

# petH ----
list_ncs_tmin <- list()
variables <- "t2m"
list_ncs_tmin[[1]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_July_Nov,
                                                                                   var = variables,
                                                                                   years = years,
                                                                                   lonLim = lonLim,
                                                                                   latLim = latLim,
                                                                                   season = season,
                                                                                   time = "DD",
                                                                                   aggr.d = "min",
                                                                                   dictionary = dicName))
names(list_ncs_tmin)[1] <- years
names(list_ncs_tmin[[1]]) <- "t2m"

list_ncs_tmax <- list()
list_ncs_tmax[[1]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_July_Nov,
                                                                                   var = variables,
                                                                                   years = years,
                                                                                   lonLim = lonLim,
                                                                                   latLim = latLim,
                                                                                   season = season,
                                                                                   time = "DD",
                                                                                   aggr.d = "max",
                                                                                   dictionary = dicName))
names(list_ncs_tmax)[1] <- years
names(list_ncs_tmax[[1]]) <- "t2m"

# BIND petH GRIDS ----
data.prelim_t2m_min <- sapply(list_ncs_tmin, "[", 1)[[1]]
data.prelim_t2m_max <- sapply(list_ncs_tmax, "[", 1)[[1]]

# bind grids by time
#tasmin.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_min, dimension = "time"))
#tasmax.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_max, dimension = "time"))

# Compute potential evapotranspiration with function petGrid from package drought4R
# For daily data the implemented method is hargreaves-samani (See ?petGrid for details)
# petGrid function requires temperature in celsius. Convert temperature units to celsius (if required).
transformeR::getGridUnits(data.prelim_t2m_min)
transformeR::getGridUnits(data.prelim_t2m_max) # no units, so set them

str(data.prelim_t2m_min) # check location of stored units...
attr(data.prelim_t2m_min$Variable, "units") <- "celsius"
attr(data.prelim_t2m_max$Variable, "units") <- "celsius"

#tasmax <- convertR::udConvertGrid(data.prelim_t2m_max, new.units = "celsius")
#tasmin <- convertR::udConvertGrid(data.prelim_t2m_min, new.units = "celsius")
petH <- drought4R::petGrid(tasmin = data.prelim_t2m_min,
                           tasmax = data.prelim_t2m_max,
                           method = "hargreaves-samani")


# add petH to July Nov list and export to rds for later binding by time

ERA5_Jul_Nov_tas_pr_petH_grid <- list("tas" = list_era5_Jul_Nov_t2m_tp$tas,
                                 "pr" = list_era5_Jul_Nov_t2m_tp$pr,
                                 "petH" = petH)

# ------------------------------------------------------------------------------------ #
# Dec binding (separate for tas and pr)
# ------------------------------------------------------------------------------------ #
# t2m ----
variables <- c("t2m")
aggr.fun <- c("none")
years <- 2020
season <- 12
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
#?loadeR::loadGridData
require(rJava) # need this for is.jnull function if not loading entire loadeR package
di <- loadeR::dataInventory(dataset = dataset_Dec_00_t2m)
dicName <- paste0(getwd(), "/inst/extdata/ERA5_ecmwf.dic")
list_era5_Dec_00_t2m <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Dec_00_t2m,
                                                                                         var = variables[x],
                                                                                         years = years,
                                                                                         lonLim = lonLim,
                                                                                         latLim = latLim,
                                                                                         season = season,
                                                                                         time = "none",
                                                                                         aggr.d = aggr.fun[x],
                                                                                         dictionary = dicName))
names(list_era5_Dec_00_t2m) <- c("tas")
list_era5_Dec_00_t2m$tas$Variable$varName <- "tas"

# load t2m for rest of dec to bind
list_era5_Dec_07_t2m <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Dec_07,
                                                                                     var = variables[x],
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "none",
                                                                                     aggr.d = aggr.fun[x],
                                                                                     dictionary = dicName))
names(list_era5_Dec_07_t2m) <- c("tas")
list_era5_Dec_07_t2m$tas$Variable$varName <- "tas"

# bind grids by time
list_era5_Dec_tas <- list(list_era5_Dec_00_t2m$tas,list_era5_Dec_07_t2m$tas)
ERA5_Dec_tas <- list("tas" = do.call(transformeR::bindGrid, c(list_era5_Dec_tas, dimension = "time")))

# aggregate to daily mean
ERA5_Dec_tas_DD <- transformeR::aggregateGrid(grid = ERA5_Dec_tas$tas,
                                              aggr.d = list(FUN = "mean", na.rm = TRUE))
# aggregate to daily min
ERA5_Dec_tas_min_DD <- transformeR::aggregateGrid(grid = ERA5_Dec_tas$tas,
                                              aggr.d = list(FUN = "min", na.rm = TRUE))
# aggregate to daily max
ERA5_Dec_tas_max_DD <- transformeR::aggregateGrid(grid = ERA5_Dec_tas$tas,
                                              aggr.d = list(FUN = "max", na.rm = TRUE))

transformeR::getGridUnits(ERA5_Dec_tas_min_DD)
transformeR::getGridUnits(ERA5_Dec_tas_max_DD) # no units, so set them to celsius for petH function

str(ERA5_Dec_tas_min_DD) # check location of stored units...
attr(ERA5_Dec_tas_min_DD$Variable, "units") <- "celsius"
attr(ERA5_Dec_tas_max_DD$Variable, "units") <- "celsius"

# change dates to no longer include hours (required for petH calculation)
class(ERA5_Dec_tas_min_DD$Dates$start)
min_time <- min(ERA5_Dec_tas_min_DD$Dates$start)
min_date <- ISOdate(year = lubridate::year(min_time),month = lubridate::month(min_time),day = lubridate::day(min_time),hour = 0,tz = "UTC")

date_data_start <- as.character(strptime(seq(from = min_date,by = "day",length.out = length(ERA5_Dec_tas_min_DD$Dates$start)),format = '%Y-%m-%d'),'%Y-%m-%d %Z')

date_data_end <- as.character(strptime(seq(from = min_date + lubridate::days(1),by = "day",length.out = length(ERA5_Dec_tas_min_DD$Dates$start)),format = '%Y-%m-%d'),'%Y-%m-%d %Z')

newdates_start <- array(data = date_data_start)
newdates_end <- array(data = date_data_end)

ERA5_Dec_tas_min_DD$Dates$start <- newdates_start
ERA5_Dec_tas_min_DD$Dates$end <- newdates_end

ERA5_Dec_tas_max_DD$Dates$start <- newdates_start
ERA5_Dec_tas_max_DD$Dates$end <- newdates_end

petH <- drought4R::petGrid(tasmin = ERA5_Dec_tas_min_DD,
                           tasmax = ERA5_Dec_tas_max_DD,
                           method = "hargreaves-samani")

# tp ----
variables <- c("tp")
aggr.fun <- c("none")
years <- 2020
season <- 12
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
#?loadeR::loadGridData
require(rJava) # need this for is.jnull function if not loading entire loadeR package
di <- loadeR::dataInventory(dataset = dataset_Dec_00_tp)
dicName <- paste0(getwd(), "/inst/extdata/ERA5_ecmwf.dic")
list_era5_Dec_00_tp <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Dec_00_tp,
                                                                                     var = variables[x],
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "none",
                                                                                     aggr.d = aggr.fun[x],
                                                                                     dictionary = dicName))
names(list_era5_Dec_00_tp) <- c("pr")
list_era5_Dec_00_tp$pr$Variable$varName <- "pr"

# load tp for rest of dec to bind
list_era5_Dec_07_tp <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Dec_07,
                                                                                     var = variables[x],
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "none",
                                                                                     aggr.d = aggr.fun[x],
                                                                                     dictionary = dicName))
names(list_era5_Dec_07_tp) <- c("pr")
list_era5_Dec_07_tp$pr$Variable$varName <- "pr"

# bind grids by time
list_era5_Dec_pr <- list(list_era5_Dec_00_tp$pr,list_era5_Dec_07_tp$pr)
ERA5_Dec_pr <- list("pr" = do.call(transformeR::bindGrid, c(list_era5_Dec_pr, dimension = "time")))

# aggregate to daily sum
ERA5_Dec_pr_DD <- transformeR::aggregateGrid(grid = ERA5_Dec_pr$pr,
                                              aggr.d = list(FUN = "sum", na.rm = TRUE))


# set daily time stamps
ERA5_Dec_pr_DD$Dates$start <- newdates_start
ERA5_Dec_pr_DD$Dates$end <- newdates_end

# combine objects into Dec list
ERA5_Dec_tas_pr_petH <- list("tas" = ERA5_Dec_tas_DD,
                             "pr" = ERA5_Dec_pr_DD,
                             "petH" = petH)

# sort out date formats for tas and pr
ERA5_Dec_tas_pr_petH$tas$Dates <- ERA5_Dec_tas_pr_petH$petH$Dates
ERA5_Dec_tas_pr_petH$pr$Dates <- ERA5_Dec_tas_pr_petH$petH$Dates


# ------------------------------------------------------------------------------------ #
# Jan ----
# ------------------------------------------------------------------------------------ #
# t2m and tp ----
variables <- c("t2m","tp")
aggr.fun <- c("mean","sum")
years <- 2021
season <- 1
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
require(rJava) # need this for is.jnull function if not loading entire loadeR package
di <- loadeR::dataInventory(dataset = dataset_Jan)
dicName <- paste0(getwd(), "/inst/extdata/ERA5_ecmwf.dic")
list_era5_Jan_t2m_tp <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Jan,
                                                                                         var = variables[x],
                                                                                         years = years,
                                                                                         lonLim = lonLim,
                                                                                         latLim = latLim,
                                                                                         season = season,
                                                                                         time = "DD",
                                                                                         aggr.d = aggr.fun[x],
                                                                                         dictionary = dicName))
names(list_era5_Jan_t2m_tp) <- c("tas","pr")
list_era5_Jan_t2m_tp$tas$Variable$varName <- "tas"
list_era5_Jan_t2m_tp$pr$Variable$varName <- "pr"

# petH ----
list_ncs_tmin <- list()
variables <- "t2m"
list_ncs_tmin[[1]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Jan,
                                                                                   var = variables,
                                                                                   years = years,
                                                                                   lonLim = lonLim,
                                                                                   latLim = latLim,
                                                                                   season = season,
                                                                                   time = "DD",
                                                                                   aggr.d = "min",
                                                                                   dictionary = dicName))
names(list_ncs_tmin)[1] <- years
names(list_ncs_tmin[[1]]) <- "t2m"

list_ncs_tmax <- list()
list_ncs_tmax[[1]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_Jan,
                                                                                   var = variables,
                                                                                   years = years,
                                                                                   lonLim = lonLim,
                                                                                   latLim = latLim,
                                                                                   season = season,
                                                                                   time = "DD",
                                                                                   aggr.d = "max",
                                                                                   dictionary = dicName))
names(list_ncs_tmax)[1] <- years
names(list_ncs_tmax[[1]]) <- "t2m"

# BIND petH GRIDS ----
data.prelim_t2m_min <- sapply(list_ncs_tmin, "[", 1)[[1]]
data.prelim_t2m_max <- sapply(list_ncs_tmax, "[", 1)[[1]]

# bind grids by time
#tasmin.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_min, dimension = "time"))
#tasmax.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_max, dimension = "time"))

# Compute potential evapotranspiration with function petGrid from package drought4R
# For daily data the implemented method is hargreaves-samani (See ?petGrid for details)
# petGrid function requires temperature in celsius. Convert temperature units to celsius (if required).
transformeR::getGridUnits(data.prelim_t2m_min)
transformeR::getGridUnits(data.prelim_t2m_max) # no units, so set them

str(data.prelim_t2m_min) # check location of stored units...
attr(data.prelim_t2m_min$Variable, "units") <- "celsius"
attr(data.prelim_t2m_max$Variable, "units") <- "celsius"

#tasmax <- convertR::udConvertGrid(data.prelim_t2m_max, new.units = "celsius")
#tasmin <- convertR::udConvertGrid(data.prelim_t2m_min, new.units = "celsius")
petH <- drought4R::petGrid(tasmin = data.prelim_t2m_min,
                           tasmax = data.prelim_t2m_max,
                           method = "hargreaves-samani")


# add petH to July Nov list and export to rds for later binding by time

ERA5_Jan_tas_pr_petH_grid <- list("tas" = list_era5_Jan_t2m_tp$tas,
                                      "pr" = list_era5_Jan_t2m_tp$pr,
                                      "petH" = petH)


# ------------------------------------------------------------------------------------------ #
# BIND AND INTERPOLATE NEW DAILY GRIDS TOGETHER JULY NOV - DEC - JAN
# ------------------------------------------------------------------------------------------ #

# tas
list_era5_Jul_Jan_tas <- list(ERA5_Jul_Nov_tas_pr_petH_grid$tas,
                         ERA5_Dec_tas_pr_petH$tas,
                         ERA5_Jan_tas_pr_petH_grid$tas)
ERA5_Jul_Jan_tas <- list("tas" = do.call(transformeR::bindGrid,
                                        c(list_era5_Jul_Jan_tas, dimension = "time")))

# pr
list_era5_Jul_Jan_pr <- list(ERA5_Jul_Nov_tas_pr_petH_grid$pr,
                         ERA5_Dec_tas_pr_petH$pr,
                         ERA5_Jan_tas_pr_petH_grid$pr)
ERA5_Jul_Jan_pr <- list("pr" = do.call(transformeR::bindGrid,
                                       c(list_era5_Jul_Jan_pr, dimension = "time")))

# petH
list_era5_Jul_Jan_petH <- list(ERA5_Jul_Nov_tas_pr_petH_grid$petH,
                         ERA5_Dec_tas_pr_petH$petH,
                         ERA5_Jan_tas_pr_petH_grid$petH)
ERA5_Jul_Jan_petH <- list("petH" = do.call(transformeR::bindGrid,
                                         c(list_era5_Jul_Jan_petH, dimension = "time")))

# list format
ERA5_Jul_Jan <- list("tas"= ERA5_Jul_Jan_tas$tas,
                     "pr" = ERA5_Jul_Jan_pr$pr,
                     "petH" = ERA5_Jul_Jan_petH$petH)

# interpolate
lake <- list(x = -9.578, y = 53.973) # Burrishoole
data.interp_new <- lapply(ERA5_Jul_Jan, function(x) transformeR::interpGrid(x, new.coordinates = lake,
                                                                            method = "bilinear",
                                                                            bilin.method = "akima"))

# ------------------------------------------------------------------------------------------ #
# BIND INTERPOLATED OLD TO NEW BINDED GRID
# ------------------------------------------------------------------------------------------ #
# tas
list_era5_op_bind_tas <- list(data.interp_new$tas,
                              dataset_old$tas)
ERA5_op_bind_tas <- list("tas" = do.call(transformeR::bindGrid,
                                         c(list_era5_op_bind_tas, dimension = "time")))

# pr
list_era5_op_bind_pr <- list(data.interp_new$pr,
                             dataset_old$pr)
ERA5_op_bind_pr <- list("pr" = do.call(transformeR::bindGrid,
                                       c(list_era5_op_bind_pr, dimension = "time")))

# petH
list_era5_op_bind_petH <- list(data.interp_new$petH,
                               dataset_old$petH)
ERA5_op_bind_petH <- list("petH" = do.call(transformeR::bindGrid,
                                           c(list_era5_op_bind_petH, dimension = "time")))

# list format
ERA5_op_bind_raw <- list("tas"= ERA5_op_bind_tas$tas,
                     "pr" = ERA5_op_bind_pr$pr,
                     "petH" = ERA5_op_bind_petH$petH)

# export new op data 2021
saveRDS(ERA5_op_bind_raw,
        file = paste0(getwd(),"/data/ERA5_WATExR_tas_pr_petH_raw_op_2021_Sp.rds"))
# ------------------------------------------------------------------------------------------ #

# BIAS Correct op data
data_op <- readRDS(file = paste0(getwd(),"/data/ERA5_WATExR_tas_pr_petH_raw_op_2021_Sp.rds"))
#any(is.na(data_op$petH$Data)) # no nas in reanalysis data
obs.data <- readRDS(paste0(getwd(),"/data/Furnace_Station_1979_2019_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_petH.rds"))
#any(is.na(obs.data$pr$Data)) # no nas in obs data
# Subset observational data to the same dates as forecast data for bias correction calibration
obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data_op[[x]], type = "temporal", which.return = 1)})
data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data_op[[x]], type = "temporal", which.return = 2)})
varnames <- c("tas", "pr","petH")
names(obs.data) <- varnames
names(data) <- varnames
#data_raw <- data
# Collect some common metadata (e.g. from variable uas)
dates <- data[[1]]$Dates
xycoords <- transformeR::getCoordinates(data[[1]])

# Bias correction with leave-one-year-out ("loo") cross-validation type
# ?biasCorrection in R for more info about the parameter settings for bias
# correction. # old version downscale r 3.1.3, not install 3.3.1

data.bc.cross <- lapply(1:length(data), function(x)  {
  precip <- FALSE
  if (names(data)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data[[x]],
                             x = data[[x]],
                             method = "eqm",
                             cross.val = "loo",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             window = c(31, 7))
})
names(data.bc.cross) <- names(data)
#which(is.na(data.bc.cross$pr$Data)) # row 9692 of pr is na (why?)
# dont use loo for operational year (it doesn't work)! It is automatic with
# "newdata" argument, but retain cross validated version for reforecast period to
# be representative of operational forecast.
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1")
#with downscaleR@v3.3.1
# Error in arr[, , , ind, , ] <- grid[["Data"]] :
# number of items to replace is not a multiple of replacement length, so install old versions again
#devtools::install_github("SantanderMetGroup/transformeR@v1.7.4") # was 2.0.1
#devtools::install_github("SantanderMetGroup/downscaleR@v3.1.3") # was 3.3.1

data.bc <- lapply(1:length(data), function(x)  {
  precip <- FALSE
  if (names(data)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data[[x]],
                             x = data[[x]],
                             newdata = data_op[[x]],
                             extrapolation = "constant",
                             method = "eqm",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             window = c(31, 7),)
})
names(data.bc) <- names(data)

#                 window = c(90, 31),

# -------------------------------------------------------------------------------------------------------------- #
# Append (loo CV) bias corrected reforecast to bias corrected operational year (e.g., 2021) --------------------
# -------------------------------------------------------------------------------------------------------------- #

# subset operational year -------
data.bc.subset <- lapply(data.bc, function(x) transformeR::subsetGrid(grid = x,
                                                                      years = 2020:2021,
                                                                      season = 1:12))

# bind grids -----
data.bc.tas.list <- list("tas_refor" = data.bc.cross$tas,
                         "tas_op" = data.bc.subset$tas)

data.bc.pr.list <- list("pr_refor" = data.bc.cross$pr,
                        "pr_op" = data.bc.subset$pr)

data.bc.petH.list <- list("petH_refor" = data.bc.cross$petH,
                          "petH_op" = data.bc.subset$petH)

data.bc.tas.list.bind <- do.call(transformeR::bindGrid, c(data.bc.tas.list, dimension = "time"))
data.bc.pr.list.bind <- do.call(transformeR::bindGrid, c(data.bc.pr.list, dimension = "time"))
data.bc.petH.list.bind <- do.call(transformeR::bindGrid, c(data.bc.petH.list, dimension = "time"))

data.bc.bind <- list("tas" = data.bc.tas.list.bind,
                     "pr" = data.bc.pr.list.bind,
                     "petH" = data.bc.petH.list.bind)

#any(is.na(data.bc.bind))
season <- 1:12
dir.Rdata <-  paste0(getwd(),"/data/")

# sort out NA introduced at location 9692... still don't know why the biasCorrection function introduces this error...
which(is.na(data.bc.bind$tas$Data)) # no NAs
which(is.na(data.bc.bind$petH$Data)) # no NAs
which(is.na(data.bc.bind$pr$Data)) # an NA TRUE 9692
data.bc.bind$pr$Data[9692]
obs.data$pr$Data[9692]
obs.data$pr$Dates$start[9692] # 14th July 2005...
data.bc.bind$pr$Dates$start[9692] # 14th July 2005...
data.bc.bind$pr$Data[9692] <- obs.data$pr$Data[9692] # replace NA with obs value

# # replace any zero or negative petH values (should incorporate this into bias correction loop as for pr, but not done yet)
 dat_names <- c("tas","pr","petH")
data.bc.bind <- readRDS(file = paste0(dir.Rdata, "ERA5_WATExR_", paste0(season, collapse = "_"), "_", paste0(dat_names, collapse = "_"), "_BC_op_Sp.rds"))
# which(data.bc.bindera5$petH$Data <= 0)
# data.bc.bindera5$petH$Data[which(data.bc.bindera5$petH$Data <= 0)]
# View(data.bc.bindera5$petH$Data) # identify smallest none zero petH value and replace zeros and negatives with this
# data.bc.bindera5$petH$Data <- ifelse(data.bc.bindera5$petH$Data <= 0, 0.07968624, data.bc.bindera5$petH$Data)
# data.bc.bind <- data.bc.bindera5

saveRDS(data.bc.bind, file = paste0(dir.Rdata, "ERA5_WATExR_", paste0(season, collapse = "_"), "_", paste0(names(data), collapse = "_"), "_BC_op_Sp.rds"))

# -------------------------------------------------------------------------------------------------------------- #
# BUILD FINAL DATA
# -------------------------------------------------------------------------------------------------------------- #

datatoexport <- data.bc.bind
dates <- datatoexport[[1]]$Dates

# extract the data arrays of all variables from the list
data <- lapply(datatoexport, function(x) x[["Data"]])

# Build data frame
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data)

# raw data
data_raw_exp <- lapply(data_op, function(x) x[["Data"]])

# Build data frame
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
df_raw <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data_raw_exp)

era5_reanalysis_raw <- df_raw[-2]
colnames(era5_reanalysis_raw) <-c("date","tas_era5_raw","pr_era5_raw","petH_era5_raw")

# remove negative pr
era5_reanalysis_raw$pr_era5_raw <- ifelse(era5_reanalysis_raw$pr_era5_raw < 0,
                                          0,
                                          era5_reanalysis_raw$pr_era5_raw)

era5_reanalysis_raw_Jan2021 <- era5_reanalysis_raw
usethis::use_data(era5_reanalysis_raw_Jan2021, overwrite = TRUE)

# Define metadata to generate the file name
# institution <- "MI"
# lake_id <- "Burrishoole"
# ClimateModelName <- "ERA5"
# ExperimentName <- "reanalysis"
# member <- "member01"
# freq <- "day"

# Create directory and save file
#startTime <- format(as.POSIXlt(yymmdd[1]), format = "%Y%m%d")
#endTime <- format(tail(as.POSIXlt(yymmdd), n = 1), format = "%Y%m%d")
#dirName <- paste0(dir.data, lake_id, "/CLIMATE/", lake_id, "_", institution, "_", ClimateModelName, "_", ExperimentName, "_", member, "_", freq, "_", startTime, "-", endTime, "/", sep = "", collapse = NULL)
dir.data <-  paste0(getwd(),'/inst/extdata/ERA5_bcc_op_Sp')
dirName <- paste0(dir.data, "/", sep = "", collapse = NULL)
dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")
write.table(df, paste0(dirName,"meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

era5_reanalysis_bcc_Jan2021 <- df[-2]
colnames(era5_reanalysis_bcc_Jan2021) <-c("date","tas_era5_bcc","pr_era5_bcc","petH_era5_bcc")
usethis::use_data(era5_reanalysis_bcc_Jan2021, overwrite = TRUE)
# ------------------------------------------------------------------------------------------ #





# ------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------------------ #
# --------------------------------------------------------------------------- #
# EEL (2020 here, but not necessarily complete; modified this script (mostly
# above) 2021-02-10 for salmon op year 2021)
# --------------------------------------------------------------------------- #
# for eel will be appending data following salmon forecasts, so Feb to Jun
# (again there will be an issue with expver following this method that we work
# around with separate requests, but will ultimately folow same style of method
# as for salmon (download requests separately for different expver periods))
# --------------------------------------------------------------------------- #
dataset_2020 <- paste0(getwd(),"/inst/extdata/ERA5_2020_Feb_Jun/download.nc")

# t2m and pr
variables <- c("t2m","tp") # for Jan or Feb to June 2020
#variables <- c("t2m@1","tp@1") # for all 2020 with expvar
aggr.fun <- c("mean","sum")
years <- 2020
#season <- 1 # for salmon
season <- 2:6 # for additional eel data
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)

#require(rJava) # need this for is.jnull function
di <- loadeR::dataInventory(dataset = dataset_2020)
dicName <- paste0(getwd(), "/inst/extdata/ERA5_ecmwf.dic")
list_era5_online <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_2020,
                                                                               var = variables[x],
                                                                               years = years,
                                                                               lonLim = lonLim,
                                                                               latLim = latLim,
                                                                               season = season,
                                                                               time = "DD",
                                                                               aggr.d = aggr.fun[x],
                                                                               dictionary = dicName))
names(list_era5_online) <- c("tas","pr")
list_era5_online$tas$Variable$varName <- "tas"
list_era5_online$pr$Variable$varName <- "pr"

############################################################################################
############### RUN THE FOLLOWING CODE CHUNK IF YOU NEED POTENTIAL EVAPOTRANSPIRATION ######
# Load needed variables
list_ncs_tmin <- list()
dicName <- paste0(getwd(), "/inst/extdata/ERA5_ecmwf.dic")
season <- 2:6
#season <- 1 # for salmon

lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
variables <- "t2m"
  list_ncs_tmin[[1]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_2020,
                                                                                     var = variables,
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "DD",
                                                                                     aggr.d = "min",
                                                                                     dictionary = dicName))
  names(list_ncs_tmin)[1] <- years
  names(list_ncs_tmin[[1]]) <- "t2m"

list_ncs_tmax <- list()
  list_ncs_tmax[[1]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_2020,
                                                                                     var = variables,
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "DD",
                                                                                     aggr.d = "max",
                                                                                     dictionary = dicName))
  names(list_ncs_tmax)[1] <- years
  names(list_ncs_tmax[[1]]) <- "t2m"

  # -------------------------------------------------------------------------------------------------------------- #
  # BIND petH GRIDS -----------------------------------------------------------------------------
  data.prelim_t2m_min <- sapply(list_ncs_tmin, "[", 1)[[1]]
  data.prelim_t2m_max <- sapply(list_ncs_tmax, "[", 1)[[1]]

  # bind grids by time
  #tasmin.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_min, dimension = "time"))
  #tasmax.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_max, dimension = "time"))

  # Compute potential evapotranspiration with function petGrid from package drought4R
  # For daily data the implemented method is hargreaves-samani (See ?petGrid for details)
  # petGrid function requires temperature in celsius. Convert temperature units to celsius (if required).
  transformeR::getGridUnits(data.prelim_t2m_min)
  transformeR::getGridUnits(data.prelim_t2m_max) # no units, so set them

  str(data.prelim_t2m_min) # check location of stored units...
  attr(data.prelim_t2m_min$Variable, "units") <- "celsius"
  attr(data.prelim_t2m_max$Variable, "units") <- "celsius"

  #tasmax <- convertR::udConvertGrid(data.prelim_t2m_max, new.units = "celsius")
  #tasmin <- convertR::udConvertGrid(data.prelim_t2m_min, new.units = "celsius")
  petH <- drought4R::petGrid(tasmin = data.prelim_t2m_min,
                             tasmax = data.prelim_t2m_max,
                             method = "hargreaves-samani")




  # bilinear interpolation
  lake <- list(x = -9.578, y = 53.973) # Burrishoole

  petH.interp <- transformeR::interpGrid(petH, new.coordinates = lake, method = "bilinear", bilin.method = "akima")
  petH.interp$Variable$varName <- "petH"

  # -------------------------------------------------------------------------------------------------------------- #

###################### END OF THE CHUNK ####################################################
############################################################################################

# downscale tas and pr data to local coords
lake <- list(x = -9.578, y = 53.973) # Burrishoole
data.interp <- lapply(list_era5_online, function(x) transformeR::interpGrid(x, new.coordinates = lake,
                                                                       method = "bilinear",
                                                                       bilin.method = "akima"))
# bind tas and pr to petH
# Jan
ERA5.data_Jan <- c(data.interp, "petH" = list(petH.interp))

# Feb to June
ERA5.data_Feb_June <- c(data.interp, "petH" = list(petH.interp))
#

# load raw hindcast
ERA5raw <- readRDS(paste0(getwd(),"/data/ERA5_WATExR_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_petH_raw.rds"))

# append op data (Jan 2020 and Feb to June 2020 if required for eels)
#ERA5op_tas_list <- list(ERA5.data_Jan$tas,ERA5raw$tas)
#ERA5op_pr_list <- list(ERA5.data_Jan$pr,ERA5raw$pr)
#ERA5op_petH_list <- list(ERA5.data_Jan$petH,ERA5raw$petH)

ERA5op_tas_list <- list(ERA5.data_Jan$tas,ERA5.data_Feb_June$tas,ERA5raw$tas)
ERA5op_pr_list <- list(ERA5.data_Jan$pr,ERA5.data_Feb_June$pr,ERA5raw$pr)
ERA5op_petH_list <- list(ERA5.data_Jan$petH,ERA5.data_Feb_June$petH,ERA5raw$petH)

# bind grids by time
ERA5_operational_tas <- do.call(transformeR::bindGrid, c(ERA5op_tas_list, dimension = "time"))
ERA5_operational_pr <- do.call(transformeR::bindGrid, c(ERA5op_pr_list, dimension = "time"))
ERA5_operational_petH <- do.call(transformeR::bindGrid, c(ERA5op_petH_list, dimension = "time"))

# place back in list
ERA5_operational <-list("tas" = ERA5_operational_tas,
                        "pr" = ERA5_operational_pr,
                        "petH" = ERA5_operational_petH)

# save as rds file...Autumn
saveRDS(ERA5_operational,
        file = paste0(getwd(),"/data/ERA5_WATExR_tas_pr_petH_raw_op_2020_Au.rds"))

# save as rds file...Spring
saveRDS(ERA5_operational,
        file = paste0(getwd(),"/data/ERA5_WATExR_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_petH_raw_op.rds"))
#test <- readRDS(file = paste0(getwd(),"/data/ERA5_WATExR_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_raw_op.rds"))
# bias correct

# -------------------------------------------------------------------------------------------------------------- #
# BIAS CORRECTION ----------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------- #
# aut
data_op <- readRDS(file = paste0(getwd(),"/data/ERA5_WATExR_tas_pr_petH_raw_op_2020_Au.rds"))
# spr
#data_op <- readRDS(file = paste0(getwd(),"/data/ERA5_WATExR_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_petH_raw_op.rds"))
# up to january? Does it matter?
#any(is.na(data_op$pr$Data)) # no nas in reanalysis data
obs.data <- readRDS(paste0(getwd(),"/data/Furnace_Station_1979_2019_1_2_3_4_5_6_7_8_9_10_11_12_tas_pr_petH.rds"))
#any(is.na(obs.data$pr$Data)) # no nas in obs data
# Subset observational data to the same dates as forecast data for bias correction calibration
obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data_op[[x]], type = "temporal", which.return = 1)})
data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data_op[[x]], type = "temporal", which.return = 2)})
varnames <- c("tas", "pr","petH")
names(obs.data) <- varnames
names(data) <- varnames
#data_raw <- data
# Collect some common metadata (e.g. from variable uas)
dates <- data[[1]]$Dates
xycoords <- transformeR::getCoordinates(data[[1]])

# Bias correction with leave-one-year-out ("loo") cross-validation type
# ?biasCorrection in R for more info about the parameter settings for bias
# correction. # old version downscale r 3.1.3, not install 3.3.1

data.bc.cross <- lapply(1:length(data), function(x)  {
  precip <- FALSE
  if (names(data)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data[[x]],
                             x = data[[x]],
                             method = "eqm",
                             cross.val = "loo",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             window = c(31, 7))
})
names(data.bc.cross) <- names(data)
#which(is.na(data.bc.cross$pr$Data)) # row 9692 of pr is na (why?)
# dont use loo for operational year (it doesn't work)! It is automatic with
# "newdata" argument, but retain cross validated version for reforecast period to
# be representative of operational forecast.
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1")
#with downscaleR@v3.3.1
# Error in arr[, , , ind, , ] <- grid[["Data"]] :
# number of items to replace is not a multiple of replacement length, so install old versions again
#devtools::install_github("SantanderMetGroup/transformeR@v1.7.4") # was 2.0.1
#devtools::install_github("SantanderMetGroup/downscaleR@v3.1.3") # was 3.3.1

data.bc <- lapply(1:length(data), function(x)  {
  precip <- FALSE
  if (names(data)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data[[x]],
                             x = data[[x]],
                             newdata = data_op[[x]],
                             extrapolation = "constant",
                             method = "eqm",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             window = c(31, 7),)
})
names(data.bc) <- names(data)

#                 window = c(90, 31),

# -------------------------------------------------------------------------------------------------------------- #
# Append (loo CV) bias corrected reforecast to bias corrected operational year (e.g., 2020) --------------------
# -------------------------------------------------------------------------------------------------------------- #

# subset operational year -------
data.bc.subset <- lapply(data.bc, function(x) transformeR::subsetGrid(grid = x,
                                                                      years = 2020,
                                                                      season = 1:6))

# bind grids -----
data.bc.tas.list <- list("tas_refor" = data.bc.cross$tas,
                         "tas_op" = data.bc.subset$tas)

data.bc.pr.list <- list("pr_refor" = data.bc.cross$pr,
                         "pr_op" = data.bc.subset$pr)

data.bc.petH.list <- list("petH_refor" = data.bc.cross$petH,
                        "petH_op" = data.bc.subset$petH)

data.bc.tas.list.bind <- do.call(transformeR::bindGrid, c(data.bc.tas.list, dimension = "time"))
data.bc.pr.list.bind <- do.call(transformeR::bindGrid, c(data.bc.pr.list, dimension = "time"))
data.bc.petH.list.bind <- do.call(transformeR::bindGrid, c(data.bc.petH.list, dimension = "time"))

data.bc.bind <- list("tas" = data.bc.tas.list.bind,
                     "pr" = data.bc.pr.list.bind,
                     "petH" = data.bc.petH.list.bind)

#any(is.na(data.bc.bind))
season <- 1:12
dir.Rdata <-  paste0(getwd(),"/data/")

# sort out NA introduced at location 9692... still don't know why the biasCorrection function introduces this error...
which(is.na(data.bc.bind$tas$Data)) # no NAs
which(is.na(data.bc.bind$petH$Data)) # no NAs
which(is.na(data.bc.bind$pr$Data)) # an NA TRUE
data.bc.bind$pr$Data[9692]
obs.data$pr$Data[9692]
obs.data$pr$Dates$start[9692] # 14th July 2005...
data.bc.bind$pr$Dates$start[9692] # 14th July 2005...
data.bc.bind$pr$Data[9692] <- obs.data$pr$Data[9692] # replace NA with obs value
saveRDS(data.bc.bind, file = paste0(dir.Rdata, "ERA5_WATExR_", paste0(season, collapse = "_"), "_", paste0(names(data), collapse = "_"), "_BC_op_Au.rds"))

# -------------------------------------------------------------------------------------------------------------- #
# BUILD FINAL DATA
# -------------------------------------------------------------------------------------------------------------- #

datatoexport <- data.bc.bind
dates <- datatoexport[[1]]$Dates

# extract the data arrays of all variables from the list
data <- lapply(datatoexport, function(x) x[["Data"]])

# Build data frame
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data)

# raw data
data_raw_exp <- lapply(data_op, function(x) x[["Data"]])

# Build data frame
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
df_raw <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data_raw_exp)

era5_reanalysis_raw <- df_raw[-2]
colnames(era5_reanalysis_raw) <-c("date","tas_era5_raw","pr_era5_raw","petH_era5_raw")

# remove negative pr
era5_reanalysis_raw$pr_era5_raw <- ifelse(era5_reanalysis_raw$pr_era5_raw < 0,
                                          0,
                                          era5_reanalysis_raw$pr_era5_raw)

era5_reanalysis_raw_June2020 <- era5_reanalysis_raw
usethis::use_data(era5_reanalysis_raw_June2020, overwrite = TRUE)

# Define metadata to generate the file name
# institution <- "MI"
# lake_id <- "Burrishoole"
# ClimateModelName <- "ERA5"
# ExperimentName <- "reanalysis"
# member <- "member01"
# freq <- "day"

# Create directory and save file
#startTime <- format(as.POSIXlt(yymmdd[1]), format = "%Y%m%d")
#endTime <- format(tail(as.POSIXlt(yymmdd), n = 1), format = "%Y%m%d")
#dirName <- paste0(dir.data, lake_id, "/CLIMATE/", lake_id, "_", institution, "_", ClimateModelName, "_", ExperimentName, "_", member, "_", freq, "_", startTime, "-", endTime, "/", sep = "", collapse = NULL)
dir.data <-  paste0(getwd(),'/inst/extdata/ERA5_bcc_op_Au')
dirName <- paste0(dir.data, "/", sep = "", collapse = NULL)
dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")
write.table(df, paste0(dirName,"meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

era5_reanalysis_bcc_June2020 <- df[-2]
colnames(era5_reanalysis_bcc_June2020) <-c("date","tas_era5_bcc","pr_era5_bcc","petH_era5_bcc")
usethis::use_data(era5_reanalysis_bcc_June2020, overwrite = TRUE)
