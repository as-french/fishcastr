# -------------------------------------------------------------------------------------------------- #
# As script to load gridded data from Potsdam institute for climate impact
# research. This script can be run to download all 1979 - 2016 EWEMBI data,
# calculate additional variables (i.e., mean air temperature and potential
# evapotranspiration by the Hargreaves-Samani method)
# See https://esg.pik-potsdam.de/search/isimip/ for more details about ISIMIP
# and EWEMBI. these data were published under the CC By 4.0 license.
# -------------------------------------------------------------------------------------------------- #

# Firstly, define all consistent parameters...

# Define the geographical domain to be loaded
lonLim <- c(-11,-8)
latLim <- c(52,55)

# Define the coordinates and name of the catchment, these coordinates will be used to interpolate the data
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# Define the period and the season
#years <- 1979:1980

years <- list(c(1979:1980),
              c(1981:1990),
              c(1991:2000),
              c(2001:2010),
              c(2011:2016))

season <- 1:12 #Full year

# could in theory create an ncml file for this, instead of list

# TASMIN
potsNc_tasmin <- list(c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmin/daily/v20180626/tasmin_ewembi1_daily_1979_1980.nc"),
               c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmin/daily/v20180626/tasmin_ewembi1_daily_1981_1990.nc"),
               c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmin/daily/v20180626/tasmin_ewembi1_daily_1991_2000.nc"),
               c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmin/daily/v20180626/tasmin_ewembi1_daily_2001_2010.nc"),
               c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmin/daily/v20180626/tasmin_ewembi1_daily_2011_2016.nc"))

# TASMAX
potsNc_tasmax <- list(c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmax/daily/v20180626/tasmax_ewembi1_daily_1979_1980.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmax/daily/v20180626/tasmax_ewembi1_daily_1981_1990.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmax/daily/v20180626/tasmax_ewembi1_daily_1991_2000.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmax/daily/v20180626/tasmax_ewembi1_daily_2001_2010.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tasmax/daily/v20180626/tasmax_ewembi1_daily_2011_2016.nc"))

# TAS MEAN
potsNc_tas <- list(c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tas/daily/v20180626/tas_ewembi1_daily_1979_1980.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tas/daily/v20180626/tas_ewembi1_daily_1981_1990.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tas/daily/v20180626/tas_ewembi1_daily_1991_2000.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tas/daily/v20180626/tas_ewembi1_daily_2001_2010.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/tas/daily/v20180626/tas_ewembi1_daily_2011_2016.nc"))

# TOTAL PRECIP
potsNc_pr <- list(c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/pr/daily/v20180626/pr_ewembi1_daily_1979_1980.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/pr/daily/v20180626/pr_ewembi1_daily_1981_1990.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/pr/daily/v20180626/pr_ewembi1_daily_1991_2000.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/pr/daily/v20180626/pr_ewembi1_daily_2001_2010.nc"),
                       c("https://esg.pik-potsdam.de/thredds/dodsC/isimip_dataroot_2/isimip2b/input_secondary/clim_atm_obs/EWEMBI/historical/pr/daily/v20180626/pr_ewembi1_daily_2011_2016.nc"))

# more info at:
# https://esg.pik-potsdam.de/thredds/catalog/esgcet/577/isimip2b.input_secondary.clim_atm_obs.EWEMBI.historical.tasmin.daily.v20180626.html?dataset=isimip2b.input_secondary.clim_atm_obs.EWEMBI.historical.tasmin.daily.v20180626.tasmin_ewembi1_daily_1979_1980.nc

#di <- loadeR::dataInventory(potsNc_tas)

dicName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/EWEMBI_isimip.dic")

# Define the variables to be loaded. Remove those not needed.
#variables <- c("tasmin")

# create list of lists for nested lapply

#potsNc_list <- list("tas" = potsNc_tas,
#                    "pr" = potsNc_pr,
#                    "tasmin" = potsNc_tas_min,
#                    "tasmax" = potsNc_tas_max)

# -------------------------------------------------------------------------------------------------- #
# DATA LOADING AND TRANSFORMATION ----
# -------------------------------------------------------------------------------------------------- #
# Load observations (EWEMBI) with function loadGridData from package loadeR.
# Data is loaded in a loop (function lapply) to load all years in a single code line.
# A list of grids is obtained, each slot in the list corresponds to a variable
library(rJava)

# -------------------------------------------------------------------------------------------------- #
# TAS
data.prelim_tas <- lapply(1:5, function(x) loadeR::loadGridData(potsNc_tas[[x]],
                                                                var = "tas",
                                                                years = years[[x]],
                                                                  lonLim = lonLim, latLim = latLim,
                                                                  season = season,
                                                                  aggr.d = "mean",
                                                                  dictionary = dicName))

# bind the grids by time
data.prelim_tas.bind <- transformeR::bindGrid(data.prelim_tas,
                                              dimension = "time")

# -------------------------------------------------------------------------------------------------- #
# PR
data.prelim_pr <- lapply(1:5, function(x) loadeR::loadGridData(potsNc_pr[[x]],
                                                                var = "pr",
                                                                years = years[[x]],
                                                                lonLim = lonLim, latLim = latLim,
                                                                season = season,
                                                                aggr.d = "sum",
                                                                dictionary = dicName))

# bind the grids by time
data.prelim_pr.bind <- transformeR::bindGrid(data.prelim_pr,
                                              dimension = "time")

# -------------------------------------------------------------------------------------------------- #
# TASMIN
data.prelim_tasmin <- lapply(1:5, function(x) loadeR::loadGridData(potsNc_tasmin[[x]],
                                                                var = "tasmin",
                                                                years = years[[x]],
                                                                lonLim = lonLim, latLim = latLim,
                                                                season = season,
                                                                aggr.d = "mean",
                                                                dictionary = dicName))

# bind the grids by time
data.prelim_tasmin.bind <- transformeR::bindGrid(data.prelim_tasmin,
                                              dimension = "time")

# -------------------------------------------------------------------------------------------------- #
# TASMAX
data.prelim_tasmax <- lapply(1:5, function(x) loadeR::loadGridData(potsNc_tasmax[[x]],
                                                                var = "tasmax",
                                                                years = years[[x]],
                                                                lonLim = lonLim, latLim = latLim,
                                                                season = season,
                                                                aggr.d = "mean",
                                                                dictionary = dicName))

# bind the grids by time
data.prelim_tasmax.bind <- transformeR::bindGrid(data.prelim_tasmax,
                                              dimension = "time")

# -------------------------------------------------------------------------------------------------- #
# CALCULATE PETH
petH <- drought4R::petGrid(tasmin = data.prelim_tasmin.bind,
                           tasmax = data.prelim_tasmax.bind,
                           method = "hargreaves-samani")

# -------------------------------------------------------------------------------------------------- #
# MAKE LIST OF VARIABLE GRIDS
data.prelim <- list("tas" = data.prelim_tas.bind,
                    "pr" = data.prelim_pr.bind,
                    "tasmin" = data.prelim_tasmin.bind,
                    "tasmax" = data.prelim_tasmax.bind,
                    "petH" = petH)

# -------------------------------------------------------------------------------------------------- #
# EXPORT AS RAW GRIDS as .rds
arc_dir <- paste0(system.file("extdata", package = "fishcastr"),
                 "/ISIMIP_EWEMBI/")
dir.create(arc_dir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

saveRDS(object = data.prelim,
        file = paste0(system.file("extdata", package = "fishcastr"),
                      "/ISIMIP_EWEMBI/EWEMBI_grid_potsdam.rds"))

# -------------------------------------------------------------------------------------------------- #
# INTERPOLATE GRIDS
data.interp <- lapply(data.prelim, function(x) transformeR::interpGrid(x,
                                                                       new.coordinates = lake,
                                                                       method = "bilinear",
                                                                       bilin.method = "akima"))

# compare to unican version?
ewembi_unican <- fishcastr::grid_ewembi_1979_2016


grid_ewembi_1979_2016_round <- data.interp

# round data and export to rds
#grid_ewembi_1979_2016 <-
# round to 2 decimal places
grid_ewembi_1979_2016_round$tas$Data <- round(grid_ewembi_1979_2016_round$tas$Data,2)
grid_ewembi_1979_2016_round$pr$Data <- round(grid_ewembi_1979_2016_round$pr$Data,2)
grid_ewembi_1979_2016_round$tasmin$Data <- round(grid_ewembi_1979_2016_round$tasmin$Data,2)
grid_ewembi_1979_2016_round$tasmax$Data <- round(grid_ewembi_1979_2016_round$tasmax$Data,2)
grid_ewembi_1979_2016_round$petH$Data <- round(grid_ewembi_1979_2016_round$petH$Data,2)

grid_ewembi_1979_2016 <- grid_ewembi_1979_2016_round

saveRDS(object = grid_ewembi_1979_2016,
        file = paste0(system.file("extdata", package = "fishcastr"),
                      "/grid_ewembi_1979_2016.rds"))

grid_ewembi_1979_2016 <- readRDS(file = paste0(system.file("extdata", package = "fishcastr"),
                                                "/grid_ewembi_1979_2016.rds"))

########## BUILD FINAL DATA --------------------------------------------------------------
data <- grid_ewembi_1979_2016
dates <- data[[1]][["Dates"]]

#data <- data.interp
# extract the data arrays of all variables from the list
data <- lapply(data, function(x) x[["Data"]])

# Build data frame
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data)

########### EXPORT DATA ACCORDING TO THE WATExR ARCHIVE DESIGN -----------------------------
# Define metadata to generate the file name
institution <- "MI"
lake_id <- "Burrishoole"
ClimateModelName <- "EWEMBI_potsdam"
ExperimentName <- "obs"
member <- "mem1"
freq <- "day"

# Create directory and save file
startTime <- format(as.POSIXlt(yymmdd[1]), format = "%Y%m%d")
endTime <- format(tail(as.POSIXlt(yymmdd), n = 1), format = "%Y%m%d")
dirName <- paste0(arc_dir,"/",ClimateModelName,"/", sep = "", collapse = NULL)
dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")
write.table(df, paste0(dirName,"meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# export table as rds
colnames(df) <- c("date","time","tas_ewembi", "pr_ewmebi","tasmin_ewembi","tasmax_ewembi","petH_ewembi")
df$date <- as.Date(df$date)
df <- df[,-2]
data_ewembi_1979_2016 <- df
#usethis::use_data(data_ewembi_1979_2016, overwrite = TRUE)
saveRDS(object = data_ewembi_1979_2016, file = paste0(system.file("extdata", package = "fishcastr"),
                                                      "/data_ewembi_1979_2016.rds"))
