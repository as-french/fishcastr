# -------------------------------------------------------------------------------------------------- #
# This script is used to download pseudo meteorological observations from the EWEMBI dataset.
# EWEMBI data can be downloaded directly from
# https://esg.pik-potsdam.de/search/isimip/ as .nc files, or downloaded using the
# climate4R bundle maintained by the university of Santander Met Group (as below).

# Before downloading from the Santander servers, it is necessary to register with
# the User Data Gateway. https://meteo.unican.es/trac/wiki/udg/registration
# Following registration, the new user will have automatic access to
# "PUBLIC_DATA" (see here for list of public datasets:
# http://meteo.unican.es/tds5/catalogs/public.html).

# -------------------------------------------------------------------------------------------------- #
# INSTALL REQUIRED PACKAGES. RUN JUST THE FIRST TIME.
# -------------------------------------------------------------------------------------------------- #
# # usually easy to install
# devtools::install_github(c("SantanderMetGroup/transformeR",
#                            "SantanderMetGroup/visualizeR", "SantanderMetGroup/convertR",
#                            "SantanderMetGroup/drought4R", "SantanderMetGroup/downscaleR"))
# # don't always install easily...
# devtools::install_github(c("SantanderMetGroup/climate4R.UDG",
#                            "SantanderMetGroup/loadeR",
#                            "SantanderMetGroup/loadeR.java",
#                            "SantanderMetGroup/loadeR.ECOMS"))

# Load packages.
# library(loadeR)
# library(transformeR)
# library(loadeR.ECOMS)
# library(visualizeR)
# library(convertR)
# library(drought4R)

# -------------------------------------------------------------------------------------------------- #
# SET OUTPUT PATH
# -------------------------------------------------------------------------------------------------- #
# This defines where the .dat files will be archived - parent of r package to
# keep out of git. This ensures static data are not stored in github.

arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ISIMIP_EWEMBI")
dir.create(arc_dir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

#arc_dir <- paste0(sub("/[^/]+$", "", getwd()),"/climate_data/")

# -------------------------------------------------------------------------------------------------- #
# DOWNLOAD EWEMBI DATA ----
# -------------------------------------------------------------------------------------------------- #
# Define the geographical domain to be loaded
lonLim <- c(-11,-8)
latLim <- c(52,55)

# Define the coordinates and name of the catchment
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# Define the period and the season
years <- 1979:2016
season <- 1:12 #Full year

# Define the dataset
dataset <- "PIK_Obs-EWEMBI"

# Login in the TAP-UDG the climate4R libraries
# More details about UDG in https://doi.org/10.1016/j.cliser.2017.07.001
climate4R.UDG::loginUDG("xxxxxx", "xxxxxxxxxx")

# Define the variables to be loaded. Remove those not needed.
variables <- c("tas", "pr")

# -------------------------------------------------------------------------------------------------- #
# DATA LOADING AND TRANSFORMATION ----
# -------------------------------------------------------------------------------------------------- #
# Load observations (EWEMBI) with function loadGridData from package loadeR.
# Data is loaded in a loop (function lapply) to load all variables in a single code line.
# A list of grids is obtained, each slot in the list corresponds to a variable
library(rJava)
data.prelim <- lapply(variables, function(x) loadeR::loadGridData(dataset, var = x, years = years,
                                                                  lonLim = lonLim, latLim = latLim,
                                                                  season = season))
# Approximately 8 minutes to download

names(data.prelim) <- variables

# Bilinear interpolation of the data to the location of the lake. See ?interpGrid for other methods.
data.interp <- lapply(data.prelim, function(x) transformeR::interpGrid(x,
                                                                       new.coordinates = lake,
                                                                       method = "bilinear",
                                                                       bilin.method = "akima"))

# -------------------------------------------------------------------------------------------------- #
# RUN THE FOLLOWING CODE FOR POTENTIAL EVAPOTRANSPIRATION ----
# -------------------------------------------------------------------------------------------------- #
# Load needed variables
tasmin <- loadeR::loadGridData(dataset, var = "tasmin", years = years,
                               lonLim = lonLim, latLim = latLim,
                               season = season,  time = "DD", aggr.d = "min")
tasmax <- loadeR::loadGridData(dataset, var = "tasmax", years = years,
                               lonLim = lonLim, latLim = latLim,
                               season = season,  time = "DD", aggr.d = "max")

# Also approximately 8 minutes to download the above two variables

# Compute potential evapotranspiration with function petGrid from package drought4R
# For daily data the implemented method is hargreaves-samani (See ?petGrid for details):
petH <- drought4R::petGrid(tasmin = tasmin,
                           tasmax = tasmax,
                           method = "hargreaves-samani")

# bilinear interpolation
# petH
petH.interp <- transformeR::interpGrid(petH, new.coordinates = lake, method = "bilinear", bilin.method = "akima")
petH.interp$Variable$varName <- "petH"
# tasmin
tasmin.interp <- transformeR::interpGrid(tasmin, new.coordinates = lake, method = "bilinear", bilin.method = "akima")
tasmin.interp$Variable$varName <- "tasmin"
# tasmax
tasmax.interp <- transformeR::interpGrid(tasmax, new.coordinates = lake, method = "bilinear", bilin.method = "akima")
tasmax.interp$Variable$varName <- "tasmax"

# Put all variables together
data <- c(data.interp, "tasmin" = list(tasmin.interp), "tasmax" = list(tasmax.interp), "petH" = list(petH.interp))
dates <- data[[1]][["Dates"]]

###################### END OF THE CHUNK ####################################################
############################################################################################

grid_ewembi_1979_2016 <- data
grid_ewembi_1979_2016_round <- grid_ewembi_1979_2016
# round to 2 decimal places
grid_ewembi_1979_2016_round$tas$Data <- round(grid_ewembi_1979_2016_round$tas$Data,2)
grid_ewembi_1979_2016_round$pr$Data <- round(grid_ewembi_1979_2016_round$pr$Data,2)
grid_ewembi_1979_2016_round$tasmin$Data <- round(grid_ewembi_1979_2016_round$tasmin$Data,2)
grid_ewembi_1979_2016_round$tasmax$Data <- round(grid_ewembi_1979_2016_round$tasmax$Data,2)
grid_ewembi_1979_2016_round$petH$Data <- round(grid_ewembi_1979_2016_round$petH$Data,2)

grid_ewembi_1979_2016 <- grid_ewembi_1979_2016_round
# save Rdata for gap filling of manual station data prior to bias correction or
# directly for posterior bias correction of seasonal forecasts SEAS5 and
# reanalysis ERA Interim
#usethis::use_data(grid_ewembi_1979_2016, overwrite = TRUE)

# store downloaded and processed data as .rds file
saveRDS(object = grid_ewembi_1979_2016, file = paste0(system.file("inst", package = "fishcastr"),
                                                   "/extdata/ISIMIP_EWEMBI/grid_ewembi_1979_2016_UC.rds"))

########## BUILD FINAL DATA --------------------------------------------------------------
data <- grid_ewembi_1979_2016
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
ClimateModelName <- "EWEMBI_UC"
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
  saveRDS(object = data_ewembi_1979_2016, file = paste0(system.file("inst", package = "fishcastr"),
                                                        "/extdata/ISIMIP_EWEMBI/data_ewembi_1979_2016_UC.rds"))
