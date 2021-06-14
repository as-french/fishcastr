# ------------------------------------------------------------------------------------------------------- #
# A script for downloading ECMWF SEAS5 reforecast data July - December 1993 to 2016
# and archived operational forecast 2017 to 2019 for retrospective skill
# assessment
# ------------------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------------------#
# Install required packages. RUN JUST THE FIRST TIME.
# ----------------------------------------------------------------------------------------------------------#
# devtools::install_github(c("SantanderMetGroup/loadeR.java", "SantanderMetGroup/loadeR@devel",
#                            "SantanderMetGroup/transformeR", "SantanderMetGroup/loadeR.ECOMS",
#                            "SantanderMetGroup/visualizeR", "SantanderMetGroup/convertR",
#                            "SantanderMetGroup/drought4R@devel", "SantanderMetGroup/downscaleR@devel"))
#
# devtools::install_github(c("SantanderMetGroup/loadeR"))
# install.packages("rJava",INSTALL_opts = --no-multiarch) # might not need nomultiarch argument

# devtools::install_github("SantanderMetGroup/loadeR", INSTALL_opts=c("--no-multiarch"))
# ----------------------------------------------------------------------------------------------------------#
# Set java params
# ----------------------------------------------------------------------------------------------------------#
 options(java.parameters = "-Xmx8000m")

# ----------------------------------------------------------------------------------------------------------#
# GENERAL SETTINGS THAT NEED TO BE DEFINED:
# ----------------------------------------------------------------------------------------------------------#

# Define the geographical domain to be loaded
lonLim <- c(-10,-9)
latLim <- c(53,54)

# Define the coordinates and name of the lake
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# Define the re-forecast dataset
dataset <- "http://meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_Seasonal_25Members_SFC.ncml"

# Login in the TAP-UDG the climate4R libraries
# More details about UDG in https://doi.org/10.1016/j.cliser.2017.07.001
#loadeR::loginUDG("xxxxxx", "xxxxxxxxxx")
climate4R.UDG::loginUDG("xxxxxx", "xxxxxxxxxx")

# Check available variables in the dataset (SEAS5)
di_ref <- loadeR::dataInventory(dataset)
#names(di_ref)

# ----------------------------------------------------------------------------------------------------------#

# ----------------------------------------------------------------------------------------------------------#
# DATA LOADING AND TRANSFORMATION ----
# ----------------------------------------------------------------------------------------------------------#
# AUTUMN
# ----------------------------------------------------------------------------------------------------------#
variables <- c("tas", "tp")
aggr.fun <- c( "mean", "sum")
mem <- 1:25
lead.month <- 0
years <- 1993:2016
season <- c(7,8,9,10,11,12) # Autumn during run (make this 7 months)

cdsNcML <- "http://meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_Seasonal_25Members_SFC.ncml"
dicName <- paste0(system.file("", package = "fishcastr"), "/extdata/SYSTEM5_ecmwf_Seasonal_25Members_SFC.dic")

# test lapply version
data.S5.autumn <- lapply(1:length(variables), function (x) loadeR::loadSeasonalForecast(cdsNcML,
                                                                                        var = variables[x],
                                                                                        dictionary = dicName,
                                                                                        members = mem,
                                                                                        lonLim = lonLim,
                                                                                        latLim = latLim,
                                                                                        season = season,
                                                                                        years = years,
                                                                                        leadMonth = lead.month,
                                                                                        time = "DD",
                                                                                        aggr.d = aggr.fun[x],
                                                                                        aggr.m = "none"))
names(data.S5.autumn) <- c("tas","pr")

# Bilinear interpolation of the data to the location of the lake
grid_SEAS5_reforecast_1993_2016_tas_pr <- lapply(data.S5.autumn, function(x) transformeR::interpGrid(x, new.coordinates = lake,
                                                                                 method = "bilinear",
                                                                                 bilin.method = "akima"))
names(grid_SEAS5_reforecast_1993_2016_tas_pr) <- c("tas", "pr")
grid_SEAS5_reforecast_1993_2016_tas_pr$pr$Variable$varName <- "pr"

############################################################################################
############### RUN THE FOLLOWING CODE CHUNK IF YOU NEED POTENTIAL EVAPOTRANSPIRATION ######
# Load needed variables
tasmin <- loadeR::loadSeasonalForecast(cdsNcML, var = "tas", years = years,dictionary = dicName,
                                       members = mem,leadMonth = lead.month, aggr.m = "none",
                                       lonLim = lonLim, latLim = latLim,
                                       season = season,  time = "DD", aggr.d = "min")
tasmax <- loadeR::loadSeasonalForecast(dataset, var = "tas", years = years,dictionary = dicName,
                                       members = mem,leadMonth = lead.month, aggr.m = "none",
                                       lonLim = lonLim, latLim = latLim,
                                       season = season,  time = "DD", aggr.d = "max")

# Compute potential evapotranspiration with function petGrid from package drought4R
# For daily data the implemented method is hargreaves-samani (See ?petGrid for details):
petH <- drought4R::petGrid(tasmin = tasmin,
                           tasmax = tasmax,
                           method = "hargreaves-samani")

# bilinear interpolation
petH.interp <- transformeR::interpGrid(petH, new.coordinates = lake, method = "bilinear", bilin.method = "akima")
petH.interp$Variable$varName <- "petH"

# Put all variables together
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw <- c(grid_SEAS5_reforecast_1993_2016_tas_pr,
                                                        "petH" = list(petH.interp))

# round data
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas$Data <- round(grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas$Data,2)
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$pr$Data <- round(grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$pr$Data,2)
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$petH$Data <- round(grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$petH$Data,2)

# convert initialisation dates into list for each variable and member...
InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates})
names(InitializationDates) <- grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas$Members
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates <- InitializationDates
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$pr$InitializationDates <- InitializationDates
grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$petH$InitializationDates <- InitializationDates

# export the re-forecasts 1993 to 2016
dirName <- paste0(system.file("", package = "fishcastr"), "/extdata/SEAS5_archive_1993_2019", sep = "", collapse = NULL)
dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")

saveRDS(object = grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw,file = paste0(dirName,"/grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw.rds"))
# ----------------------------------------------------------------------------------------------------------#

# ----------------------------------------------------------------------------------------------------------#
# DOWNLOAD ARCHIVED OPERATIONAL FORECASTS 2017 to 2019 ----
# ----------------------------------------------------------------------------------------------------------#

variables <- c("tas", "tp")
aggr.fun <- c("mean", "sum")
mem <- 1:25
lead.month <- 0
years <- 2017:2019
season <- c(7,8,9,10,11,12) # Autumn during run

dicName <- paste0(system.file("", package = "fishcastr"), "/extdata/SYSTEM5_ecmwf_Seasonal_25Members_SFC.dic")
cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_forecast_Seasonal_51Members_SFC.ncml"
#diop <- loadeR::dataInventory(cdsNcML)

data.S5.autumn <- lapply(1:length(variables), function (x) loadeR::loadSeasonalForecast(cdsNcML,
                                                                                        var = variables[x],
                                                                                        dictionary = dicName,
                                                                                        members = mem,
                                                                                        lonLim = lonLim,
                                                                                        latLim = latLim,
                                                                                        season = season,
                                                                                        years = years,
                                                                                        leadMonth = lead.month,
                                                                                        time = "DD",
                                                                                        aggr.d = aggr.fun[x],
                                                                                        aggr.m = "none"))
names(data.S5.autumn) <- variables

# Bilinear interpolation of the data to the location of the lake
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr <- lapply(data.S5.autumn, function(x) transformeR::interpGrid(x, new.coordinates = lake,
                                                                                 method = "bilinear",
                                                                                 bilin.method = "akima"))

#change names to match obs data...
names(grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr) <- c("tas", "pr")
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr$pr$Variable$varName <- "pr"

# ----------------------------------------------------------------------------------------------------------#
# POTENTIAL EVAPOTRANSPIRATION ----
# ----------------------------------------------------------------------------------------------------------#
tasmin <- loadeR::loadSeasonalForecast(cdsNcML, var = "tas", years = years,dictionary = dicName,
                                       members = mem,leadMonth = lead.month, aggr.m = "none",
                                       lonLim = lonLim, latLim = latLim,
                                       season = season,  time = "DD", aggr.d = "min")
tasmax <- loadeR::loadSeasonalForecast(cdsNcML, var = "tas", years = years,dictionary = dicName,
                                       members = mem,leadMonth = lead.month, aggr.m = "none",
                                       lonLim = lonLim, latLim = latLim,
                                       season = season,  time = "DD", aggr.d = "max")

data_tas_min_max_SEAS5 <- list("tas_min" = tasmin,"tas_max" = tasmax)

data_tas_min_max_SEAS5dir <- paste0(system.file("", package = "fishcastr"),"/extdata/SF_Autumn_op_raw/S5_seas_25_7_8_9_10_11_12_tas_min_tas_max.rds")
grid_SEAS5_2017_2020_7_8_9_10_11_12_tasmin_tasmax <- readRDS(data_tas_min_max_SEAS5dir)
grid_SEAS5_2017_2019_7_8_9_10_11_12_tasmin_tasmax <- lapply(grid_SEAS5_2017_2020_7_8_9_10_11_12_tasmin_tasmax,
                                                           function(x) transformeR::subsetGrid(grid = x,
                                                                                               years = 2017:2019))
tasmin <- grid_SEAS5_2017_2019_7_8_9_10_11_12_tasmin_tasmax$tas_min
tasmax <- grid_SEAS5_2017_2019_7_8_9_10_11_12_tasmin_tasmax$tas_max

# Compute potential evapotranspiration with function petGrid from package drought4R
# For daily data the implemented method is hargreaves-samani (See ?petGrid for details):
petH <- drought4R::petGrid(tasmin = tasmin,
                           tasmax = tasmax,
                           method = "hargreaves-samani")

# bilinear interpolation
petH.interp <- transformeR::interpGrid(petH, new.coordinates = lake, method = "bilinear", bilin.method = "akima")
petH.interp$Variable$varName <- "petH"

# Put all variables together
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw <- c(grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr,
                                                        "petH" = list(petH.interp))

# round data
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$Data <- round(grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$Data,2)
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$pr$Data <- round(grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$pr$Data,2)
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$petH$Data <- round(grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$petH$Data,2)

# convert initialisation dates into list for each variable and member...
InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates})
names(InitializationDates) <- grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$Members
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates <- InitializationDates
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$pr$InitializationDates <- InitializationDates
grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$petH$InitializationDates <- InitializationDates

# export the archive operational forecasts 2017 to 2019
saveRDS(object = grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw,file = paste0(dirName,"/grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw.rds"))

# ----------------------------------------------------------------------------------------------------------#
# BIND RE-FORECAST AND ARCHIVED OPERATIONAL GRIDS TOGETHER ----
# ----------------------------------------------------------------------------------------------------------#

# extract t2m and tp grids for all years and combine them into a list
data.prelim_tas <- list("hindcast_data" = grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas,
                        "operational_data" = grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas)
data.prelim_pr <- list("hindcast_data" = grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$pr,
                       "operational_data" = grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$pr)
data.prelim_petH <- list("hindcast_data" = grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$petH,
                         "operational_data" = grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$petH)

# bind grids by time
data.prelim_tas_bind <- do.call(transformeR::bindGrid, c(data.prelim_tas, dimension = "time"))
data.prelim_pr_bind <- do.call(transformeR::bindGrid, c(data.prelim_pr, dimension = "time"))
data.prelim_petH_bind <- do.call(transformeR::bindGrid, c(data.prelim_petH, dimension = "time"))

# check binding looks ok
#visualizeR::temporalPlot(... = data.prelim_tas_bind)

# compile multiple variables to single list to be bias corrected
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw <- list("tas" = data.prelim_tas_bind,
                                                           "pr" = data.prelim_pr_bind,
                                                           "petH" = data.prelim_petH_bind)

# check these initalisation dates are not already list format...
#init_dates_seas.for.data
#init_dates_seas.op.data

# sort out initialization dates...
InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- c(grid_SEAS5_1993_2016_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates$Member_1,
                                                                               grid_SEAS5_2017_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates$Member_1)})
names(InitializationDates) <- grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$Members
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw$tas$InitializationDates <- InitializationDates
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw$pr$InitializationDates <- InitializationDates
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw$petH$InitializationDates <- InitializationDates

#grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw <- readRDS(paste0(system.file("", package = "fishcastr"),"/data/grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw.rds"))

usethis::use_data(grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw, overwrite = TRUE)
