# ------------------------------------------------------------------------------------------------- #
# # A script for stitching together pre-downloaded netcdf files obtained from
# Copernicus climate data store:
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels?tab=overview
# Note that data formats vary depending on date on which download was carried
# out. The script below illustrates the use of dictionaries for loading grid
# ERA5 data into R using the climate4R package bundle. While the extdata nc
# files last date is 31 dec 2019, the exported data here are subsetted to end of
# January 2019 to reflect operational conditions.
# ------------------------------------------------------------------------------------------------- #

# Install required packages. RUN JUST THE FIRST TIME.
# #easy
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

####### GENERAL SETTINGS THAT NEED TO BE DEFINED IN EACH CASE STUDY ---------------------------------

# Output path where the data will be saved (change to your local path).
dir.data <- paste0(
  system.file("inst", package = "fishcastr"),
  "/extdata/ECMWF_ERA5"
)

# -------------------------------------------------------------------------------------------------------------- #
# LOAD ERA 5 REANALYSIS .nc FILES (1980 - 2018) ----
# -------------------------------------------------------------------------------------------------------------- #
dataset_list <- list.files(path = dir.data, pattern = NULL, all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

list_years <- list(c(1979:1984),
                   c(1985:1990),
                   c(1991:1996),
                   c(1997:2002),
                   c(2003:2008),
                   c(2009:2014),
                   c(2015:2018),
                   c(2019))

list_ncs <- list()

dicName <- paste0(
  system.file("inst", package = "fishcastr"),
  "/extdata/ERA5_ecmwf.dic"
)

variables <- c("t2m","tp")
aggr.fun <- c("mean","sum")
season <- 1:12
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
library(rJava)
for(i in 1:length(list_years)){
  years <- list_years[[i]]
  list_ncs[[i]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_list[i],
                                                                                var = variables[x],
                                                                                years = years,
                                                                                lonLim = lonLim,
                                                                                latLim = latLim,
                                                                                season = season,
                                                                                time = "DD",
                                                                                aggr.d = aggr.fun[x],
                                                                                dictionary = dicName))
  names(list_ncs)[i] <- paste(years,collapse = "_")
  names(list_ncs[[i]]) <- variables
}

############################################################################################
############### RUN THE FOLLOWING CODE CHUNK IF YOU NEED POTENTIAL EVAPOTRANSPIRATION ######
# Load needed variables
list_ncs_tmin <- list()
season <- 1:12
lonLim <- c(-10,-9.5)
latLim <- c(53.8,54.2)
variables <- "t2m"
for(i in 1:length(list_years)){
  years <- list_years[[i]]
  list_ncs_tmin[[i]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_list[i],
                                                                                     var = variables,
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "DD",
                                                                                     aggr.d = "min",
                                                                                     dictionary = dicName))
  names(list_ncs_tmin)[i] <- paste(years,collapse = "_")
  names(list_ncs_tmin[[i]]) <- variables
}

list_ncs_tmax <- list()
for(i in 1:length(list_years)){
  years <- list_years[[i]]
  list_ncs_tmax[[i]] <- lapply(1:length(variables), function(x) loadeR::loadGridData(dataset_list[i],
                                                                                     var = variables,
                                                                                     years = years,
                                                                                     lonLim = lonLim,
                                                                                     latLim = latLim,
                                                                                     season = season,
                                                                                     time = "DD",
                                                                                     aggr.d = "max",
                                                                                     dictionary = dicName))
  names(list_ncs_tmax)[i] <- paste(years,collapse = "_")
  names(list_ncs_tmax[[i]]) <- variables
}
###################### END OF THE CHUNK ####################################################
############################################################################################

# --------------------------------------------------------------------------------------------------------- # BIND petH GRIDS ----
# --------------------------------------------------------------------------------------------------------- #
data.prelim_t2m_min <- sapply(list_ncs_tmin, "[", 1)
data.prelim_t2m_max <- sapply(list_ncs_tmax, "[", 1)

# bind grids by time
tasmin.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_min, dimension = "time"))
tasmax.list.bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m_max, dimension = "time"))

# Compute potential evapotranspiration with function petGrid from package drought4R
# For daily data the implemented method is hargreaves-samani (See ?petGrid for details)
# petGrid function requires temperature in celsius. Convert temperature units to celsius (if required).
transformeR::getGridUnits(tasmax.list.bind)
transformeR::getGridUnits(tasmin.list.bind) # no units, so set them

#str(tasmax.list.bind) # check location of stored units...
attr(tasmin.list.bind$Variable, "units") <- "celsius"
attr(tasmax.list.bind$Variable, "units") <- "celsius"

#tasmax <- convertR::udConvertGrid(tasmax.list.bind, new.units = "celsius")
#tasmin <- convertR::udConvertGrid(tasmin.list.bind, new.units = "celsius")
petH <- drought4R::petGrid(tasmin = tasmin.list.bind,
                           tasmax = tasmax.list.bind,
                           method = "hargreaves-samani")

# bilinear interpolation
lake <- list(x = -9.578, y = 53.973) # Burrishoole

petH.interp <- transformeR::interpGrid(petH, new.coordinates = lake,
                                       method = "bilinear", bilin.method = "akima")
petH.interp$Variable$varName <- "petH"

# -------------------------------------------------------------------------------------------------------------- #

# -------------------------------------------------------------------------------------------------------------- #
# BIND t2m and tp GRIDS
# -------------------------------------------------------------------------------------------------------------- #

# extract t2m and tp grids for all years and compile them into a list
data.prelim_t2m <- sapply(list_ncs, "[", 1)
data.prelim_tp <- sapply(list_ncs, "[", 2)

# bind grids by time
data.prelim_t2m_bind <- do.call(transformeR::bindGrid, c(data.prelim_t2m, dimension = "time"))
data.prelim_tp_bind <- do.call(transformeR::bindGrid, c(data.prelim_tp, dimension = "time"))

# compile multiple variables to single list to be interpolated
data.prelim <- list("t2m" = data.prelim_t2m_bind, "tp" = data.prelim_tp_bind)

# -------------------------------------------------------------------------------------------------------------- #
# INTERPOLATE GRIDS TO POINT COORDINATES OF LOCAL MET STATION
# -------------------------------------------------------------------------------------------------------------- #
lake <- list(x = -9.578, y = 53.973) # Burrishoole
data.interp <- lapply(data.prelim, function(x) transformeR::interpGrid(x, new.coordinates = lake,
                                                                       method = "bilinear",
                                                                       bilin.method = "akima"))
# check data by plotting
# plot(1:length(data.interp$t2m$Data), data.interp$t2m$Data, type = "p")
# range(data.interp$t2m$Dates$start)

# -------------------------------------------------------------------------------------------------------------- #
# Put all variables together in single list
#data <- c(data.interp, "petH" = list(petH.interp))
# -------------------------------------------------------------------------------------------------------------- #
# SAVE ERA5 1979 - 2019 RAW...Rdata and dat
# -------------------------------------------------------------------------------------------------------------- #
ERA5.data <- c(data.interp, "petH" = list(petH.interp))
#rename vars to match system5 and local obs
names(ERA5.data) <- c("tas", "pr", "petH")
ERA5.data$tas$Variable$varName <- "tas"
ERA5.data$pr$Variable$varName <- "pr"
ERA5.data$petH$Variable$varName <- "petH"

ERA5.data$tas$Data <- round(ERA5.data$tas$Data,2)
ERA5.data$pr$Data <- round(ERA5.data$pr$Data,2)
ERA5.data$petH$Data <- round(ERA5.data$petH$Data,2)

grid_ERA5_1979_2019_Jun_raw <- ERA5.data
#usethis::use_data(grid_ERA5_1979_2019_Jun_raw, overwrite = TRUE)

# subset to Jan 2019
grid_ERA5_subset_1979_2018 <- lapply(grid_ERA5_1979_2019_Jun_raw,
                                     function(x) transformeR::subsetGrid(x,
                                                                         years = 1979:2018))
grid_ERA5_subset_jun_2019 <- lapply(grid_ERA5_1979_2019_Jun_raw,
                                    function(x) transformeR::subsetGrid(x,
                                                                        years = 2019,
                                                                        season = 1:6))
# bind grids
vars <- c("tas","pr","petH")
grid_ERA5_1979_2019_Jun_raw <- lapply(vars,function(x){
  transformeR::bindGrid(grid_ERA5_subset_1979_2018[[x]],
                        grid_ERA5_subset_jun_2019[[x]],
                        dimension = "time")
})
names(grid_ERA5_1979_2019_Jun_raw) <- vars

years <- range(lubridate::year(grid_ERA5_1979_2019_Jun_raw$tas$Dates$start))

saveRDS(grid_ERA5_1979_2019_Jun_raw,
        file = paste0(
          system.file("inst", package = "fishcastr"),
          "/extdata/grid_ERA5_",years[1],"_",years[2],
          "_Jun_raw_CDS.rds"
        ))
