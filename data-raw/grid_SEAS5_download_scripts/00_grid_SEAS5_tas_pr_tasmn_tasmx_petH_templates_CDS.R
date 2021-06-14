# ----------------------------------------------------------------------------------------------------------#
# A script building grid templates for merging downloaded (netcdf) seasonal
# climate forecasts from the Copernicus climate data store (CDS).

# Not all files downloaded from the Copernicus CDS are in grid format (e.g.,
# ERA5 is in grid format, which enables use of the climate4R bundle of R
# packages; whereas SEAS5 is not in grid format) - it is possible to edit what R
# reads by creating ncml files that specify coordinates types etc, which
# effectively converts the nc data into climate4R readable grid format (this is
# actually what the University of Cantabria met group do to facilitate climate
# data access through their User Data Gateway). However, for users who are
# unfamiliar with ncml, we aimed to create an empty grid template, and write a
# function that populates this empty template using data downloaded directly
# from the CDS. In so doing, we seek to bypass the need for using a netcdf
# editor outside the R environment. This script represents the first step in
# this process (i.e., to build empty grid templates for seasonal climate
# forecasts initialised in February (for salmonids) and July (for eels). The
# second step in this process is included as the function
# fishcastr::read_nc_to_grid().
# ----------------------------------------------------------------------------------------------------------#

################################################################################
# For users interested in editing nc files using ncml...
# we can alter ncml using:
# http://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/edu/ucar/toolsUI/4.6.8/toolsUI-4.6.8.jar
# - This java application facilitates writing of ncml files to ensure data are
# in grid format that is readable by the climate4R tools.

# Example:
# For SEAS5 data the edit involves changing the following lines to specify a
# _CoordinateAxisType attribute i.e., open the application, click on the ncml
# tab, save the ncml and change the following lines:
# <ncml:variable name="number" shape="number" type="int">
#   <ncml:attribute name="long_name" value="ensemble_member" />
#
#   to :
#
# <ncml:variable name="number" shape="number" type="int">
#   <ncml:attribute name="long_name" value="ensemble_member" />
#   <ncml:attribute name="_CoordinateAxisType" value="Ensemble" />
# now save the ncml and check in the FeatureTypes tab if the data are now in grid
# format (they should be!)
################################################################################

# ---------------------------------------------------------------------------------------------- #
# THIS IS HOW THE TEMPLATE GRID WAS CREATED (following the java application steps above) ----
# Note - Two templates are created (one for leap years and one for non-leap years)
# ---------------------------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------------------------- #
# NON-LEAP YEAR - Feb - Aug ----
# ---------------------------------------------------------------------------------------------- #

lonLim <- c(-10,-9)
latLim <- c(53,54)

# Define the coordinates and name of the lake
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# SPRING
variables <- c("t2m")
mem <- 1:25
lead.month <- 0
years <- 1993
season <- c(2,3,4,5,6,7,8) # Spring during run (make this 7 months)
#agg.fun <- c("mean")
#tim <- c("DD")
dicName <- paste0(getwd(), "/extdata/SYSTEM5_ecmwf_Seasonal_25Members_SFC_online.dic")

# ---------------------------------------------------------------------------------------------- #
# 2M TEMPERATURE (mean)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)

# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "none"))
names(data.template_prelim) <- "tas"
data.template_prelim$tas$Variable$varName <- "tas"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$tas$Data),
                   NA,
                   data.template_prelim$tas$Data)

# check attribute order
#attributes(data.template_prelim$tas$Data)
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tas$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$tas$Data)

# sub in NA array into template grid
data.template_prelim$tas$Data <- NA_array

# set members
data.template_prelim$tas$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tas$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tas$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tas_non_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# PRECIPITATION (use t2m but aggregate to daily sum as precipitation data does not read)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di <- loadeR::dataInventory(cdsNcML)

variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "sum"))

# change various attibutes
names(data.template_prelim) <- "pr" # note change name here
data.template_prelim$pr$Variable$varName <- "pr"
#attributes(data.template_prelim$pr$Variable)
attr(data.template_prelim$pr$Variable,"description") <- "Total precipitation"
attr(data.template_prelim$pr$Variable,"units") <- "m"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$pr$Data),
                   NA,
                   data.template_prelim$pr$Data)

# check attribute order
#attributes(data.template_prelim$pr$Data)
# $dim [1]  25 212   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"

# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$pr$Data)

# sub in NA array into template grid
data.template_prelim$pr$Data <- NA_array

# set members
data.template_prelim$pr$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$pr$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$pr$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tp_non_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# TASMIN
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "min"))
names(data.template_prelim) <- "tasmin"
data.template_prelim$tasmin$Variable$varName <- "tasmin"

# replace data values in data array with NAs
NA_array_tasmin <- ifelse(!is.na(data.template_prelim$tasmin$Data),
                   NA,
                   data.template_prelim$tasmin$Data)

# check attribute order
#attributes(data.template_prelim$tasmin$Data)
attr(data.template_prelim$tasmin$Variable,"description") <- "2 metre minimum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmin$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmin) <- attributes(data.template_prelim$tasmin$Data)

# sub in NA array into template grid
#data.template_prelim$tasmin$Data <- NA_array_tasmin

# set members
data.template_prelim$tasmin$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmin$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmin$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_non_leap_year <- data.template_prelim


# ---------------------------------------------------------------------------------------------- #
# TASMAX
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "max"))
names(data.template_prelim) <- "tasmax"
data.template_prelim$tasmax$Variable$varName <- "tasmax"

# replace data values in data array with NAs
NA_array_tasmax <- ifelse(!is.na(data.template_prelim$tasmax$Data),
                   NA,
                   data.template_prelim$tasmax$Data)

# check attribute order
#attributes(data.template_prelim$tasmax$Data)
attr(data.template_prelim$tasmax$Variable,"description") <- "2 metre maximum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmax$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmax) <- attributes(data.template_prelim$tasmax$Data)

# sub in NA array into template grid
#data.template_prelim$tasmax$Data <- NA_array_tasmax

# set members
data.template_prelim$tasmax$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmax$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmax$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_non_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# POTENTIAL EVAPOTRANSPIRATION
# ---------------------------------------------------------------------------------------------- #

grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year <-
  drought4R::petGrid(tasmin = grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_non_leap_year$tasmin,
                     tasmax = grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_non_leap_year$tasmax,
                     method = "hargreaves-samani")

# remove calculated petH values from data array
 NA_array_petH <-
   ifelse(
     !is.na(grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year$Data),
     NA,
     grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year$Data
   )
 attributes(NA_array_petH) <- attributes(grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year$Data)
 grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year$Data <- NA_array_petH
 grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year$Variable$varName <- "petH"

 # remove data from tasmin and max
grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_non_leap_year$tasmin$Data <- NA_array_tasmin
grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_non_leap_year$tasmax$Data <- NA_array_tasmax

grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_non_leap_year <-
  list("tas" = grid_SEAS5_template_2_3_4_5_6_7_8_tas_non_leap_year$tas,
       "pr" = grid_SEAS5_template_2_3_4_5_6_7_8_tp_non_leap_year$pr,
       "tasmin" = grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_non_leap_year$tasmin,
       "tasmax" = grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_non_leap_year$tasmax,
       "petH" = grid_SEAS5_template_2_3_4_5_6_7_8_petH_non_leap_year)

usethis::use_data(object = grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_non_leap_year,
                  overwrite = TRUE)

# ---------------------------------------------------------------------------------------------- #
# LEAP YEAR - Feb - Aug ----
# ---------------------------------------------------------------------------------------------- #
lonLim <- c(-10,-9)
latLim <- c(53,54)

# Define the coordinates and name of the lake
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# SPRING
variables <- c("t2m")
mem <- 1:25
lead.month <- 0
years <- 1996
season <- c(2,3,4,5,6,7,8) # Spring during run (make this 7 months)
agg.fun <- c("mean")
tim <- c("DD")
dicName <- paste0(getwd(), "/extdata/SYSTEM5_ecmwf_Seasonal_25Members_SFC_online.dic")

# ---------------------------------------------------------------------------------------------- #
# 2M TEMPERATURE (mean)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)

# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "none"))
names(data.template_prelim) <- "tas"
data.template_prelim$tas$Variable$varName <- "tas"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$tas$Data),
                   NA,
                   data.template_prelim$tas$Data)

# check attribute order
#attributes(data.template_prelim$tas$Data)
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tas$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$tas$Data)

# sub in NA array into template grid
data.template_prelim$tas$Data <- NA_array

# set members
data.template_prelim$tas$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tas$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tas$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tas_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# PRECIPITATION (use t2m but aggregate to daily sum as precipitation data does not read)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di <- loadeR::dataInventory(cdsNcML)

variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "sum"))

# change various attibutes
names(data.template_prelim) <- "pr" # note change name here
data.template_prelim$pr$Variable$varName <- "pr"
#attributes(data.template_prelim$pr$Variable)
attr(data.template_prelim$pr$Variable,"description") <- "Total precipitation"
attr(data.template_prelim$pr$Variable,"units") <- "m"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$pr$Data),
                   NA,
                   data.template_prelim$pr$Data)

# check attribute order
#attributes(data.template_prelim$pr$Data)
# $dim [1]  25 212   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"

# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$pr$Data)

# sub in NA array into template grid
data.template_prelim$pr$Data <- NA_array

# set members
data.template_prelim$pr$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$pr$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$pr$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tp_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# TASMIN
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "min"))
names(data.template_prelim) <- "tasmin"
data.template_prelim$tasmin$Variable$varName <- "tasmin"

# replace data values in data array with NAs
NA_array_tasmin <- ifelse(!is.na(data.template_prelim$tasmin$Data),
                           NA,
                           data.template_prelim$tasmin$Data)

# check attribute order
#attributes(data.template_prelim$tasmin$Data)
attr(data.template_prelim$tasmin$Variable,"description") <- "2 metre minimum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmin$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmin) <- attributes(data.template_prelim$tasmin$Data)

# sub in NA array into template grid
#data.template_prelim$tasmin$Data <- NA_array_tasmin

# set members
data.template_prelim$tasmin$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmin$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmin$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_leap_year <- data.template_prelim


# ---------------------------------------------------------------------------------------------- #
# TASMAX
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-02-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "max"))
names(data.template_prelim) <- "tasmax"
data.template_prelim$tasmax$Variable$varName <- "tasmax"

# replace data values in data array with NAs
NA_array_tasmax <- ifelse(!is.na(data.template_prelim$tasmax$Data),
                          NA,
                          data.template_prelim$tasmax$Data)

# check attribute order
#attributes(data.template_prelim$tasmax$Data)
attr(data.template_prelim$tasmax$Variable,"description") <- "2 metre maximum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmax$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmax) <- attributes(data.template_prelim$tasmax$Data)

# sub in NA array into template grid
#data.template_prelim$tasmax$Data <- NA_array_tasmax

# set members
data.template_prelim$tasmax$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmax$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmax$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# POTENTIAL EVAPOTRANSPIRATION
# ---------------------------------------------------------------------------------------------- #

grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year <-
  drought4R::petGrid(tasmin = grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_leap_year$tasmin,
                     tasmax = grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_leap_year$tasmax,
                     method = "hargreaves-samani")

# remove calculated petH values from data array
NA_array_petH <-
  ifelse(
    !is.na(grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year$Data),
    NA,
    grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year$Data
  )
attributes(NA_array_petH) <- attributes(grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year$Data)
grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year$Data <- NA_array_petH
grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year$Variable$varName <- "petH"

# remove data from tasmin and max
grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_leap_year$tasmin$Data <- NA_array_tasmin
grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_leap_year$tasmax$Data <- NA_array_tasmax

grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_leap_year <-
  list("tas" = grid_SEAS5_template_2_3_4_5_6_7_8_tas_leap_year$tas,
       "pr" = grid_SEAS5_template_2_3_4_5_6_7_8_tp_leap_year$pr,
       "tasmin" = grid_SEAS5_template_2_3_4_5_6_7_8_tasmin_leap_year$tasmin,
       "tasmax" = grid_SEAS5_template_2_3_4_5_6_7_8_tasmax_leap_year$tasmax,
       "petH" = grid_SEAS5_template_2_3_4_5_6_7_8_petH_leap_year)

usethis::use_data(object = grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_leap_year,
                  overwrite = TRUE)


# ---------------------------------------------------------------------------------------------- #
# NON-LEAP YEAR - Jul - Jan ----
# ---------------------------------------------------------------------------------------------- #

lonLim <- c(-10,-9)
latLim <- c(53,54)

# Define the coordinates and name of the lake
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# SPRING
variables <- c("t2m")
mem <- 1:25
lead.month <- 0
years <- 1993
season <- c(7,8,9,10,11,12) # Autumn during run (make this 7 months)
#agg.fun <- c("mean")
#tim <- c("DD")
dicName <- paste0(getwd(), "/extdata/SYSTEM5_ecmwf_Seasonal_25Members_SFC_online.dic")

# ---------------------------------------------------------------------------------------------- #
# 2M TEMPERATURE (mean)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)

# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "none"))
names(data.template_prelim) <- "tas"
data.template_prelim$tas$Variable$varName <- "tas"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$tas$Data),
                   NA,
                   data.template_prelim$tas$Data)

# check attribute order
#attributes(data.template_prelim$tas$Data)
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tas$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$tas$Data)

# sub in NA array into template grid
data.template_prelim$tas$Data <- NA_array

# set members
data.template_prelim$tas$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tas$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tas$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tas_non_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# PRECIPITATION (use t2m but aggregate to daily sum as precipitation data does not read)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di <- loadeR::dataInventory(cdsNcML)

variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "sum"))

# change various attibutes
names(data.template_prelim) <- "pr" # note change name here
data.template_prelim$pr$Variable$varName <- "pr"
#attributes(data.template_prelim$pr$Variable)
attr(data.template_prelim$pr$Variable,"description") <- "Total precipitation"
attr(data.template_prelim$pr$Variable,"units") <- "m"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$pr$Data),
                   NA,
                   data.template_prelim$pr$Data)

# check attribute order
#attributes(data.template_prelim$pr$Data)
# $dim [1]  25 212   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"

# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$pr$Data)

# sub in NA array into template grid
data.template_prelim$pr$Data <- NA_array

# set members
data.template_prelim$pr$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$pr$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$pr$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tp_non_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# TASMIN
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "min"))
names(data.template_prelim) <- "tasmin"
data.template_prelim$tasmin$Variable$varName <- "tasmin"

# replace data values in data array with NAs
NA_array_tasmin <- ifelse(!is.na(data.template_prelim$tasmin$Data),
                           NA,
                           data.template_prelim$tasmin$Data)

# check attribute order
#attributes(data.template_prelim$tasmin$Data)
attr(data.template_prelim$tasmin$Variable,"description") <- "2 metre minimum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmin$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmin) <- attributes(data.template_prelim$tasmin$Data)

# sub in NA array into template grid
#data.template_prelim$tasmin$Data <- NA_array_tasmin

# set members
data.template_prelim$tasmin$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmin$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmin$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tasmin_non_leap_year <- data.template_prelim


# ---------------------------------------------------------------------------------------------- #
# TASMAX
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1993-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "max"))
names(data.template_prelim) <- "tasmax"
data.template_prelim$tasmax$Variable$varName <- "tasmax"

# replace data values in data array with NAs
NA_array_tasmax <- ifelse(!is.na(data.template_prelim$tasmax$Data),
                          NA,
                          data.template_prelim$tasmax$Data)

# check attribute order
#attributes(data.template_prelim$tasmax$Data)
attr(data.template_prelim$tasmax$Variable,"description") <- "2 metre maximum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmax$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmax) <- attributes(data.template_prelim$tasmax$Data)

# sub in NA array into template grid
#data.template_prelim$tasmax$Data <- NA_array_tasmax

# set members
data.template_prelim$tasmax$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmax$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmax$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tasmax_non_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# POTENTIAL EVAPOTRANSPIRATION
# ---------------------------------------------------------------------------------------------- #

grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year <-
  drought4R::petGrid(tasmin = grid_SEAS5_template_7_8_9_10_11_12_tasmin_non_leap_year$tasmin,
                     tasmax = grid_SEAS5_template_7_8_9_10_11_12_tasmax_non_leap_year$tasmax,
                     method = "hargreaves-samani")

# remove calculated petH values from data array
NA_array_petH <-
  ifelse(
    !is.na(grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year$Data),
    NA,
    grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year$Data
  )
attributes(NA_array_petH) <- attributes(grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year$Data)
grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year$Data <- NA_array_petH
grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year$Variable$varName <- "petH"

# remove data from tasmin and max
grid_SEAS5_template_7_8_9_10_11_12_tasmin_non_leap_year$tasmin$Data <- NA_array_tasmin
grid_SEAS5_template_7_8_9_10_11_12_tasmax_non_leap_year$tasmax$Data <- NA_array_tasmax

grid_SEAS5_template_7_8_9_10_11_12_tas_pr_tasmin_tasmax_petH_non_leap_year <-
  list("tas" = grid_SEAS5_template_7_8_9_10_11_12_tas_non_leap_year$tas,
       "pr" = grid_SEAS5_template_7_8_9_10_11_12_tp_non_leap_year$pr,
       "tasmin" = grid_SEAS5_template_7_8_9_10_11_12_tasmin_non_leap_year$tasmin,
       "tasmax" = grid_SEAS5_template_7_8_9_10_11_12_tasmax_non_leap_year$tasmax,
       "petH" = grid_SEAS5_template_7_8_9_10_11_12_petH_non_leap_year)

usethis::use_data(object = grid_SEAS5_template_7_8_9_10_11_12_tas_pr_tasmin_tasmax_petH_non_leap_year,
                  overwrite = TRUE)

# ---------------------------------------------------------------------------------------------- #
# LEAP YEAR - Jul - Jan ----
# ---------------------------------------------------------------------------------------------- #

lonLim <- c(-10,-9)
latLim <- c(53,54)

# Define the coordinates and name of the lake
lake <- list(x = -9.578, y = 53.973) # Burrishoole
lakename <- "Burrishoole"

# SPRING
variables <- c("t2m")
mem <- 1:25
lead.month <- 0
years <- 1996
season <- c(7,8,9,10,11,12) # Autumn during run (make this 7 months)
#agg.fun <- c("mean")
#tim <- c("DD")
dicName <- paste0(getwd(), "/extdata/SYSTEM5_ecmwf_Seasonal_25Members_SFC_online.dic")

# ---------------------------------------------------------------------------------------------- #
# 2M TEMPERATURE (mean)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)

# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "none"))
names(data.template_prelim) <- "tas"
data.template_prelim$tas$Variable$varName <- "tas"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$tas$Data),
                   NA,
                   data.template_prelim$tas$Data)

# check attribute order
#attributes(data.template_prelim$tas$Data)
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tas$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$tas$Data)

# sub in NA array into template grid
data.template_prelim$tas$Data <- NA_array

# set members
data.template_prelim$tas$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tas$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tas$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tas_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# PRECIPITATION (use t2m but aggregate to daily sum as precipitation data does not read)
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di <- loadeR::dataInventory(cdsNcML)

variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "sum"))

# change various attibutes
names(data.template_prelim) <- "pr" # note change name here
data.template_prelim$pr$Variable$varName <- "pr"
#attributes(data.template_prelim$pr$Variable)
attr(data.template_prelim$pr$Variable,"description") <- "Total precipitation"
attr(data.template_prelim$pr$Variable,"units") <- "m"

# replace data values in data array with NAs
NA_array <- ifelse(!is.na(data.template_prelim$pr$Data),
                   NA,
                   data.template_prelim$pr$Data)

# check attribute order
#attributes(data.template_prelim$pr$Data)
# $dim [1]  25 212   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"

# set attributes of NA array to match original data
attributes(NA_array) <- attributes(data.template_prelim$pr$Data)

# sub in NA array into template grid
data.template_prelim$pr$Data <- NA_array

# set members
data.template_prelim$pr$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$pr$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$pr$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tp_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# TASMIN
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "min"))
names(data.template_prelim) <- "tasmin"
data.template_prelim$tasmin$Variable$varName <- "tasmin"

# replace data values in data array with NAs
NA_array_tasmin <- ifelse(!is.na(data.template_prelim$tasmin$Data),
                          NA,
                          data.template_prelim$tasmin$Data)

# check attribute order
#attributes(data.template_prelim$tasmin$Data)
attr(data.template_prelim$tasmin$Variable,"description") <- "2 metre minimum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmin$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmin) <- attributes(data.template_prelim$tasmin$Data)

# sub in NA array into template grid
#data.template_prelim$tasmin$Data <- NA_array_tasmin

# set members
data.template_prelim$tasmin$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmin$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmin$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tasmin_leap_year <- data.template_prelim


# ---------------------------------------------------------------------------------------------- #
# TASMAX
# ---------------------------------------------------------------------------------------------- #
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_SEAS5/download_eel_t2m/download_eel_t2m_jul_unzip")

# downloaded Copernicus CDS file with modified ncml
cdsNcML <- paste0(arc_dir, "/date_1996-07-01.ncml")

#cdsNcML <- "http://data.meteo.unican.es/tds5/dodsC/cfsrr/CFSv2_Seasonal.ncml"
library(rJava)
#di_t2m <- loadeR::dataInventory(cdsNcML)
variables <- c("t2m")
# test lapply version
data.template_prelim <- lapply(1:length(variables),
                               function (x) loadeR::loadGridData(cdsNcML,
                                                                 var = variables[x],
                                                                 dictionary = dicName,
                                                                 members = mem,
                                                                 lonLim = lonLim,
                                                                 latLim = latLim,
                                                                 season = season,
                                                                 years = years,
                                                                 time = "DD",
                                                                 aggr.d = "max"))
names(data.template_prelim) <- "tasmax"
data.template_prelim$tasmax$Variable$varName <- "tasmax"

# replace data values in data array with NAs
NA_array_tasmax <- ifelse(!is.na(data.template_prelim$tasmax$Data),
                          NA,
                          data.template_prelim$tasmax$Data)

# check attribute order
#attributes(data.template_prelim$tasmax$Data)
attr(data.template_prelim$tasmax$Variable,"description") <- "2 metre maximum temperature"
# $dim [1]  25 847   2   2
# $dimensions [1] "member" "time"   "lat"    "lon"
attr(data.template_prelim$tasmax$Variable,"units") <- "celsius"
# set attributes of NA array to match original data
attributes(NA_array_tasmax) <- attributes(data.template_prelim$tasmax$Data)

# sub in NA array into template grid
#data.template_prelim$tasmax$Data <- NA_array_tasmax

# set members
data.template_prelim$tasmax$Members <- paste0("Member_",1:25)

# set initialization dates
initialization_dates <- min(data.template_prelim$tasmax$Dates$start)
initialization_dates_list <- lapply(1:25,FUN = function(x){initialization_dates})
names(initialization_dates_list) <- paste0("Member_",1:25)
data.template_prelim$tasmax$InitializationDates <- initialization_dates_list

# export (non-leap year) template
grid_SEAS5_template_7_8_9_10_11_12_tasmax_leap_year <- data.template_prelim

# ---------------------------------------------------------------------------------------------- #
# POTENTIAL EVAPOTRANSPIRATION
# ---------------------------------------------------------------------------------------------- #

grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year <-
  drought4R::petGrid(tasmin = grid_SEAS5_template_7_8_9_10_11_12_tasmin_leap_year$tasmin,
                     tasmax = grid_SEAS5_template_7_8_9_10_11_12_tasmax_leap_year$tasmax,
                     method = "hargreaves-samani")

# remove calculated petH values from data array
NA_array_petH <-
  ifelse(
    !is.na(grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year$Data),
    NA,
    grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year$Data
  )
attributes(NA_array_petH) <- attributes(grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year$Data)
grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year$Data <- NA_array_petH
grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year$Variable$varName <- "petH"

# remove data from tasmin and max
grid_SEAS5_template_7_8_9_10_11_12_tasmin_leap_year$tasmin$Data <- NA_array_tasmin
grid_SEAS5_template_7_8_9_10_11_12_tasmax_leap_year$tasmax$Data <- NA_array_tasmax

grid_SEAS5_template_7_8_9_10_11_12_tas_pr_tasmin_tasmax_petH_leap_year <-
  list("tas" = grid_SEAS5_template_7_8_9_10_11_12_tas_leap_year$tas,
       "pr" = grid_SEAS5_template_7_8_9_10_11_12_tp_leap_year$pr,
       "tasmin" = grid_SEAS5_template_7_8_9_10_11_12_tasmin_leap_year$tasmin,
       "tasmax" = grid_SEAS5_template_7_8_9_10_11_12_tasmax_leap_year$tasmax,
       "petH" = grid_SEAS5_template_7_8_9_10_11_12_petH_leap_year)

usethis::use_data(object = grid_SEAS5_template_7_8_9_10_11_12_tas_pr_tasmin_tasmax_petH_leap_year,
                  overwrite = TRUE)
