# ----------------------------------------------------------------------------------------------------------
# This script is used to fill gaps in local Newport Furnace, Met Eireann,
# Ireland manual met station air temperature data (for tas, tas_min and tas_max)
# with auto station data and EWEMBI gridded obs and linear interpolation where
# the first two options are not possible. The data are subsequently used for
# calculation of "observed" potential evapotranspiration (using the
# Hargreaves-Samani method) and bias correction of reanalysis and seasonal
# climate forecasts.
# ----------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------- #
# IMPORT MANUAL MET STATION DATA ----
# ---------------------------------------------------------------------------------------------------------- #
#data_met_manual_station_1959_2019 <- fishcastr::data_met_manual_station_1959_2019

data_met_manual_station_1959_2019 <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),
                                                           "/extdata/data_met_manual_station_1959_2019.rds"))

# subset to 1979
met_man_1979_2019 <- data_met_manual_station_1959_2019[data_met_manual_station_1959_2019$date >= as.Date("1979-01-01"),]
rm(data_met_manual_station_1959_2019)

# total nas rain
sum(is.na(met_man_1979_2019$pr_manual)) # 0
# total nas air temp
sum(is.na(met_man_1979_2019$tas_manual)) # 794
sum(is.na(met_man_1979_2019$tas_min_manual)) # 794
sum(is.na(met_man_1979_2019$tas_max_manual)) # 794

# consecutive nas
fishcastr::longestNAstrech(met_man_1979_2019$tas_manual) # 100
fishcastr::longestNAstrech(met_man_1979_2019$tas_min_manual) # 100
fishcastr::longestNAstrech(met_man_1979_2019$tas_max_manual) # 100

# ---------------------------------------------------------------------------------------------------------- #
# IMPORT EWEMBI DATA "1979-01-01" "2016-12-31" -------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------- #
ewembi_1979_2016 <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),
                                          "/extdata/data_ewembi_1979_2016.rds"))
# ---------------------------------------------------------------------------------------------------------- #
# IMPORT AUTO STATION DATA "2005-01-01" "2019-09-17" (contains gaps) ---------------------------------------
# ---------------------------------------------------------------------------------------------------------- #
met_auto_2005_2019 <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),
                      "/extdata/data_met_auto_station_2005_2019.rds"))

# ---------------------------------------------------------------------------------------------------------- #
# FILL GAPS (POST 2005) IN MANUAL STATION AIR TEMP DATA WITH AUTO STATION DATA -----------------------------
# ---------------------------------------------------------------------------------------------------------- #
# mean and min and max tas ----
data_tas_filled <- merge(met_man_1979_2019, met_auto_2005_2019, by = "date", all.x = TRUE)
data_tas_filled$tas_manual <- ifelse(is.na(data_tas_filled$tas_manual),
                                       data_tas_filled$tas_auto,
                                       data_tas_filled$tas_manual)
data_tas_filled$tas_min_manual <- ifelse(is.na(data_tas_filled$tas_min_manual),
                                     data_tas_filled$tas_min_auto,
                                     data_tas_filled$tas_min_manual)
data_tas_filled$tas_max_manual <- ifelse(is.na(data_tas_filled$tas_max_manual),
                                         data_tas_filled$tas_max_auto,
                                         data_tas_filled$tas_max_manual)

# total nas
sum(is.na(data_tas_filled$tas_manual)) # 355/794 left
sum(is.na(data_tas_filled$tas_min_manual)) # 355/443 left
sum(is.na(data_tas_filled$tas_max_manual)) # 355/421 left
# consecutive nas
fishcastr::longestNAstrech(data_tas_filled$tas_manual) # 100
fishcastr::longestNAstrech(data_tas_filled$tas_min_manual) # 100
fishcastr::longestNAstrech(data_tas_filled$tas_max_manual) # 100

# -------------------------------------------------------------------------------- #
# already included this step during fishcastr::download_Met_Eireann_station_data(), but could be needed in other applications
# # identify non-matching pairs (of nas); i.e, where at least one of the two is na, set both to na.
# data_tas_filled$tas_min_manual <- ifelse(is.na(data_tas_filled$tas_max_manual),
#                                                    NA,
#                                       data_tas_filled$tas_min_manual)
#
# data_tas_filled$tas_max_manual <- ifelse(is.na(data_tas_filled$tas_min_manual),
#                                                    NA,
#                                       data_tas_filled$tas_max_manual)
# -------------------------------------------------------------------------------- #

# total nas again (should be 307 minimum... and should match)
sum(is.na(data_tas_filled$tas_min_manual)) # 355 left
sum(is.na(data_tas_filled$tas_max_manual)) # 355 left

# consecutive nas
longestNAstrech(data_tas_filled$tas_min_manual) # 100
longestNAstrech(data_tas_filled$tas_max_manual) # 100

# ---------------------------------------------------------------------------------------------------------- #
# FILL GAPS (PRE-2005, POST 1979) IN MANUAL STATION AIR TEMP DATA WITH EWEMBI GRIDDED DATA -----------------
# ---------------------------------------------------------------------------------------------------------- #
# MEAN tas ----
data_tas_filledv2 <- merge(data_tas_filled, ewembi_1979_2016, by = "date", all.x = TRUE)
data_tas_filledv2$tas_manual <- ifelse(is.na(data_tas_filledv2$tas_manual),
                                         data_tas_filledv2$tas_ewembi,
                                         data_tas_filledv2$tas_manual)
data_tas_filledv2$tas_min_manual <- ifelse(is.na(data_tas_filledv2$tas_min_manual),
                                           data_tas_filledv2$tasmin_ewembi,
                                           data_tas_filledv2$tas_min_manual)
data_tas_filledv2$tas_max_manual <- ifelse(is.na(data_tas_filledv2$tas_max_manual),
                                           data_tas_filledv2$tasmax_ewembi,
                                           data_tas_filledv2$tas_max_manual)

# total nas
sum(is.na(data_tas_filledv2$tas_manual)) # 0/355 left
sum(is.na(data_tas_filledv2$tas_min_manual)) # 0/355 left
sum(is.na(data_tas_filledv2$tas_max_manual)) # 0/355 left

# ---------------------------------------------------------------------------------------------------------- #
# CALCULATE PET (Hargreaves-Samani) for tmax and tmin
# ---------------------------------------------------------------------------------------------------------- #
################ #
###### NOTE #### #
# FOR THIS FUNCTION TO WORK, $DATA MUST BE 3D ARRAY FORMAT (CONVERSION FROM
# INTERPOLATED BACK TO SINGLE GRID SQUARE SHOWN BELOW). ALSO XY COORDINATES MUST
# MATCH DIMENSIONS OF 3D ARRAY.
################ #
################ #
# subset manual station data to tas min and tas max only
tas_min_max_filled <- data_tas_filledv2[,c("date","tas_min_manual","tas_max_manual")]

# reformat 2d filled station data to 3d array
obs_grid_tasmin3d <-
  transformeR::mat2Dto3Darray(
    mat2D = matrix(ncol = 1, tas_min_max_filled$tas_min_manual),
    x = c(-9.75,-9.25),
    y = c(53.8,54.25)
  )
obs_grid_tasmax3d <-
  transformeR::mat2Dto3Darray(
    mat2D = matrix(ncol = 1, tas_min_max_filled$tas_max_manual),
    x = c(-9.75,-9.25),
    y = c(53.8,54.25)
  )

# substitute manual station data in for ewembi data
grid_ewembi_1979_2016 <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),
                                          "/extdata/grid_ewembi_1979_2016.rds"))

grid_ewembi_1979_2016$tasmin$Data <- obs_grid_tasmin3d
grid_ewembi_1979_2016$tasmax$Data <- obs_grid_tasmax3d

# rest xycords
grid_ewembi_1979_2016$tasmin$xyCoords <- list(x = c(-9.75,-9.25),
                                              y = c(53.8,54.25))
grid_ewembi_1979_2016$tasmax$xyCoords <- list(x = c(-9.75,-9.25),
                                              y = c(53.8,54.25))

# set new dates that match local obs dates...
grid_ewembi_1979_2016$tasmin$Dates$start <- as.character(as.POSIXct(tas_min_max_filled$date, tz = "GMT"),
                                                         format("%Y-%m-%d %H:%M:%S %Z"))
grid_ewembi_1979_2016$tasmin$Dates$end <- as.character(as.POSIXct(tas_min_max_filled$date, tz = "GMT"),
                                                       format("%Y-%m-%d %H:%M:%S %Z"))
grid_ewembi_1979_2016$tasmax$Dates$start <- as.character(as.POSIXct(tas_min_max_filled$date, tz = "GMT"),
                                                         format("%Y-%m-%d %H:%M:%S %Z"))
grid_ewembi_1979_2016$tasmax$Dates$end <- as.character(as.POSIXct(tas_min_max_filled$date, tz = "GMT"),
                                                       format("%Y-%m-%d %H:%M:%S %Z"))

grid_met_obs_1979_2019_tas_min_max <- list("tasmin" = grid_ewembi_1979_2016$tasmin,
                                           "tasmax" = grid_ewembi_1979_2016$tasmax)

grid_met_obs_petHS <- drought4R::petGrid(tasmin = grid_met_obs_1979_2019_tas_min_max$tasmin,
                           tasmax = grid_met_obs_1979_2019_tas_min_max$tasmax,
                           method = "hargreaves-samani")

# now interpolate petH
lake <- list(x = -9.578, y = 53.973)
grid_met_obs_petHS.interp <- transformeR::interpGrid(grid_met_obs_petHS,
                                        new.coordinates = lake,
                                        method = "bilinear",
                                        bilin.method = "akima")

# -------------------------------------------------------------------------------------------------------------- #
# CREATE GRID OBJECT FOR LOCAL MET STATION FILLED (WITH AUTO AND EWEMBI) DATA
# -------------------------------------------------------------------------------------------------------------- #
petH <- grid_met_obs_petHS.interp

grid_met_obs_1979_2019 <- grid_ewembi_1979_2016
# petH
grid_met_obs_1979_2019$petH <- grid_met_obs_petHS.interp
grid_met_obs_1979_2019$petH$Data <- round(grid_met_obs_1979_2019$petH$Data,2)

# tasmin
grid_met_obs_1979_2019$tasmin$Data <- as.array(data_tas_filledv2$tas_min_manual)
grid_met_obs_1979_2019$tasmin$Dates$start <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                          format("%Y-%m-%d %H:%M:%S %Z"))
grid_met_obs_1979_2019$tasmin$Dates$end <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                          format("%Y-%m-%d %H:%M:%S %Z"))
grid_met_obs_1979_2019$tasmin$xyCoords <- grid_met_obs_1979_2019$petH$xyCoords
attributes(grid_met_obs_1979_2019$tasmin$Data)$dimensions <- "time"
attributes(grid_met_obs_1979_2019$tasmin$Data)$dim <- length(data_tas_filledv2$tas_min_manual)

# tas max
grid_met_obs_1979_2019$tasmax$Data <- as.array(data_tas_filledv2$tas_max_manual)
grid_met_obs_1979_2019$tasmax$Dates$start <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                          format("%Y-%m-%d %H:%M:%S %Z"))
grid_met_obs_1979_2019$tasmax$Dates$end <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                        format("%Y-%m-%d %H:%M:%S %Z"))
grid_met_obs_1979_2019$tasmax$xyCoords <- grid_met_obs_1979_2019$petH$xyCoords
attributes(grid_met_obs_1979_2019$tasmax$Data)$dimensions <- "time"
attributes(grid_met_obs_1979_2019$tasmax$Data)$dim <- length(data_tas_filledv2$tas_max_manual)

# tas
grid_met_obs_1979_2019$tas$Data <- as.array(data_tas_filledv2$tas_manual)
grid_met_obs_1979_2019$tas$Dates$start <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                         format("%Y-%m-%d %H:%M:%S %Z"))
grid_met_obs_1979_2019$tas$Dates$end <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                       format("%Y-%m-%d %H:%M:%S %Z"))
attributes(grid_met_obs_1979_2019$tas$Data)$dimensions <- "time"
attributes(grid_met_obs_1979_2019$tas$Data)$dim <- length(data_tas_filledv2$tas_manual)

#pr
grid_met_obs_1979_2019$pr$Data <- as.array(data_tas_filledv2$pr_manual)
grid_met_obs_1979_2019$pr$Dates$start <- as.character(as.POSIXct(data_tas_filledv2$date, tz = "GMT"),
                                                       format("%Y-%m-%d %Z"))
grid_met_obs_1979_2019$pr$Dates$end <- as.character(as.POSIXct(data_tas_filledv2$date +1, tz = "GMT"),
                                                     format("%Y-%m-%d %Z"))
attributes(grid_met_obs_1979_2019$pr$Data)$dimensions <- "time"
attributes(grid_met_obs_1979_2019$pr$Data)$dim <- length(data_tas_filledv2$pr_manual)

# -------------------------------------------------------------------------------------------------------------- #
# EXPORT LOCAL MET LIST OBJECT AS rda TO BE USED FOR BIAS CORRECTION OF ECMWF'S ERA5 AND SEAS5
# -------------------------------------------------------------------------------------------------------------- #
saveRDS(object = grid_met_obs_1979_2019, file = paste0(system.file("inst", package = "fishcastr"),
                                                      "/extdata/grid_met_obs_1979_2019.rds"))

# -------------------------------------------------------------------------------------------------------------- #
# EXPORT LOCAL MET DATA TO .dat FILE FOR ARCHIVE
# -------------------------------------------------------------------------------------------------------------- #
data <- grid_met_obs_1979_2019

# extract the data arrays of all variables from the list
data <- lapply(data, function(x) x[["Data"]])

# Build data frame
dates <- grid_met_obs_1979_2019[[1]]$Dates
xycoords <- transformeR::getCoordinates(grid_met_obs_1979_2019[[1]])
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data)
ClimateModelName <- "Furnace_manual_station_gp_fld"

# Create directory and save df as .dat file
arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/Met_Eireann_EWEMBI_gp_fld")
dir.create(arc_dir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

dirName <- paste0(arc_dir,"/",ClimateModelName,"/", sep = "", collapse = NULL)
dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")

write.table(df, paste0(dirName,"meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
# now ready for bias correction of reanalyses and seasonal forecasts...

# -------------------------------------------------------------------------------------------------------------- #
# EXPORT LOCAL MET DATA AS DATA TABLE
# -------------------------------------------------------------------------------------------------------------- #
# export table as rda
colnames(df) <- c("date","time","tas_manual", "pr_manual","tasmin_manual","tasmax_manual","petH_manual")
df$date <- as.Date(df$date)
df <- df[,-2]
data_met_obs_1979_2019 <- df
saveRDS(object = data_met_obs_1979_2019, file = paste0(system.file("inst", package = "fishcastr"),
                                                       "/extdata/data_met_obs_1979_2019.rds"))
