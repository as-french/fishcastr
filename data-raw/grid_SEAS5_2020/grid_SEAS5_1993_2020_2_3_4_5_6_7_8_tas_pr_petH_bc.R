# ------------------------------------------------------------------------------------------------------- #
# A script for bias correcting ECMWF SEAS5 reforecast data Feb - August 1993 to
# 2016 and archived operational forecast 2017 to 2019 for retrospective skill
# assessment
# ------------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------------------------------------------------------------#
# LOAD OBSERVATIONS AND DATA TO BE BIAS CORRECTED ----
# ------------------------------------------------------------------------------------------------------------------#
data_reforecast <- fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_raw

obs.data <- fishcastr::grid_met_obs_1979_2019[-c(3,4)]
obs.data$petH$Variable$varName <- "petH"

# for demonstration purposes, we will subset the obs grid to 2018 as if we were in
# july 2019 about to build a forecast..

#obs.data <- lapply(obs.data, function(x) transformeR::subsetGrid(grid = x,
#                                                                 years = 1993:2019))

#range(data_reforecast$tas$Dates$start)
#range(obs.data$tas$Dates$start)
#attr(obs.data$tas$Data, "dimensions") <- c("time")
#attr(obs.data$pr$Data, "dimensions") <- c("time")

#attr(data_reforecast$tas$Data, "dimensions") <- c("member", "time")
#attr(data_reforecast$pr$Data, "dimensions") <- c("member", "time")

# subset obs data to reforecast period... simulated 2019 forecst
# subset operational year -------
#obs.data.subset <- lapply(obs.data, function(x) transformeR::subsetGrid(grid = x,
#                                                                      years = 1993:2018,
#                                                                      season = 1:12))

# Subset all datasets to the same Dates as the reforcast precipitation. Note that we compute daily accumulated
# precipitation, for this reason this variable has no value for the first day of every season.
if (sum(names(data_reforecast)=="pr")>0){
  data <- lapply(1:length(data_reforecast), function(x)  {transformeR::intersectGrid(data_reforecast[[x]], obs.data[[which(names(data_reforecast)=="pr")]], type = "temporal", which.return = 1)})
  names(data) <- sapply(data, function(x) transformeR::getVarNames(x))
  obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data[[x]], type = "temporal", which.return = 1)})
  names(obs.data) <- sapply(obs.data, function(x) transformeR::getVarNames(x))
} else{
  obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data_reforecast[[x]], type = "temporal", which.return = 1)})
  names(obs.data) <- sapply(obs.data, function(x) transformeR::getVarNames(x))
}

# Check variable consistency
if (!identical(names(obs.data), names(data))) stop("variables in obs.data and data (seasonal forecast) do not match.")
#range(data$tas$Dates$start)
#range(obs.data$tas$Dates$start)
#range(data_reforecast$tas$Dates$start)
#range(data$tas$InitializationDates)
#range(data$tas$Dates$start)

# ----------------------------------------------------------------------------------------------------------#
# Bias correction and bias correction with leave-one-year-out ("loo")
# cross-validation
# ----------------------------------------------------------------------------------------------------------#
# type ?biasCorrection in R for more info about the parameter settings for bias
# correction. loo required for hindcast to be comparable with operational system
# note that each held out window is defualted to the length of the seaonal
# foreast. Therefore, the window argument cannot be used. This is slightly
# inconsistent with bias correction of ERA5, but ok as long as seasons are not
# too long... July to December is maybe pushing it, but this comment at least
# acknowledges it... Maybe the package will allow the window argument to be
# used in future.

# NOTE POSSIBLE ISSUE
# ISSUE WITH BC FOR SEASONAL FORECASTS DETAIL...
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1")
#with downscaleR@v3.3.1
# Error in arr[, , , ind, , ] <- grid[["Data"]] :
# number of items to replace is not a multiple of replacement length, so install old versions again
# try downscaler 3.1.3 if 3.3.1 fails (see (probably) same issue posted on github July 2020,
#https://github.com/search?q=org%3ASantanderMetGroup+%22arr%5B%2C+%2C+%2C+ind%2C+%2C+%5D%22&type=issues),but also seen this separate error: Error: object ‘draw.world.lines’ is not exported by 'namespace:transformeR'
devtools::install_github("SantanderMetGroup/transformeR@v1.7.4") # was 2.0.1
devtools::install_github("SantanderMetGroup/downscaleR@v3.1.3") # was 3.3.1
#revert back to 3.1.3

data.bc.cross <- lapply(1:length(data), function(x)  {
  precip <- FALSE
  if (names(data)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data[[x]],
                             x = data[[x]],
                             method = "eqm",
                             cross.val = "loo",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             join.members = TRUE)
})

names(data.bc.cross) <- names(data)

# sort out initialisation dates again...1993 to 2019
# retain only initialisation dates that are included in $Dates sublist
init_dates <- data$tas$InitializationDates$Member_1
#init_dates <- data$tas$InitializationDates$Member_1[-length(data$tas$InitializationDates$Member_1)]

InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- init_dates})
names(InitializationDates) <- data.bc.cross$tas$Members
data.bc.cross$tas$InitializationDates <- InitializationDates
data.bc.cross$pr$InitializationDates <- InitializationDates
data.bc.cross$petH$InitializationDates <- InitializationDates

grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc <- data.bc.cross

# round data
grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc$tas$Data <- round(grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc$tas$Data,2)
grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc$pr$Data <- round(grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc$pr$Data,2)
grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc$petH$Data <- round(grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc$petH$Data,2)

# export data
usethis::use_data(grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc, overwrite = TRUE)


# # no loo required for operational year...
# data.bc <- lapply(1:length(data), function(x)  {
#   precip <- FALSE
#   if (names(data)[x] == "pr") precip <- TRUE
#   downscaleR::biasCorrection(y = obs.data[[x]],
#                              x = data[[x]],
#                              newdata = data_reforecast[[x]],
#                              method = "eqm",
#                              precipitation = precip,
#                              wet.threshold = 0.1,
#                              join.members = TRUE)
# })
# names(data.bc) <- names(data)
# # initialisation dates do not need any correction after non loo method?

#range(data.bc.cross$tas$Dates$start)
#append loo with non loo for operational year and save.
# ----------------------------------------------------------------------------------------------------------#

# # subset operational year for demonstrative purposes -------
# # subsetGrid takes care of initialization dates, unlike intersectGrid
# data.bc.subset <- lapply(data.bc, function(x) transformeR::subsetGrid(grid = x,
#                                                                       years = 2020))
#
# # bind grids -----
# data.bc.tas.list <- list("tas_refor" = data.bc.cross$tas,
#                          "tas_op" = data.bc.subset$tas)
#
# data.bc.pr.list <- list("pr_refor" = data.bc.cross$pr,
#                         "pr_op" = data.bc.subset$pr)
#
# data.bc.petH.list <- list("petH_refor" = data.bc.cross$petH,
#                           "petH_op" = data.bc.subset$petH)
#
# data.bc.tas.list.bind <- do.call(transformeR::bindGrid, c(data.bc.tas.list, dimension = "time"))
# data.bc.pr.list.bind <- do.call(transformeR::bindGrid, c(data.bc.pr.list, dimension = "time"))
# data.bc.petH.list.bind <- do.call(transformeR::bindGrid, c(data.bc.petH.list, dimension = "time"))
#
# data.bc.bind <- list("tas" = data.bc.tas.list.bind,
#                      "pr" = data.bc.pr.list.bind,
#                      "petH" = data.bc.petH.list.bind)
#
# # again sort out initialisation dates
# init_dates <- data$tas$InitializationDates$Member_1
#
# InitializationDates <- list()
# InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- init_dates})
# names(InitializationDates) <- data.bc.bind$tas$Members
# data.bc.bind$tas$InitializationDates <- InitializationDates
# data.bc.bind$pr$InitializationDates <- InitializationDates
# data.bc.bind$petH$InitializationDates <- InitializationDates

# season <- 2:8
# dataset <- "S5_seas_25"
# dir.Rdata <-  paste0(getwd(),"/data/")
# saveRDS(data.bc.bind, file = paste0(dir.Rdata, dataset,"_", paste0(season, collapse = "_"), "_", paste0(names(data), collapse = "_"), "_bc.rds"))
# dir.Rdata <-  paste0(getwd(),"/inst/extdata/S5/SF_Sp_1993_pres_bcc/")
# saveRDS(data.bc.bind, file = paste0(dir.Rdata, dataset,"_", paste0(season, collapse = "_"), "_", paste0(names(data), collapse = "_"), "_bc.rds"))
# ----------------------------------------------------------------------------------------------------------#
# save Rdata (*.rds file)
# ----------------------------------------------------------------------------------------------------------#
#dir.Rdata = paste0(getwd(),"/data/")
#saveRDS(data.bc.cross, file = paste0(dir.Rdata, dataset, "_", paste0(season, collapse = "_"), "_", paste0(names(data), collapse = "_"), "_BCcross.rds"))

# ----------------------------------------------------------------------------------------------------------#
# BUILD FINAL DATA AND EXPORT ACCORDING TO THE WATExR ARCHIVE DESIGN
# ----------------------------------------------------------------------------------------------------------#
## SEE the proposal for the WATExR Archive Design in:
## https://docs.google.com/document/d/1yzNtw9W_z_ziPQ6GrnSgD9ov5O1swnohndDTAWOgpwc/edit

# Select the object to export (can be 'data.bc', 'data.bc.cross' or 'data')
datatoexport <- data.bc.bind

#dir.data <- "C:\\Users\\afrench.INSTITUTE\\WAH2\\Fish_phenology_manuscript\\METEO_data\\Seasonal_Forecast_Spring_raw_system5\\"
#datatoexport <- data

# Collect some common metadata (e.g., from variable uas)
dates <- datatoexport[[1]]$Dates
xycoords <- transformeR::getCoordinates(datatoexport[[1]])

# Give format to dates
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")

# Define metadata to generate the file name
institution <- "MI"
lake_id <- "Burrishoole"
ClimateModelName <- "S5"
ExperimentName <- "seasonal_bcc"
freq <- "day"

dir.data <- paste0(getwd(),"/inst/extdata/S5_bcc_op_Sp/")
mem = 1:25

# Save a single file for each member
for (i in mem) {
  # Build data.frame for a single member
  single.member <- lapply(datatoexport, function(x) transformeR::subsetGrid(x, members = i))
  single.member <- lapply(single.member, function(x) x$Data)
  # Remove unwanted variables
  # data.frame creation
  df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), single.member)
  if (i < 10) {
    member <- paste0("member0", i, sep = "", collapse = NULL)
  } else {
    member <- paste0("member", i, sep = "", collapse = NULL)
  }
  startTime <- format(as.POSIXlt(yymmdd[1]), format = "%Y%m%d")
  endTime <- format(tail(as.POSIXlt(yymmdd), n = 1), format = "%Y%m%d")
  dirName <- paste0(dir.data, "/", member, "/", sep = "", collapse = NULL)
  dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  write.table(df, paste0(dirName, "meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
}
# ----------------------------------------------------------------------------------------------------------#
