# ----------------------------------------------------------------------------------------------- #
# A script for bias correcting ECMWF SEAS5 reforecast data Jul - Dec 1993 to
# 2016 and archived operational forecast 2017 to 2019 for retrospective skill
# assessment
# ----------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------- #
# LOAD OBSERVATIONS AND DATA TO BE BIAS CORRECTED ----
# ----------------------------------------------------------------------------------------------- #
data_reforecast <- fishcastr::grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_raw

obs.data <- fishcastr::grid_met_obs_1979_2019[-c(3,4)]
obs.data$petH$Variable$varName <- "petH"

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

# ------------------------------------------------------------------------------------------------- #
# Bias correction and bias correction with leave-one-year-out ("loo")
# cross-validation
# ------------------------------------------------------------------------------------------------- #
# type ?biasCorrection in R for more info about the parameter settings for bias
# correction. loo required for hindcast to be comparable with operational system
# note that each held out window is defualted to the length of the seaonal
# foreast. Therefore, the window argument cannot be used. This is slightly
# inconsistent with bias correction of ERA5, but ok as long as seasons are not
# too long... July to December is maybe pushing it, but this comment at least
# acknowledges it... Maybe the package will allow the window argument to be
# used in future.

# ------------------------------------------------------------------------------------------------- #
# NOTE POSSIBLE ISSUE
# ISSUE WITH BC FOR SEASONAL FORECASTS DETAIL...
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1")
#with downscaleR@v3.3.1
# Error in arr[, , , ind, , ] <- grid[["Data"]] :
# number of items to replace is not a multiple of replacement length, so install old versions again
# try downscaler 3.1.3 if 3.3.1 fails (see (probably) same issue posted on github July 2020,
#https://github.com/search?q=org%3ASantanderMetGroup+%22arr%5B%2C+%2C+%2C+ind%2C+%2C+%5D%22&type=issues),but also seen this separate error: Error: object ‘draw.world.lines’ is not exported by 'namespace:transformeR'
#devtools::install_github("SantanderMetGroup/transformeR@v1.7.4") # was 2.0.1
#devtools::install_github("SantanderMetGroup/downscaleR@v3.1.3") # was 3.3.1
#revert back to 3.1.3 if required
# ------------------------------------------------------------------------------------------------- #

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

InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- init_dates})
names(InitializationDates) <- data.bc.cross$tas$Members
data.bc.cross$tas$InitializationDates <- InitializationDates
data.bc.cross$pr$InitializationDates <- InitializationDates
data.bc.cross$petH$InitializationDates <- InitializationDates

grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc <- data.bc.cross

# round data
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc$tas$Data <- round(grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc$tas$Data,2)
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc$pr$Data <- round(grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc$pr$Data,2)
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc$petH$Data <- round(grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc$petH$Data,2)

# export data
usethis::use_data(grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc, overwrite = TRUE)
