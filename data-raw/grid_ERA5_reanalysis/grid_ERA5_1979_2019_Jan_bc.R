# ------------------------------------------------------------------------------------------------------- #
# A script for bias adjusting ECMWF ERA5 data up to Jan 2019 for
# retrospective skill assessment.
# ------------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------------------------------------------------- #
# BIAS ADJUSTMENT ----
# ------------------------------------------------------------------------------------------------------- #
# reanalysis_data <- fishcastr::grid_ERA5_1979_2019_Jan_raw
reanalysis_data <- readRDS(file = paste0(
                             system.file("inst", package = "fishcastr"),
                             "/extdata/grid_ERA5_1979_2019_Jan_raw_CDS.rds"
                           ))

#obs.data <- fishcastr::grid_met_obs_1979_2019[-c(3,4)]
obs.data <- readRDS(file = paste0(
  system.file("inst", package = "fishcastr"),
  "/extdata/grid_met_obs_1979_2019.rds"
))[-c(3,4)]

obs.data$petH$Variable$varName <- "petH"

# Subset observational data to the same dates as forecast data for bias correction calibration
obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], reanalysis_data[[x]], type = "temporal", which.return = 1)})
data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], reanalysis_data[[x]], type = "temporal", which.return = 2)})
varnames <- c("tas", "pr","petH")
names(obs.data) <- varnames
names(data) <- varnames

# Bias correction with leave-one-year-out ("loo") cross-validation type
# ?biasCorrection in R for more info about the parameter settings for bias
# correction. # old version downscale r 3.1.3, not install 3.3.1
#devtools::install_github("SantanderMetGroup/transformeR@v1.7.4") # was 2.0.1
#devtools::install_github("SantanderMetGroup/downscaleR@v3.1.3") # was 3.3.1

# works using versions:
# transformeR 2.0.1
# downscaleR 3.3.1
# but requires years to be subset into whole years for loo to work. might also work with older versions
#devtools::install_github("SantanderMetGroup/transformeR@v2.0.1") # was 2.0.1v1.7.4 (or 2.1.0 on 2021_05_05)
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1") # was v3.1.3

# NOTE using loo one must subset both to full years data, or erro occurs... Error in 1:nrow(mat2D) : argument of length 0 (with v3.1.3 or 3.3.1 downscale and 1.7.4 or 2.0.1 transformeR)
data_sub <- lapply(data,function(x){transformeR::subsetGrid(x,years = 1979:2018)})
obs.data_sub <- lapply(obs.data,function(x){transformeR::subsetGrid(x,years = 1979:2018)})

data.bc.cross <- lapply(1:length(data_sub), function(x)  {
  precip <- FALSE
  if (names(obs.data_sub)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data_sub[[x]],
                             x = data_sub[[x]],
                             method = "eqm",
                             cross.val = "loo",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             window = c(31, 7))
})
names(data.bc.cross) <- names(data)

# dont use loo for operational year (it doesn't work)! It is automatic with
# "newdata" argument, but retain cross validated version for reforecast period to
# be representative of operational forecast.
#devtools::install_github("SantanderMetGroup/downscaleR@v3.3.1")
#with downscaleR@v3.3.1
# Error in arr[, , , ind, , ] <- grid[["Data"]] :
# number of items to replace is not a multiple of replacement length, so install old versions again
#devtools::install_github("SantanderMetGroup/transformeR@v1.7.4") # was 2.0.1
#devtools::install_github("SantanderMetGroup/downscaleR@v3.1.3") # was 3.3.1

data.bc <- lapply(1:length(data_sub), function(x)  {
  precip <- FALSE
  if (names(data_sub)[x] == "pr") precip <- TRUE
  downscaleR::biasCorrection(y = obs.data_sub[[x]],
                             x = data_sub[[x]],
                             newdata = reanalysis_data[[x]],
                             extrapolation = "constant",
                             method = "eqm",
                             precipitation = precip,
                             wet.threshold = 0.1,
                             window = c(31, 7))
})
names(data.bc) <- names(data)

#                 window = c(90, 31),

# -------------------------------------------------------------------------------------------------- #
# Append (loo CV) bias corrected reforecast to bias corrected most recent
# re-forecast or operational year (e.g., 2019) ----
# -------------------------------------------------------------------------------------------------- #

# subset operational year -------
data.bc.subset <- lapply(data.bc, function(x) transformeR::subsetGrid(grid = x,
                                                                      years = 2019,
                                                                      season = 1))

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

# # sort out NA introduced at location 12473... don't know why the biasCorrection function introduces this error...
 # which(is.na(data.bc.bind$tas$Data)) # no NAs
 # which(is.na(data.bc.bind$petH$Data)) # no NAs
 # which(is.na(data.bc.bind$pr$Data)) # an NA TRUE 12473
 # data.bc.bind$pr$Data[12460:12490] # anything odd either side? 6 zeros before and
 # data$pr$Data[12460:12490] # nothing odd in ERA5
 # obs.data$pr$Data[12460:12490] # 15 zeros in a row, which maybe the window width of one week cannot deal with.
 #
 # # check date of missing entry
 # data.bc.bind$pr$Dates$start[12473] # "2013-02-23 GMT"
 # obs.data$pr$Dates$start[12473] # "2013-02-23 GMT"
 # obs.data$pr$Data[12473] # 0

 # replace missing entry with observed value
 data.bc.bind$pr$Data[12473] <- obs.data$pr$Data[12473] # replace NA with obs value

  # round
  data.bc.bind$tas$Data <- round(data.bc.bind$tas$Data,2)
  data.bc.bind$pr$Data <- round(data.bc.bind$pr$Data,2)
  data.bc.bind$petH$Data <- round(data.bc.bind$petH$Data,2)

  # export BC ERA5 data
  grid_ERA5_1979_2019_Jan_bc <- data.bc.bind

  usethis::use_data(grid_ERA5_1979_2019_Jan_bc, overwrite = TRUE)

  saveRDS(grid_ERA5_1979_2019_Jan_bc,
          file = paste0(
            system.file("inst", package = "fishcastr"),
            "/extdata/grid_ERA5_1979_2019_Jan_bc_CDS.rds"
          ))

# ------------------------------------------------------------------------------------------------------ #
