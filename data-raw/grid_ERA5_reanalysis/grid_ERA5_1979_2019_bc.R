# ------------------------------------------------------------------------------------------------------- #
# A script for bias adjusting ECMWF ERA5 data up to end of Dec 2019 for
# retrospective skill assessment.
# ------------------------------------------------------------------------------------------------------- #

# ------------------------------------------------------------------------------------------------------- #
# BIAS ADJUSTMENT ----
# ------------------------------------------------------------------------------------------------------- #
# reanalysis_data <- fishcastr::grid_ERA5_1979_2019_Jan_raw
reanalysis_data <- readRDS(file = paste0(
                             system.file("inst", package = "fishcastr"),
                             "/extdata/grid_ERA5_1979_2019_raw_CDS.rds"
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
data_sub <- lapply(data,function(x){transformeR::subsetGrid(x,years = 1979:2019)})
obs.data_sub <- lapply(obs.data,function(x){transformeR::subsetGrid(x,years = 1979:2019)})

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

# # # # sort out NA introduced at location 9273 and 12473... don't know why the
# # biasCorrection function (1.7.4 transformer and 3.1.3 downscaler) introduces these errors...
#  which(is.na(data.bc.cross$tas$Data)) # no NAs
#  which(is.na(data.bc.cross$petH$Data)) # no NAs
#  which(is.na(data.bc.cross$pr$Data)) # an NA TRUE 9273 12473
#  data.bc.cross$pr$Data[12460:12490] # anything odd either side? 6 zeros before and
#  data$pr$Data[12460:12490] # nothing odd in ERA5
#   data.bc.cross$pr$Data[9260:9290] # six zeros after
#   data$pr$Data[9260:9290] # nothing odd in ERA5
#   obs.data$pr$Data[12460:12490] # 15 zeros in a row, which maybe the window width of one week cannot deal with.
#   obs.data$pr$Data[9260:9290] # six zeros in a row, which maybe the window width of one week cannot deal with.
# #  #
# #  # # check date of missing entry
#  data.bc.cross$pr$Dates$start[12473] # "2013-02-23 GMT"
#  obs.data$pr$Dates$start[12473] # "2013-02-23 GMT"
#  data.bc.cross$pr$Dates$start[9273] # "2004-05-21 GMT"
#  obs.data$pr$Dates$start[9273] # "2004-05-21 GMT"
#  obs.data$pr$Data[12473] # 0
#  obs.data$pr$Data[9273] # 0

 # replace NAs entry with observed values
 data.bc.cross$pr$Data[12473] <- obs.data$pr$Data[12473] # replace NA with obs value
 data.bc.cross$pr$Data[9273] <- obs.data$pr$Data[9273] # replace NA with obs value

  # round
 data.bc.cross$tas$Data <- round(data.bc.cross$tas$Data,2)
 data.bc.cross$pr$Data <- round(data.bc.cross$pr$Data,2)
 data.bc.cross$petH$Data <- round(data.bc.cross$petH$Data,2)

  # export BC ERA5 data
  grid_ERA5_1979_2019_bc <- data.bc.cross
  #usethis::use_data(grid_ERA5_1979_2019_Jan_bc, overwrite = TRUE)

  saveRDS(grid_ERA5_1979_2019_bc,
          file = paste0(
            system.file("inst", package = "fishcastr"),
            "/extdata/grid_ERA5_1979_2019_bc_CDS.rds"
          ))
