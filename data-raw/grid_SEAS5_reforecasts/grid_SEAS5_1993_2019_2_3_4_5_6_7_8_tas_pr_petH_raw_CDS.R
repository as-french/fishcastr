# ----------------------------------------------------------------------------------------- #
# A script to compute variables from raw CDS downloaded multi-member SEAS5
# forecast nc files (e.g., t2m or tp) that have been converted to grid using
# read_nc_to_grid. Here, we extract tas, pr, tasmin and tasmax and calculate
# potential evapotranspiration by the Hargreaves-Samani equation (petH).
# ----------------------------------------------------------------------------------------- #

nc_path <- paste0(
  system.file("extdata", package = "fishcastr"),
  "/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip"
)

# ----------------------------------------------------------------------------------------- #
# TAS
# ----------------------------------------------------------------------------------------- #

t2m_data <- fishcastr::read_nc_to_grid(nc_folder_path = nc_path,
                                       forecast_months = 2:8,
                                       lat = c(53,54),
                                       lon = c(-10,-9),
                                       interp_coord = list(x = -9.578, y = 53.973),
                                       lead_month = 0,
                                       members = 1:25)

# extract daily mean tas
tas <- transformeR::aggregateGrid(grid = t2m_data,
                                     aggr.d = list(FUN = "mean", na.rm = TRUE))


# ----------------------------------------------------------------------------------------- #
# TASMIN
# ----------------------------------------------------------------------------------------- #
# extract tasmin
tasmin <- transformeR::aggregateGrid(grid = t2m_data,
                                     aggr.d = list(FUN = "min", na.rm = TRUE))

# convert time units to not include hours as this is required for petH calculation
tasmin$Dates$start <- as.character(as.Date(tasmin$Dates$start),'%Y-%m-%d %Z')
tasmin$Dates$end <- as.character(as.Date(tasmin$Dates$end),'%Y-%m-%d %Z')


# ----------------------------------------------------------------------------------------- #
# TASMAX
# ----------------------------------------------------------------------------------------- #
# extract tasmax
tasmax <- transformeR::aggregateGrid(grid = t2m_data,
                                     aggr.d = list(FUN = "max", na.rm = TRUE))
# convert time units to not include hours as this is required for petH calculation
tasmax$Dates$start <- as.character(as.Date(tasmax$Dates$start),'%Y-%m-%d %Z')
tasmax$Dates$end <- as.character(as.Date(tasmax$Dates$end),'%Y-%m-%d %Z')

# ----------------------------------------------------------------------------------------- #
# PETH
# ----------------------------------------------------------------------------------------- #
# calculate petH
petH <- drought4R::petGrid(tasmin = tasmin,tasmax = tasmax,method = "hargreaves-samani")
petH$Variable$varName <- "petH"

# ----------------------------------------------------------------------------------------- #
# PR
# ----------------------------------------------------------------------------------------- #
nc_path <- paste0(
  system.file("extdata", package = "fishcastr"),
  "/ECMWF_SEAS5/download_salmon_tp/download_salmon_tp_unzip"
)

tp_data <- fishcastr::read_nc_to_grid(nc_folder_path = nc_path,
                                       forecast_months = 2:8,
                                       lat = c(53,54),
                                       lon = c(-10,-9),
                                       interp_coord = list(x = -9.578, y = 53.973),
                                       lead_month = 0,
                                       members = 1:25)

# ----------------------------------------------------------------------------------------- #
# COMBINE DATA INTO A LIST AND INTERPOLATE TO POINT
# ----------------------------------------------------------------------------------------- #
data.prelim <- list("tas" = tas,
                  "pr" = tp_data,
                  "petH" = petH)

interp_coord = list(x = -9.578, y = 53.973)

# interpolate to point
data.interp<- lapply(data.prelim, function(x) transformeR::interpGrid(x, new.coordinates = interp_coord,
                                                                      method = "bilinear",
                                                                      bilin.method = "akima"))

# round data
data.interp$tas$Data <- round(data.interp$tas$Data,2)
data.interp$pr$Data <- round(data.interp$pr$Data,2)
data.interp$petH$Data <- round(data.interp$petH$Data,2)

# ----------------------------------------------------------------------------------------- #
# EXPORT DATA TO EXT FOLDER FOR BIAS CORRECTION ETC.
# ----------------------------------------------------------------------------------------- #
# export data to rds
years <- range(lubridate::year(data.interp$tas$Dates$start))
months <- range(lubridate::month(data.interp$tas$Dates$start))
vars <- names(data.interp)
saveRDS(data.interp,
        file = paste0(
          system.file("extdata", package = "fishcastr"),
          "/grid_SEAS5_",years[1],"_",years[2],"_",paste(months[1]:months[2],collapse = "_"),"_",
          paste(vars,collapse = "_"),"_raw_CDS.rds"
        ))
# ----------------------------------------------------------------------------------------- #
