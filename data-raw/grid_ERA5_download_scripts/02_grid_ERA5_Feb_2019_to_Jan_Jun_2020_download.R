# ------------------------------------------------------------------------------------------------------- #
# A script for downloading ECMWF ERA5 data Feb 2019 to Jan 2020 (for salmon) and
# July 2019 to June 2020 (for eels)
# ## NOTE ## - the user has to change credential details after registering with
# Copernicus Climate Data Store.
# This script can be adapted for adding additional reforecast years as they
# become available. However, if the user seeks to produce an operational
# forecast (i.e., download ERA5 data before it has been fully QAQCd), please
# refer to the script "ERA5_reanalysis_download_complex_expver.R" for an example
# of the complexities involved in accessing recent ERA5 data.
# ------------------------------------------------------------------------------------------------------- #
library(keyring)
library(ecmwfr)

# on first time run these lines with user specific credentials that can be accessed via login to the Copernicus cds and the ecmwf websites, respectively.

# ecmwfr::wf_set_key(user = "xxxxx",
#                    key = "xxxxxxxxxxxxxxxxxxxxxxxxxx",
#                    service = "cds")
# ecmwfr::wf_set_key(user = "xxxxxxxxxxxxxxx",
#                    key = "xxxxxxxxxxxxxxxxxxxxxxxxxxxx",
#                    service = "webapi")

arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_ERA5")
dir.create(arc_dir, showWarnings = TRUE, mode = "0777")

########
# SALMON
# ----------------------------------------------------------------------------------- #
# reanalysis Feb to Dec 2019
# ----------------------------------------------------------------------------------- #
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2019",
  month = c("02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download_F_D_2019.nc"
)

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = arc_dir,
                             verbose = TRUE)
# ----------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------- #
# reanalysis Jan 2020
# ----------------------------------------------------------------------------------- #
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2020",
  month = c("01"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download_J_2020.nc"
)

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = arc_dir,
                             verbose = TRUE)

# ----------------------------------------------------------------------------------- #

########
# EELS
# ----------------------------------------------------------------------------------- #
# reanalysis July to Dec 2019
# ----------------------------------------------------------------------------------- #
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2019",
  month = c("07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download_J_D_2019.nc"
)

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = arc_dir,
                             verbose = TRUE)
# ----------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------- #
# reanalysis Jan to June 2020
# ----------------------------------------------------------------------------------- #
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = "2020",
  month = c("01","02","03","04","05","06"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = "download_Jan_Jun_2020.nc"
)

ncfile <- ecmwfr::wf_request(user = "xxxxx",
                             request = request,
                             transfer = TRUE,
                             path = arc_dir,
                             verbose = TRUE)

# ----------------------------------------------------------------------------------- #
