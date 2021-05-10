# ------------------------------------------------------------------------------------------------------- #
# A script for downloading ECMWF ERA5 data 1979 to end Jan 2019 for
# retrospective skill assessment of seasonal forecasts for salmon and 1979 to
# end June 2019 for eels (noting that SEAS5 seasonal forecast lead-time and
# target season are appended to the end of these ERA5 datasets; e.g., ...Jan
# 2019|FMAMJJ for salmon, and ...June 2019|JASOND for eels). Thus, these ERA5
# data comprise antecedent conditions for each species' forecast.
# ## NOTE ## - the user has to change credential details after registering with
# Copernicus Climate Data Store.
# ------------------------------------------------------------------------------------------------------- #
library(keyring)
library(ecmwfr)

# on first time run these lines with user specific credentials that can be accessed via login to the Copernicus cds and the ecmwf websites, respectively.

# ecmwfr::wf_set_key(user = "xxxxx",
#                    key = "xxxxxxxxxxxxxxxxxxxxxxxx",
#                    service = "cds")
# ecmwfr::wf_set_key(user = "xxxxxxxxxxxxx",
#                    key = "xxxxxxxxxxxxxxxxxxxxxxxx",
#                    service = "webapi")


# ----------------------------------------------------------------------------------- #
# MULTI-YEAR REQUEST (1979 to 2018) ----
# ----------------------------------------------------------------------------------- #
list_years <- list(c(1979:1984),
                   c(1985:1990),
                   c(1991:1996),
                   c(1997:2002),
                   c(2003:2008),
                   c(2009:2014),
                   c(2015:2018))

arc_dir <- paste0(system.file("inst", package = "fishcastr"),
                  "/extdata/ECMWF_ERA5")
dir.create(arc_dir, showWarnings = TRUE, mode = "0777")

for(i in 1:7){
request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = c("2m_temperature", "total_precipitation"),
  year = as.character(list_years[[i]]),
  month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  area = c(54.2, -10, 53.8, -9.5),
  dataset_short_name = "reanalysis-era5-single-levels",
  target = paste0("download_",i,".nc")
)

ncfile <- wf_request(user = "xxxxx",
                     request = request,
                     transfer = TRUE,
                     path = arc_dir,
                     verbose = TRUE,
                     time_out = 4*3600)
}
# first batch of 1:4 24 years took night to download (set running at 2021-04-27 22:39 and completed last download at 2021-04-28 9:23)
# second batch of 5:7 16 years took night to download (set running at 2021-04-28 23:00 and completed last download at 2021-04-29 05:02)

# There are two options here that are not mutually exclusive. 1. set the time out to 1 hour assuming the request will be completed and purged from the queue within the hour - in this case, set a job name to shift this request to the rstudio jobs window. 2. set a 60 second time out (and again push it to the jobs window to allow simultaneous working in another R session).

# 2 year download takes 30 minutes

# Regardless of manually set time out, the request is still processing at the CDS servers, so we can transfer data later with following lines (which are returned after time-out from the wf_request() function). The user might will need to replace the following url and user number.
 wf_transfer(url = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxx',
             user = "xxxxx",
             path = arc_dir,
             filename = 'download_2015_2016.nc',
             service = 'cds',
             verbose = TRUE)
# #
# wf_delete(user = "xxxxx",
#           url ='xxxxxxxxxxxxxxxxxxxxxxxxxxxx',
#           service = "cds")

 # ----------------------------------------------------------------------------------- #
 # MULTI-MONTH REQUEST (JAN - DEC 2019) (WHOLE YEAR)
 # ----------------------------------------------------------------------------------- #

 request <- list(
   product_type = "reanalysis",
   format = "netcdf",
   variable = c("2m_temperature", "total_precipitation"),
   year = "2019",
   month = c("01","02","03","04","05","06","07","08","09","10","11","12"),
   day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
   time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
   area = c(54.2, -10, 53.8, -9.5),
   dataset_short_name = "reanalysis-era5-single-levels",
   target = paste0("download_",8,".nc")
 )

 ncfile <- ecmwfr::wf_request(user = "xxxxx",
                      request = request,
                      transfer = TRUE,
                      path = arc_dir,
                      verbose = TRUE,
                      time_out = 3*3600)

 # again if time out is reached, download data when progress at CDS server finishes
 wf_transfer(url = 'xxxxxxxxxxxxxxxxxxxxxxxx',
             user = "xxxxx",
             path = arc_dir,
             filename = paste0("download_",9,".nc"),
             service = 'cds',
             verbose = TRUE)
 # #
 # wf_delete(user = "xxxxx",
 #           url ='xxxxxxxxxxxxxxxxxxxxxxxx',
 #           service = "cds")

 # ----------------------------------------------------------------------------------- #

# # ----------------------------------------------------------------------------------- #
# # SINGLE MONTH REQUEST (JAN 2019) (SALMON)
# # ----------------------------------------------------------------------------------- #
#
#  request <- list(
#    product_type = "reanalysis",
#    format = "netcdf",
#    variable = c("2m_temperature", "total_precipitation"),
#    year = "2019",
#    month = c("01"),
#    day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
#    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
#    area = c(54.2, -10, 53.8, -9.5),
#    dataset_short_name = "reanalysis-era5-single-levels",
#    target = paste0("download_",8,".nc")
#  )
#
#  ncfile <- wf_request(user = "xxxxx",
#                       request = request,
#                       transfer = TRUE,
#                       path = arc_dir,
#                       verbose = TRUE,
#                       time_out = 3*3600)
#
#  # again if time out is reached, download data when progress at CDS server finishes
#  wf_transfer(url = 'xxxxxxxxxxxxxxxxxxxxxxxxx',
#              user = "xxxxx",
#              path = arc_dir,
#              filename = paste0("download_",9,".nc"),
#              service = 'cds',
#              verbose = TRUE)
#  # #
#  # wf_delete(user = "xxxxx",
#  #           url ='xxxxxxxxxxxxxxxxxxxxxxxxxxx',
#  #           service = "cds")
#
#  # ----------------------------------------------------------------------------------- #
#
#  # ----------------------------------------------------------------------------------- #
#  # MULTI-MONTH REQUEST (JAN - JUN 2019) (EEL)
#  # ----------------------------------------------------------------------------------- #
#
#  request <- list(
#     product_type = "reanalysis",
#     format = "netcdf",
#     variable = c("2m_temperature", "total_precipitation"),
#     year = "2019",
#     month = c("01","02","03","04","05","06"),
#     day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
#     time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
#     area = c(54.2, -10, 53.8, -9.5),
#     dataset_short_name = "reanalysis-era5-single-levels",
#     target = paste0("download_",9,".nc")
#  )
#
#  ncfile <- wf_request(user = "xxxxx",
#                       request = request,
#                       transfer = TRUE,
#                       path = arc_dir,
#                       verbose = TRUE,
#                       time_out = 3*3600)
#
#  # again if time out is reached, download data when progress at CDS server finishes
#  wf_transfer(url = 'xxxxxxxxxxxxxxxxxxxxxxxxxxx',
#              user = "xxxxx",
#              path = arc_dir,
#              filename = paste0("download_",10,".nc"),
#              service = 'cds',
#              verbose = TRUE)
#  # #
#  # wf_delete(user = "xxxxx",
#  #           url ='xxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
#  #           service = "cds")
#
#  # ----------------------------------------------------------------------------------- #


