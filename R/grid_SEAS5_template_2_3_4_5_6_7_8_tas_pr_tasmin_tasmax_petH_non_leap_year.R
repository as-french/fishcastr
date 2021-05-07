#' A template grid object in format of ECMWF's SEAS seasonal climate forecast at
#' Burrishoole, Co Mayo, Ireland
#'
#' @description An empty template 25 member grid for daily mean air temperature
#'   at 2m, daily total precipitation and potential evapotranspiration
#'   (calculated using the Hargreaves-Samani equation).
#'
#' @details To build this template, raw SEAS5 6-hourly data were initially
#'   accessed using the Copernicus Climate Data Store for the 1993 seasonal
#'   forecast initialised on 01 February. The template grid was built following
#'   two steps: Firstly, an ncml file was written to correspond with the raw
#'   downloaded netcdf file from the CDS (here, we used the unidata Java
#'   application linked to below); Secondly, using the ncml file, the data were
#'   read into R using the climate4R tool loadeR::loadGridData() (see /data-raw/
#'   folder for details). The template represents an archived 25 member SEAS5
#'   re-forecast and can be used to represent any 25 member re-forecast SEAS5
#'   forecast (1993-2016) or the first 25 members of archived operational
#'   forecasts (2017 - 2019).
#'
#' @format A list of five 25 member grids: \describe{
#'   \item{tas}{mean air temperature, in degrees Celsius}
#'   \item{pr}{total precipitation, in millimetres}
#'   \item{tasmin}{minimum air temperature, in degrees Celsius}
#'   \item{tasmax}{maximum air temperature, in degrees Celsius}
#'   \item{petH}{potential evapotranspiration, in millimetres}... }
#' @source
#' \url{https://cds.climate.copernicus.eu/#!/home}
#' \url{http://artifacts.unidata.ucar.edu/content/repositories/unidata-releases/edu/ucar/toolsUI/4.6.8/toolsUI-4.6.8.jar}
#'
"grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_non_leap_year"
