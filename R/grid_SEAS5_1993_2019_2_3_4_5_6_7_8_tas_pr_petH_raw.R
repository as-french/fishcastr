#' ECMWF's SEAS seasonal climate forecast at Burrishoole, Co Mayo, Ireland
#'
#' A 25 member grid containing predictions for daily mean air temperature at 2m,
#' daily total precipitation and potential evapotranspiration (calculated using
#' the Hargreaves-Samani equation). Data were obtained from the University of
#' Cantabria's archived 25 member SEAS5 re-forecasts (1993-2016) and 51 member
#' operational forecasts (2017 - 2019). The first 25 members of the archived
#' operational forecasts were selected. Dates: February to August between the
#' period 1993-02-01 to 2019-08-31. These grid data are raw (but rounded to 2
#' decimal places) and have not yet undergone posterior bias correction using
#' the *climate4R* bundle of packages.
#'
#' @format A list of three 25 member grids: \describe{
#'   \item{tas}{mean air temperature, in degrees Celsius}
#'   \item{pr}{total precipitation, in millimetres}
#'   \item{petH}{potential evapotranspiration, in millimetres}... }
#' @source
#' \url{http://meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_Seasonal_25Members_SFC.ncml}
#' \url{http://data.meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_forecast_Seasonal_51Members_SFC.ncml}
#'
"grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_raw"
