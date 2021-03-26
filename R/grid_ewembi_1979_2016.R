#' EWEMBI pseudo- meteorological observations at Burrishoole, Co. Mayo, Ireland.
#'
#' A grid containing daily mean min and max air temperature at 2m and daily
#' total precipitation and potential evapotranspiration (calculated using the
#' Hargreaves-Samani equation) observed at Burrishoole, obtained from the EWEMBI
#' gridded observation dataset. Dates 1979-01-01 to 2016-12-31. These grid data
#' may be used for posterior bias correction of climate reanalysis and seasonal
#' climate forecasts using the *climate4R* bundle of packages.
#'
#' @format A list of five grids: \describe{
#'   \item{tas}{mean air temperature, in degrees Celsius}
#'   \item{pr}{total precipitation, in millimetres}
#'   \item{tasmin}{min air temperature, in degrees Celsius}
#'   \item{tasmax}{max air temperature, in degrees Celsius}
#'   \item{petH}{potential evapotranspiration, in millimetres}... }
#' @source
#' \url{"https://meteo.unican.es/tds5/catalogs/ewembi/ewembiDatasets.html?dataset=ewembi/ewembi_daily.ncml"}
#' \url{https://www.isimip.org/gettingstarted/input-data-bias-correction/details/27/}
#'
"grid_ewembi_1979_2016"
