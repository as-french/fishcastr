#' Daily meteorological observations at Met Éireann's Furnace automatic weather
#' station, Burrishoole, Co. Mayo, Ireland.
#'
#' A dataset containing daily total precipitation and daily mean, min and max
#' air temperature recorded at the Furnace met automatic station since
#' 2005-02-22. Note for reference that Met Éireann calculate potential
#' evapotranspiration using the Penman-Monteith Equation (if downloading from
#' URL below).
#'
#' @format A data frame with 5370 rows and five variables: \describe{
#'   \item{date}{date, a date}
#'   \item{tas_auto}{mean air temperature, in degrees Celsius}
#'   \item{pr_auto}{total daily precipitation, in millimetres}
#'   \item{tas_min_auto}{min air temperature, in degrees Celsius}
#'   \item{tas_max_auto}{max air temperature, in degrees Celsius} ... }
#' @source \url{https://www.met.ie/climate/available-data/historical-data}
#'
"data_met_auto_station_2005_2019"
