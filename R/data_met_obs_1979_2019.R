#' Gap filled meteorological observations at Burrishoole, Co. Mayo, Ireland.
#'
#' A data table containing daily mean, min and max air temperature at 2m and
#' daily total precipitation and potential evapotranspiration (calculated using
#' the Hargreaves-Samani equation). Data were primarily obtained from Met
#' Éireann's Furnace manual weather station at Burrishoole. Dates 1979-01-01 to
#' 2019-12-31. Gaps (within the 2005 - present period) were filled using Met
#' Éireann's automatic station data where possible. Pre-2005 gaps (and gaps
#' post-2005 that were not possible to fill with automatic met station data)
#' were filled using EWEMBI gridded pseudo-observations.
#'
#' @format A data table with six columns: \describe{
#'   \item{date}{a date, Gregorian calendar}
#'   \item{tas_manual}{mean air temperature, in degrees Celsius}
#'   \item{pr_manual}{total precipitation, in millimetres}
#'   \item{tasmin_manual}{min air temperature, in degrees Celsius}
#'   \item{tasmax_manual}{max air temperature, in degrees Celsius}
#'   \item{petH_manual}{potential evapotranspiration, in millimetres}... }
#' @source
#' \url{https://www.met.ie/climate/available-data/historical-data}
#'
"data_met_obs_1979_2019"
