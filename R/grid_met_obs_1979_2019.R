#' Gap-filled meteorological observations at Burrishoole, Co. Mayo, Ireland.
#'
#' A grid containing daily mean, minimum and maximum air temperature at 2m and
#' daily total precipitation and potential evapotranspiration (calculated using
#' the Hargreaves-Samani equation). Data were primarily obtained from Met
#' Éireann's Furnace manual weather station at Burrishoole. Dates 1979-01-01 to
#' 2019-12-31. Gaps (within the 2005 - present period) were filled using Met
#' Éireann's automatic station data where possible. Pre-2005 gaps (and gaps
#' post-2005 that were not possible to fill with automatic met station data)
#' were filled using EWEMBI gridded pseudo-observations. These grid data may be
#' used for posterior bias correction of climate reanalysis and seasonal climate
#' forecasts using the *climate4R* bundle of packages.
#'
#' @format A list of five grids: \describe{
#'   \item{tas}{mean air temperature, in degrees Celsius}
#'   \item{pr}{total precipitation, in millimetres}
#'   \item{tasmin}{min air temperature, in degrees Celsius}
#'   \item{tasmax}{max air temperature, in degrees Celsius}
#'   \item{petH}{potential evapotranspiration, in millimetres}... }
#' @source
#' \url{https://www.met.ie/climate/available-data/historical-data}
#'
"grid_met_obs_1979_2019"
