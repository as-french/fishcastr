#' Calculate photoperiod from latitude and longitude
#'
#' This function takes a date vector and latitude and longitude and calculates
#' photoperiod (daylength) from sunrise and sunset times using the suncalc
#' package.
#'
#' @param dates Vector of dates in class "Date".
#' @param latitude Latitude in decimal degrees WGS84
#' @param longitude Longitude in decimal degrees WGS84
#' @param ... additional arguments to suncalc::getSunlightTimes
#' @return A vector of daylengths.
#' @export
#' @examples
#'photper_calc(dates = seq(from = as.Date("1981-01-01"),
#'                         to = as.Date("1981-12-31"), by = "day"),
#'             latitude = 53.932458,
#'             longitude = -9.575556)
photper_calc <- function(dates, latitude, longitude, ...){
  sunrise_set <- suncalc::getSunlightTimes(date = dates,
                                  lat = latitude,
                                  lon = longitude,
                                  keep = c("sunrise", "sunset"),
                                  tz = "UTC")
  daylength_res <- as.numeric(sunrise_set$sunset - sunrise_set$sunrise)
  return(daylength_res)
}
