#' Calculate potential evapotranspiration
#'
#' This function takes vectors of dates, maximum air temperature and minimum air
#' temperature and latitude and calculates potential evapotranspiration by the
#' Hargreaves-Samani equation implemented in *drought4R* package (Bedia &
#' Iturbide, 2019).
#'
#' @param refdates Vector of dates in class "Date".
#' @param lats Latitude in decimal degrees WGS84
#' @param tasmin a vector of min air temperatures
#' @param tasmax a vector of max air temperatures
#' @return A vector of potential evapotranspiration estimates.
#' @references
#' Allen, R. G., & Food and Agriculture Organization of the United Nations
#' (Eds.). (1998). Crop evapotranspiration: Guidelines for computing crop water
#' requirements. Food and Agriculture Organization of the United Nations.
#' Bedia, J., & Iturbide, M. (2019). Drought4R: A climate4R package for drought
#' assessment. https://github.com/SantanderMetGroup/transformeR/wiki
#' @examples
#' # simulate min and max temps
#' min_temp <- rnorm(1:365,
#'                  mean = 12+8*sin(1.5*pi + seq(length = 365,
#'                                               from = 0,
#'                                               to = 2*pi)),
#'                  sd = 3)
#' max_temp <- min_temp + 2
#' potential_ev_HS(refdates = seq(from = as.Date("1981-01-01"),
#'                                to = as.Date("1981-12-31"), by = "day"),
#'                 tasmin = min_temp,
#'                 tasmax = max_temp,
#'                 lats = 53.932458)
#' @export
potential_ev_HS <- function(tasmin, tasmax, lats, refdates) {
  # split time series into calendar years
  calendar_year <- lubridate::year(refdates)
  # identify leap years
  leapyear <- lubridate::leap_year(calendar_year)
  lats <- lats * (pi / 180) ## Conversion decimal degrees --> radians
  J <- as.integer(format(refdates, format("%j")))

  # if leap year set nodays in year
  year.len <- ifelse(leapyear == TRUE, 366, 365)
  ds <- 0.409 * sin(2 * pi * J / year.len - 1.39) ## Solar declination, FAO, eq. 24 (p. 46)
  aux.lat <- matrix(lats, nrow = length(J), ncol = length(lats), byrow = TRUE)
  ws <- acos(-tan(aux.lat) * tan(ds))   ## Sunset hour angle, FAO, eq. 25 (p. 46)
  dr <- 1 + 0.033 * cos(2 * pi * J / year.len) ## inverse relative distance Earth-Sun , FAO, eq. 23 (p. 46)
  Gsc <- 0.082 ## Solar constant (MJ.m-2.min-1)
  Ra <- (24 * 60 * Gsc * dr * (ws * sin(aux.lat) * sin(ds) + cos(aux.lat) * cos(ds) * sin(ws))) / pi ## FAO, eq. 21 (p. 46)
  trng <- tasmax - tasmin
  result <-  as.vector(.0023 * (Ra / 2.45) * ((tasmax + tasmin) / 2 + 17.8) * sqrt(trng)) ## FAO, eq. 52 (p. 64), applying conversion of units to Ra (MJ.m-2.day-1 --> mm.day-1)
  return(result)
}
