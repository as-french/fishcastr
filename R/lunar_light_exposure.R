#' Generate proxy measure of moonlight exposure.
#'
#' @description This function takes a date and calculates an approximate measure
#'   of moonlight exposure depending on geographic location. The function
#'   extracts the duration of the night (sunrise to sunset) in minutes and
#'   calculates the percentage of the night during which the moon is above the
#'   horizon. The function also extracts the sunlit fraction of the moon at
#'   solar midnight (nadir) and the moon's peak altitude/angle above the horizon
#'   during the night in radians. The approximate measure of exposure is
#'   calculated as ln((sin(peak_moon_altitude) * prop_night_moon_above_horizon *
#'   illuminated_fraction_nadir^4) + 0.00001) + min(exposure). This function
#'   requires the suncalc package. This proxy does not account for opposition
#'   surge, but the power of 4 is intended to increase the relative difference
#'   between full moon exposure and other phases (Krisciunas and Schaefer 1991).
#'   However, NOTE: the log transformation is used to increase resolution of
#'   small changes in lunar light exposure at low exposure values. This can be
#'   removed by setting the log_result argument to FALSE.
#'
#' @param date_of_interest A Date.
#' @param lat A numeric.
#' @param lon A numeric.
#' @param log_result A boolean. Default TRUE.
#' @return A numeric of approximate relative lunar exposure (range 0 to 1
#'   depending on lat argument).
#' @references
#' Krisciunas, K., & Schaefer, B. E. (1991). A model of the brightness of
#' moonlight. Publications of the Astronomical Society of the Pacific, 103,
#' 1033. https://doi.org/10.1086/132921
#' @examples
#'lunar_light_exposure(date_of_interest = seq(from = as.Date("1970-01-01"),
#'                                            to = as.Date("1970-06-30"),
#'                                            by = "day"),
#'                     lat = 53.932458,
#'                     lon = -9.575556)
#' @export
lunar_light_exposure <- function(date_of_interest,
                                 lat,
                                 lon,
                                 log_result = TRUE){

  previous_day = date_of_interest - 1

  sun_times = suncalc::getSunlightTimes(date = c(previous_day, date_of_interest),
                                        lat = lat,
                                        lon = lon)

  # extract nautical minutes sequence from sunset to sunrise
  night_mins_seq_1 <- sun_times[sun_times$date == previous_day,]$sunset
  night_mins_seq_2 <- sun_times[sun_times$date == date_of_interest,]$sunrise

  night_mins_seq <- mapply(FUN = seq,
                           from = night_mins_seq_1,
                           to = night_mins_seq_2,
                           by = (60))

  # extract moon angle (altitude in radians)
  single_night_moon_alt <- mapply(FUN = suncalc::getMoonPosition,
                                  date = night_mins_seq, lat = lat, lon = lon, keep = "altitude",
                                  SIMPLIFY = FALSE)

  # extract altitude elements
  single_night_moon_alt_alt <- lapply(X = single_night_moon_alt, FUN = "[[","altitude")

  # peak moon altitude during night (radians)
  peak_moon_alt = mapply(FUN = max, single_night_moon_alt_alt)

  # proportion of (half hours) of night moon is above horizon
  prop_night_moon_above_horizon = lapply(X = single_night_moon_alt_alt,
                                         FUN = function(x){length(x[x > 0])/length(x)})
  prop_night_moon_above_horizon_vec <- do.call("c",prop_night_moon_above_horizon)

  # extract time of middle of solar night (nadir)
  nadir_time = sun_times[sun_times$date == date_of_interest,]$nadir

  # moon fraction illuminated at nadir
  illu_fraction_nadir <- suncalc::getMoonIllumination(date = nadir_time, keep = c("fraction"))[["fraction"]]

  # index
  if(log_result == TRUE){
  exposure <- log(((sin(peak_moon_alt)) * prop_night_moon_above_horizon_vec * (illu_fraction_nadir^4)+0.00001))
  result <- exposure + (abs(min(exposure)))
  }

  if(log_result == FALSE){
  result <- (sin(peak_moon_alt)) * prop_night_moon_above_horizon_vec * (illu_fraction_nadir^4)
  }

  return(result)
}
