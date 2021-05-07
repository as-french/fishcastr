#' Define ordinal biological year days based on a vector of temperature based
#' biological years
#'
#' @description This function takes a vector of thermally defined biological
#'   years and creates a new vector of biological year days.
#'
#' @param thermal_year Vector of thermally defined biological years.
#' @return A vector of biological year days.
#' @export
thermal_year_days <- function(thermal_year){
  therm_year <- as.vector(thermal_year)
  thermal_year_day <- as.vector(numeric(length = length(thermal_year)))
  for(i in min(therm_year):max(therm_year)){
    thermal_year_day[therm_year == i] <- seq.int(length(thermal_year[therm_year == i]))
  }
  return(thermal_year_day)
}
