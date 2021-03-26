#' Air to water temperature model.
#'
#' This function builds on the three-parameter equation defined by A. Ducharne
#' (2008). Ducharne's equation describes the statistical relationship between
#' lagged rolling average air temperature and water temperature in streams
#' (whereby lag period is a constant in days). We extended Ducharne's equation
#' to four parameters to account for stratification in lakes, whereby the lag
#' period between air temperature and water temperature shortens during the
#' summer owing to the shallower mixed layer depth during stratification (see
#' Piccolroaz et al. (2013) for further discussion) and Calderó-Pascual et al.
#' (2020), for details of stratification in relation to Lough Feeagh,
#' Burrishoole, Ireland. We also include a nested bias adjustment function that
#' can be adapted for other settings - this adjustment multiplication factor
#' depends on year day and reduces residual hysteresis.
#'
#' @param Ta A numeric vector of air temperatures.
#' @param Yday A numeric vector of year days (derivable from lubridate::yday).
#' @param A A numeric that describes the gradient, A, in the "Tw = A * ave(Ta,
#'   Lag) + B" line.
#' @param ac A numeric that defines the amplitude and width of lag function;
#'   whereby as ac increases, the lag time between air temperature and water
#'   temperature during summer decreases; similarly as ac increases, the width
#'   fo the lag function narrows defining a lag time for the
#'   summer/stratification period that contrasts more sharply with
#'   non-stratified period. ac represents the difference in lag time between
#'   strongly stratified and fully mixed periods.
#' @param b A numeric defining the mid-summer point where lag period between air
#'   and water temperatures is shortest.
#' @param B A constant that defines the intercept in "Tw = A * ave(Ta, Lag) +
#'   B".
#' @return A numeric vector of estimated water temperatures.
#' @references
#' Calderó-Pascual, M., de Eyto, E., Jennings, E., Dillane, M., Andersen, M. R.,
#' Kelly, S., Wilson, H. L., & McCarthy, V. (2020). Effects of Consecutive
#' Extreme Weather Events on a Temperate Dystrophic Lake: A Detailed Insight
#' into Physical, Chemical and Biological Responses. Water, 12(5), 1411.
#' https://doi.org/10.3390/w12051411
#'
#' Ducharne, A. (2008). Importance of stream temperature to climate change
#' impact on water quality. Hydrology and Earth System Sciences, 12(3), 797–810.
#'
#' Piccolroaz, S., Toffolon, M., & Majone, B. (2013). A simple lumped model to
#' convert air temperature into surface water temperature in lakes. Hydrology
#' and Earth System Sciences, 17(8), 3323–3338.
#' https://doi.org/10.5194/hess-17-3323-2013
#' @export
air_to_water_model <- function(Ta, Yday, A = 1, ac, b, B) {

  # bias adjustment factor (residual hysteresis correction factor)
#  multip_factor <- function(ydays, mu, stdev){
#    result <- 0.99 + 0.4*((ydays-mu)/((1000/(stdev))^2))*(stdev)*exp(-1*((ydays - (mu))^2)/(2*((1000/(stdev))^2)))
#    return(result)
#  }

  # bias adjustment factor (residual hysteresis correction factor)
  multip_factor <- function(ydays, magn, phi){
    result <- 0.985 + magn*cos(4*pi*(2^(-1*(ydays/365.25))) + phi)
    return(result)
  }

  Tw <- (multip_factor(ydays = Yday,magn = 0.05,phi = 1.25*pi))*((A) * (data.table::frollmean(
    x = Ta,
    n = as.vector(round(30 - ((
      ac
    ) * exp(
      -1 * ((Yday - (b)) ^ 2) / (2 * ((400 / (
        ac
      )) ^ 2))
    )))),
    align = "right",
    fill = NA,
    adaptive = TRUE
  )) + (B))
  return(Tw)
}
