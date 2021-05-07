#' Calculate rate of change "delta" for a variable
#'
#' @description This function calculates a daily gradient defined by a specified
#'   number of points. The user specifies the number of days for which the
#'   gradient will be calculated (e.g., 10 days). The number of days specified
#'   relates to the assumed perceptive abilities of diadromous fishes. For
#'   example, one might expect eels to migrate in response to decreasing
#'   temperature, but eels might only perceive a decrease in temperature if is
#'   occurs over several days or weeks; i.e., eels might not migrate in response
#'   to a decrease in temperature from one day to the next, but rather respond
#'   to an average rate of decrease in temperature that has occurred over a
#'   specified period.
#'
#' @importFrom stats .lm.fit coef
#' @param y A vector (e.g., daily water temperature).
#' @param react_time Number of days of changing variable perceivable by fish.
#' @param ... Additional arguments to nested functions.
#' @return A vector of daily delta values.
#' @examples
#' delta_var(y = rnorm(n = 365, mean = 10 + 10*sin((1:365*(pi/365.25)) + (1.5*pi*(1/365.25))),
#'                     sd = 2),
#'           react_time = 20)
#' @export
delta_var <- function(y, react_time, ...){
orig_y <- y
  # remove any NAs at start of modelled series
  if(any(is.na(y))){
  startingNas <- which(!complete.cases(y))
    if(all(diff(startingNas)==1) & startingNas[1] == 1){
      y <- y[complete.cases(y)]
    }
  }

  if(react_time >= 2){
  roll_delta <- function(vec) {
    a <- stats::coef(stats::.lm.fit(cbind(1, seq(vec)), vec))[2]
    return(a)
  }

  delta_y_return <- data.table::frollapply(x = y,
                                           n = react_time,
                                           FUN = roll_delta,
                                           fill = NA,
                                           align = "right")
  if(any(is.na(orig_y))){
  result <- c(rep(NA, times = length(startingNas)),delta_y_return)
  }

  if(!any(is.na(orig_y))){
    result <- delta_y_return
  }

  return(result)
  }

  if(react_time == 1) {

    y_vec_NA <- as.vector(y) # will be zeros (as also in cumulative rainfall function; i.e., we cannot use first few days of dataset)
    y_vec <- y_vec_NA[complete.cases(y_vec_NA)]# remove NAs from y vector

    delta_y <- numeric(length = length(y_vec))
    for(i in (react_time+1):length(y_vec)){
        delta_y[i] <- y_vec[i] - y_vec[i-1]
    }

    if(any(is.na(orig_y))){
    delta_y_return <- c(rep(NA, times = length(startingNas)),delta_y,rep(NA, times = length(y_vec_NA)-length(y_vec)))# put NAs back at end of vector
    }
    if(!any(is.na(orig_y))){
      delta_y_return <- c(delta_y,rep(NA, times = length(y_vec_NA)-length(y_vec)))
    }

    return(delta_y_return)
  }
}
