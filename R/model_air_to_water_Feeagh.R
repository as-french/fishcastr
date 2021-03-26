#' A model object for estimating water temperature from air temperature.
#'
#' A bbmle::mle2 object calibrated using Lough Feeagh 2m temperature using
#' downscaled ERA5 air temperature. Five parameters are included in the model
#' fitting procedure (four of which are required for running the air to water
#' temperature model for the outflow of the Burrishoole catchment, Ireland. In
#' addition to sigma (exp(sigma) is the standard deviation of Gaussian
#' conditional distribution of the response), parameters are:
#'
#' (i) A, describes a gradient, A, in the "Tw = A * ave(Ta, (Lag ~ f(Yday))) +
#' B" model. We fix A at 1;
#'
#' (ii) ac defines the amplitude and width of lag function that roughly mirrors
#' the typical temporal progression of Schmidt stability in Lough Feeagh;
#' whereby as ac increases, the lag time between air temperature and water
#' temperature during summer decreases; similarly as ac increases, the width of
#' the lag function narrows defining a lag time for the summer/stratification
#' period that contrasts more sharply with non-stratified period. ac represents
#' the difference in lag time between strongly stratified and fully mixed
#' periods;
#'
#' (iii) b defines a late spring point where lag period between air and water
#' temperatures is shortest on average across the time series;
#'
#' (iv) B defines the intercept in "Tw = A * ave(Ta, (Lag ~ f(Yday))) + B".
#'
#' The model for Burrishoole was calibrated using bbmle::mle2 using water
#' temperature measurements recorded at 2m depth by the Automatic Water Quality
#' Monitoring Station (AWQMS) on Lough Feeagh since 2004, and downscaled ECMWF
#' ERA5 reanalysis 2m air temperature data.
#'
#' A sinusoidal bias adjustment multiplication factor that varies with year day
#' was included to reduce residual hysteresis present in the basic lagged air
#' temperature model. Further details on calibration can be found within the
#' fishcastr package in /data-raw/data_air_to_water_model.R.
#'
#' @format A bbmle::mle2 S4 class object: \describe{ \item{model object}{air to
#'   water model object, bbmle:mle2 object} ... }
#' @source \url{http://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.3757}
#'
#'
"model_air_to_water_Feeagh"
