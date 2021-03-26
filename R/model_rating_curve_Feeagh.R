#' A rating curve model for the outflows of Lough Feeagh, Burrishoole, Co. Mayo,
#' Ireland.
#'
#' A bbmle::mle2 fitted rating curve relating the water level of Lough Feeagh,
#' Newport, Mayo, Ireland, to the combined discharge from the two Lough Feeagh
#' outflows (Mill Race and Salmon leap). Measurements were collected and data
#' collated by Sean Kelly (Marine Institute, Ireland) between 2016 and 2018 and
#' are also available in this package.
#'
#' @format A bbmle::mle2 model with three estimated coefficients: \describe{
#'   \item{c}{c, constant}
#'   \item{Beta}{Beta, constant}
#'   \item{st_dev}{variance parameter, estimated using gamma conditional
#'   distribution of y} ... }
#' @source
#'   \url{https://doi.org/10.1371/journal.pone.0235963}
#'
"model_rating_curve_Feeagh"
