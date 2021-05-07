#' A wrapper function for GR4J hydrologic model
#'
#' @description This function generates discharge predictions for a specified
#' period given a series of arguments containing warm-up and target run
#' periods and model parameters for GR4J (Coron et al., 2020).
#'
#' @param met_data A data.frame with headers in order: "date","pr","petH".
#' @param warm_up_dates_range Two concatenated dates.
#' @param run_dates_range Two concatenated dates.
#' @param GR4J_params A numeric vector containing GR4J parameters.
#' @param ... Other arguments to nested functions.
#' @param log_transform natural log transform discharges? TRUE or FALSE
#' @return A two column data.frame with headers: "date" and "Qsim".
#' @references
#' Coron, L., Delaigue, O., Thirel, G., Perrin, C., & Michel, C. (2020). AirGR:
#' Suite of GR Hydrological Models for Precipitation-Runoff Modelling. In R
#' News. https://doi.org/10.15454/EX11NA
#' @examples
#' \dontrun{
#' met <- fishcastr::grid_ERA5_1979_2019_Jan_bc
#' data_met <-  fishcastr::convert_grid_to_dataframe(grid_obj = met)[, -2]
#' names(data_met)[which(names(data_met) == "dates1")] <-  "date"
#' hydrologic_model(met_data = data_met[, c("date", "pr", "petH")] ,
#'                  warm_up_dates_range = c(as.Date("1979-01-01"),
#'                                         as.Date("1980-12-31")),
#'                  run_dates_range = c(as.Date("1981-01-01"),
#'                                     as.Date("2019-01-31")),
#'                  GR4J_params = as.numeric(fishcastr::GR4J_Burr_params_ERA5_bcc))
#'                  }
#' @export
hydrologic_model <- function(met_data,
                             warm_up_dates_range,
                             run_dates_range,
                             GR4J_params,
                             log_transform = FALSE,
                             ...){

  met_data$date = as.POSIXct(met_data$date, tz = 'UTC')

  # set up GR4J model inputs
  inputs = airGR::CreateInputsModel(airGR::RunModel_GR4J,
                                    DatesR = met_data$date,
                                    Precip = met_data$pr,
                                    PotEvap = met_data$petH)

  # set options for warm-up and run period
  # warm-up dates and data index
  warm_up_date_range = c(as.POSIXct(warm_up_dates_range[1],tz = 'UTC'),
                         as.POSIXct(warm_up_dates_range[2],tz = 'UTC'))
  run_date_range = c(as.POSIXct(run_dates_range[1],tz = 'UTC'),
                     as.POSIXct(run_dates_range[2],tz = 'UTC'))
  run_period_index = which(met_data[,1] >= run_date_range[1] & met_data[,1] <= run_date_range[2])
  warm_up_index = which(met_data[,1] >= warm_up_date_range[1] & met_data[,1] <= warm_up_date_range[2])

  opts = airGR::CreateRunOptions(airGR::RunModel_GR4J,
                                 InputsModel = inputs,
                                 IndPeriod_Run = run_period_index,
                                 IndPeriod_WarmUp = warm_up_index)

  # run model
  mod1 = airGR::RunModel_GR4J(InputsModel = inputs,
                              RunOptions = opts,
                              Param = GR4J_params)

  # extract modelled discharge
  if(log_transform == FALSE){
  data_discharge = data.frame("date" = as.Date(mod1$DatesR), "discharge" = mod1$Qsim)
  }

  if(log_transform == TRUE){
  data_discharge = data.frame("date" = as.Date(mod1$DatesR), "ln_discharge" = log(mod1$Qsim))
  }

  return(data_discharge)
}
