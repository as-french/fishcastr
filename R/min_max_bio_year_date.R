#' Extract start and end dates of biological/forecast years
#'
#' @description This function extracts the start and end dates of biological
#'   years for use in summary tables. For example, to summarise European eel
#'   "silvering" years.
#'
#' @param dates A vector of Dates.
#' @param bio_years A vector of biological years corresponding to dates
#'   argument.
#' @param biofix_day A vector containing biological year days corresponding to
#'   dates argument.
#' @return A table containing the start and end date of all biological years
#'   delineated in the input data.
#' @examples
#' \dontrun{
#'
#' # ----------------------------------------------------------- #
#' # IMPORT RAW AIR TEMP AND AIR TO WATER TEMP MODEL -----
#' # ----------------------------------------------------------- #
#' data_air_temp <-
#'   fishcastr::convert_grid_to_dataframe(grid_obj = fishcastr::grid_ERA5_1979_2019_Jun_bc)[,-2]
#' names(data_air_temp) <-
#'   c("date", "tas_era5_bcc", "pr_era5_bcc", "petH_era5_bcc")
#' air_to_water_params <-
#'   fishcastr::air_to_water_Feeagh_params_ERA5_bcc
#' data_water_temp <- data.frame(
#'   "date" = data_air_temp$date,
#'   "water_temp" = fishcastr::air_to_water_model(
#'     Ta = data_air_temp$tas_era5_bcc,
#'     Yday = lubridate::yday(data_air_temp$date),
#'     A = air_to_water_params$A,
#'     ac = air_to_water_params$ac,
#'     b = air_to_water_params$b,
#'     B = air_to_water_params$B
#'   )
#' )
#'
#' # ----------------------------------------------------------- #
#' # DEFINING FORECAST "THERMAL" YEARS, YEAR DAYS AND DAYS SINCE COMMON BIOFIX
#' ----
#' # ----------------------------------------------------------- #
#' eel_bio_year <-
#'   fishcastr::bio_year_therm(
#'     mean_temp = data_water_temp$water_temp,
#'     dates = data_water_temp$date,
#'     biofix_temp = 11,
#'     min_no_days_above_biofix = 10,
#'     increasing_temp = TRUE,
#'     yday_head = "eel_yday",
#'     bio_year_head = "eel_year",
#'     days_since_earliest_biofix_head = "eel_yday_biofix",
#'     incomplete_first_year = 1978,
#'     start_previous_calendar_year = FALSE
#'   )
#'
#' # ----------------------------------------------------------- #
#' # SAVE EEL BIOYEAR DATA FOR SUPPLEMENTARY TABLE ----
#' # ----------------------------------------------------------- #
#' # Create vignette sub directory ----
#' tab_eel <-
#'   knitr::kable(
#'     fishcastr::min_max_bio_year_date(
#'       dates = eel_bio_year$date,
#'       bio_years = eel_bio_year$eel_year,
#'       biofix_day = eel_bio_year$eel_yday_biofix
#'     )
#'   )
#'
#' kableExtra::kable_styling(kable_input = tab_eel, position = "center")
#'
#'                  }
#'@export
min_max_bio_year_date <- function(dates,
                                  bio_years,
                                  biofix_day){
  # check for and remove NA years
  df <- data.frame(dates = dates,
                   bio_years = bio_years,
                   biofix_day = biofix_day)
  df <- df[complete.cases(df),]
  dates <- as.vector(df$dates)
  bio_years <- as.vector(df$bio_years)
  biofix_day <- as.vector(df$biofix_day)

  unique_bio_years <- unique(bio_years)
  min_max <- matrix(ncol = 5, nrow = length(unique_bio_years))
  rownames(min_max) <- unique_bio_years
  colnames(min_max) <- c("Forecast year",
                         "Start date",
                         "Start relative to calendar day 124",
                         "End date",
                         "Forecast year length (days)")

  min_max_df <- as.data.frame(min_max)
  for(i in 1:length(unique_bio_years)){
    min_max_df[i,] = c(unique_bio_years[i],
                       min(dates[bio_years == unique_bio_years[i]]),
                       min(biofix_day[bio_years == unique_bio_years[i]]),
                       max(dates[bio_years == unique_bio_years[i]]),
                       (max(dates[bio_years == unique_bio_years[i]])-min(dates[bio_years == unique_bio_years[i]])))
    #print(c(min(dates[bio_years == unique_bio_years[i]]), max(dates[bio_years == unique_bio_years[i]])))
  }
  min_max_df[,2] <- as.Date(min_max_df[,2],origin = "1970-01-01")
  #  min_max_df[,3] <- yday(min_max_df[,2])
  #  min_max_df[,3] <- min(biofix_day)

  min_max_df[,4] <- as.Date(min_max_df[,4],origin = "1970-01-01")
  row.names(min_max_df) <- NULL
  return(min_max_df)
}
