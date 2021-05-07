#' Delineate biological years defined by a biofix temperature
#'
#' @description This function takes a date and temperature column and creates a
#'   new column that facets/delineates biological years based on thermal
#'   conditions that define a "biofix". For example, the function can be used to
#'   identify the start (biofix) of the European eel biological year, which
#'   might occur when temperatures are rising and have exceeded X degrees
#'   Celsius for a minimum of Y consecutive days.
#'
#' @param mean_temp Vector of daily mean temperatures in degrees Celsius.
#' @param dates Vector of dates in class "Date".
#' @param biofix_temp Biofix temperature.
#' @param min_no_days_above_biofix Minimum number of days for which biofix
#'   temperature must be exceeded
#' @param increasing_temp Is temperature rising; i.e., does the biofix occur in
#'   spring? Boolean. Note FALSE implies biofix occurs as temperatures are
#'   falling (i.e., in autumn) - FALSE is experimental.
#' @param yday_head Header to be assigned to year day column.
#' @param bio_year_head Header to be assigned to biological year column.
#' @param days_since_earliest_biofix_head Header to be assigned to a universal
#'   biological year day column, assuming that biofix temperature is not reached
#'   on the same day every year. Biological year days in this column are
#'   relative to the earliest biofix year day recorded in the time series (e.g.,
#'   01 June 1990; year day 152)
#' @param incomplete_first_year Specified bio year at start of time series for
#'   which data are incomplete.
#' @param start_previous_calendar_year Does the biological year start during the
#'   previous calendar year; e.g., as temperatures are cooling during autumn
#'   (e.g., anadromous brown trout smolts that migrate to sea during spring may
#'   begin downstream movements the previous autumn).
#' @param ... Additional arguments to nested functions.
#' @return A dataframe of original date and temperatures in addition to columns:
#'   year day, biological year, biological year day, comparative biological year
#'   day relative to earliest biofix in the time series.
#' @examples
#' \dontrun{
#' data_water_temp <- data.frame(
#'   "date" = fishcastr::data_ERA5_1979_2019_Jun_bc$date,
#'   "water_temp" = fishcastr::air_to_water_model(
#'     Ta = fishcastr::data_ERA5_1979_2019_Jun_bc$tas,
#'     Yday = lubridate::yday(data_ERA5_1979_2019_Jun_bc$date),
#'     A = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$A,
#'     ac = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$ac,
#'     b = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$b,
#'     B = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$B
#'   )
#' )
#'
#' data_seel_forecast_years <-
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
#'}
#' @export
bio_year_therm <- function(mean_temp,
                           dates,
                           biofix_temp,
                           min_no_days_above_biofix,
                           increasing_temp,
                           yday_head,
                           bio_year_head,
                           days_since_earliest_biofix_head,
                           incomplete_first_year = NULL,
                           start_previous_calendar_year,
                           ...){

  mean_t <-  as.vector(mean_temp)
  years <- as.vector(as.numeric(format(as.Date(dates, format="%Y-%m-%d"),"%Y")))
  unique_years <- unique(as.numeric(format(as.Date(dates, format="%Y-%m-%d"),"%Y")))
  #unique_years
  thermal_year <- as.vector(numeric(length = length(dates)))
  obs_no <- seq.int(length(dates))
  #i = 1970
  #  i = min(unique_years)
  for(i in min(unique_years):(max(unique_years))){
#print(i)
    if(increasing_temp == TRUE){
      ##### increasing temp (e.g., EELS)
      if(any(mean_t[years == i] >= biofix_temp, na.rm = TRUE)){

        biofix_days <- which(mean_t[years == i] >= biofix_temp) # find days on which growth/feeding onset threshold is exceeded and assign to matrix length X
        # build matrix of potential start days
        biofix_days_matrix <- matrix(nrow = min_no_days_above_biofix, ncol = length(biofix_days))
        colnames(biofix_days_matrix) <- biofix_days
        for(l in 1:length(biofix_days)){
          biofix_days_matrix[,l] <- mean_t[years == i][biofix_days[l]:(biofix_days[l]+(min_no_days_above_biofix-1))]
        }

        # remove columns with one or more NAs
        biofix_days_matrix <- biofix_days_matrix[ , colSums(is.na(biofix_days_matrix)) == 0]

        min_start <- matrix(nrow = min_no_days_above_biofix, ncol = 0)
        # use matrix to check for each day above threshold whether subsequent (e.g., 20) days are also all above threshold
        for(k in 1:ncol(biofix_days_matrix)){
          #print(k)
          if(all(biofix_days_matrix[,k] >= biofix_temp)){
            min_start <- cbind(min_start, biofix_days_matrix[,k, drop = FALSE])
          }
        }

        biofix_year_onset <- as.integer(noquote(colnames(min_start[,1, drop = FALSE]))) # earliest obs day after which that day and X subsequent mean temperatures all exceed feeding threshold

        # add X days to this onset day, as the eels will then have experienced X days with temp above Y threshold.
        biofix_obs_no <- obs_no[years == i][biofix_year_onset+min_no_days_above_biofix]

        for(j in 1:length(obs_no[years == i])){
          if(obs_no[years == i][j] >= biofix_obs_no){
            thermal_year[years == i][j] = i
          }
          if(obs_no[years == i][j] < biofix_obs_no){
            thermal_year[years == i][j] = i-1
          }
        }
      } else { for(j in 1:length(obs_no[years == i])){
        thermal_year[years == i][j] = i-1
      }
      }
      ######
    }

    if(increasing_temp == FALSE){
      ##### decreasing temp (e.g., SMOLTS)
      if(any(mean_t[years == i] <= biofix_temp)){

        biofix_days <- which(mean_t[years == i] <= biofix_temp) # find days on which growth onset threshold is exceeded and assign to matrix length X
        # build matrix of potential start days
        biofix_days_matrix <- matrix(nrow = min_no_days_above_biofix, ncol = length(biofix_days))
        colnames(biofix_days_matrix) <- biofix_days
        for(l in 1:length(biofix_days)){
          biofix_days_matrix[,l] <- mean_t[years == i][biofix_days[l]:(biofix_days[l]+(min_no_days_above_biofix-1))]
        }

        # remove columns with one or more NAs
        biofix_days_matrix <- biofix_days_matrix[ , colSums(is.na(biofix_days_matrix)) == 0]

        min_start <- matrix(nrow = min_no_days_above_biofix, ncol = 0)
        # use matrix to check for each day under threshold whether subsequent 10 days are also below dormancy threshold
        for(k in 1:ncol(biofix_days_matrix)){
          if(all(biofix_days_matrix[,k] <= biofix_temp, na.rm = TRUE)){
            min_start <- cbind(min_start, biofix_days_matrix[,k, drop = FALSE])
          }
        }

        # find day of year on which temperatures fall below 10 degrees after summer (after gap > 100)
        gap <- as.vector(integer(length = ncol(min_start)))
        for(p in 1:ncol(min_start)){
          gap[p] <- as.integer(noquote(colnames(min_start)[p+1])) -  as.integer(noquote(colnames(min_start)[p]))
        }

        biofix_year_onset <- as.integer(noquote(colnames(min_start)[which.max(gap) + 1]))
        biofix_obs_no <- obs_no[years == i][biofix_year_onset]

        for(j in 1:length(obs_no[years == i])){
          if(obs_no[years == i][j] >= biofix_obs_no){
            thermal_year[years == i][j] = i+1

          }
          if(obs_no[years == i][j] < biofix_obs_no){
            thermal_year[years == i][j] = i
          }
        }
      } else { for(j in 1:length(obs_no[years == i])){
        thermal_year[years == i][j] = i-1
      }
      }

    }

      }

  # append thermal year ydays and days since earliest biofix to data to return

  # THERMAL YEAR DAYS
  thermal_year_day <- thermal_year_days(thermal_year = thermal_year)

  # CBIND RESULTS IS NOT COMPARABLE TABLE WITH bio_year_photo fuction
  result1 <- data.frame("date" = dates,
                        "meanT" = mean_t,
                        "yday_head" = thermal_year_day,
                        "bio_year_head" = thermal_year,
                        "year_day_cal" = lubridate::yday(dates))

  colnames(result1) <- c("date", "meanT", yday_head, bio_year_head, "yday_cal")

  # subset of only complete years if specified
  if(is.null(incomplete_first_year)){
    data_complete_years <- result1
  }

    if(!is.null(incomplete_first_year)){
  data_complete_years <- result1[which(result1[[bio_year_head]] != incomplete_first_year), ]
}

  # DAYS SINCE EARLIEST BIOFIX
  data_complete_years$days_since_biofix <- days_since_biofix(dataset = data_complete_years,
                                                             bio_date_day = yday_head,
                                                             calendar_date = "date",
                                                             bio_year = bio_year_head,
                                                             year_day = "yday_cal",
                                                             start_previous_calendar_year = start_previous_calendar_year)

  result <- data_complete_years[,-c(5)]

  colnames(result) <- c("date", "meanT", yday_head, bio_year_head, days_since_earliest_biofix_head)

  # label rows NA if no mean t is available...
  result[!complete.cases(result),colnames(result)[-1]] <- NA
  return(result)
}
