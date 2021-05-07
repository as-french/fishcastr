#' Delineate ordinal biological year days relative to earliest biofix in time
#' series
#'
#' @description This function takes a dataset of dates, temperatures, biological
#'   year days, biological years, and ordinal year days and returns a vector of
#'   biological year days relative to the earliest ordinal year day on which a
#'   thermal biofix was reached in the dataset. Thus, a inter-annually
#'   comparable biological year day is calculated that can be used to compare
#'   the timing of ecological events (e.g, migration phenology).
#'
#' @param dataset A dataframe cintaining dates, temperatures, iological year
#'   day, biological year, calendar year day
#' @param bio_date_day Header of column in dataset that contains biological year
#'   days.
#' @param calendar_date Header of column in dataset that contains calendar
#'   dates.
#' @param bio_year Header of column in dataset containing biological years.
#' @param year_day Header of column in dataset containing calendar ordinal year
#'   days.
#' @param start_previous_calendar_year Does the biological year start during the
#'   previous calendar year (e.g., anadromous brown trout, which migrate in
#'   spring following smoltification which may begin during previous calendar
#'   year)? Boolean.
#' @param ... Additional arguments to nested functions.
#' @return A vector of ordinal biological year days relative to earliest biofix
#'   in the time series.
#' @export
days_since_biofix <- function(dataset,
                              bio_date_day,
                              calendar_date,
                              bio_year,
                              year_day,
                              start_previous_calendar_year,
                              ...){

  days_since_biofix_start <- as.vector(numeric(length = nrow(dataset)))
  days_since_biofix_start_positive <- as.vector(numeric(length = nrow(dataset)))
  # subtract 120 days from year day for all thermal years to calculate number of days since the 120th day of the year
  for(i in list(c(unique(dataset[c(bio_year)])))[[1]][[1]]){
    #days_since_biofix_start[dataset[c(bio_year)] == i] <- dataset[c(year_day)][dataset[c(bio_year)] == i] - sort(lubridate::yday(dataset[c(calendar_date)][dataset[c(bio_date_day)] == 1]), decreasing = FALSE)[1]
    days_since_biofix_start[dataset[c(bio_year)] == i] <- dataset[c(year_day)][dataset[c(bio_year)] == i] -  as.integer(earliest_day(dataset, calendar_date, bio_date_day, start_previous_calendar_year)) + 1
  }


  # identify row of first negative day since earliest start day and add 365 to it and all subsequence values (which will also be negative..)
  #i = 2019
  #length(days_since_biofix_start[dataset[c(bio_year)] == i])
  for(i in list(c(unique(dataset[c(bio_year)])))[[1]][[1]]){

    # if at least one value is less than zero do loop below
    # if not, leave values as they are...
    if(any(days_since_biofix_start[dataset[c(bio_year)] == i] < 0)){

      for(j in 1:length(days_since_biofix_start[dataset[c(bio_year)] == i])){

        if(j < min(which(days_since_biofix_start[dataset[c(bio_year)] == i]< 0))){
          days_since_biofix_start_positive[dataset[c(bio_year)] == i][j] <- days_since_biofix_start[dataset[c(bio_year)] == i][j]
        }# this one is fine

        if(j >= min(which(days_since_biofix_start[dataset[c(bio_year)] == i]< 0))){
          # if leap year
          if((i %% 4) == 0){
            days_since_biofix_start_positive[dataset[c(bio_year)] == i][j] <- days_since_biofix_start[dataset[c(bio_year)] == i][j]+366
          }
          # if not leap year
          if((i %% 4) != 0){
            days_since_biofix_start_positive[dataset[c(bio_year)] == i][j] <- days_since_biofix_start[dataset[c(bio_year)] == i][j]+365
          }
        }
      }
    }

    if(!any(days_since_biofix_start[dataset[c(bio_year)] == i] < 0)){
      days_since_biofix_start_positive[dataset[c(bio_year)] == i] <- days_since_biofix_start[dataset[c(bio_year)] == i]
    }

  }
  return(days_since_biofix_start_positive)
}
#--------------------------------------------------------------------------------------------------#
