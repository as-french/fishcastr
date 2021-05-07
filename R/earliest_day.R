#' Find the year day of the earliest biofix in the time series
#'
#' @description This function takes a dataset of dates, temperatures, biological
#'   year days, biological years, and ordinal year days and returns the earliest
#'   ordinal year day on which a thermal biofix was reached.
#'
#' @param dataset A dataframe containing dates, temperatures, biological year
#'   day, biological year, calendar year day
#' @param bio_date_day Header of column in dataset that contains biological year
#'   days.
#' @param calendar_date Header of column in dataset that contains calendar
#'   dates.
#' @param start_previous_calendar_year Does the biological year start during the
#'   previous calendar year (e.g., anadromous brown trout, which migrate in
#'   spring following smoltification which may begin during previous calendar
#'   year)? Boolean.
#' @param ... Additional arguments to nested functions.
#' @return A single year day for the earliest calendar year day on which a
#'   biofix occured.
#' @export
earliest_day <- function(dataset, calendar_date, bio_date_day, start_previous_calendar_year, ...){
  y_days <- lubridate::yday(dataset[c(calendar_date)][dataset[c(bio_date_day)] == 1])
  date_days <- dataset[c(calendar_date)][dataset[c(bio_date_day)] == 1]

  # e.g., for eels
  if(start_previous_calendar_year == FALSE){
    eel_year_start_days <- data.frame(y_days,date_days)
    eel_year_start_days <- eel_year_start_days[complete.cases(eel_year_start_days),]
    sorted_eel_year_start_days <- eel_year_start_days[order(eel_year_start_days$y_days, decreasing = FALSE),] # sort data by titre
    rownames(sorted_eel_year_start_days) <- sorted_eel_year_start_days["date_days"][,1]
    earliest_start_date_eel_year_yday <- sorted_eel_year_start_days[1,1, drop = FALSE] # 120th day of the year
  }

  # e.g., for salmon smolts
  if(start_previous_calendar_year == TRUE){
    #remove dates from first half of calendar year
    eel_year_start_days <- data.frame(y_days,date_days)
    # append column with separate months
    eel_year_start_days$month <- lubridate::month(eel_year_start_days$date_days)
    # filter out dates in JFMAMJ
    #eel_year_start_days_subset <- subset(eel_year_start_days, month >=7)
    eel_year_start_days_subset <- eel_year_start_days[eel_year_start_days$month >=7,]
    sorted_eel_year_start_days <- eel_year_start_days_subset[order(eel_year_start_days_subset$y_days, decreasing = FALSE),] # sort data by titre
    rownames(sorted_eel_year_start_days) <- sorted_eel_year_start_days["date_days"][,1]
    earliest_start_date_eel_year_yday <- sorted_eel_year_start_days[1,1, drop = FALSE] # 120th day of the year
  }
  return(earliest_start_date_eel_year_yday)
}
