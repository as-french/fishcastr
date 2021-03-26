#' Delineate biological years defined by a photoperiod based biofix
#'
#' This function takes a column of dates and a latitude and longitude to
#' facets/delineates biological years based on photoperiod conditions that
#' define a "biofix". For example, the start (biofix) of the Atlantic salmon
#' (smolt) year occurs on the shortest day of the previous calendar year.
#'
#' @param dates Vector of dates in class "Date".
#' @param latitude Latitude in decimal degrees WGS84.
#' @param longitude Longitude in decimal degrees WGS84.
#' @param retain_photoper Should the column of daylengths be returned as part of
#'   the result? Boolean.
#' @param start_previous_year Does the biological year start during the previous
#'   calendar year? Boolean.
#' @param shortest_day Is the biofix defined by the shortest day of the year?
#'   Boolean.
#' @param yday_head Header to be assigned to a biological year day column (e.g.,
#'   salmonid_yday).
#' @param bio_year_head Header to be assigned to a biological year column (e.g.,
#'   salmonid_year).
#' @return A dataframe consisting of original date column in addition to
#'   columns: photoperiod (if specified), biological year day, biological year.
#' @export
#' @examples
#' bio_year_photoper(dates = seq(from = as.Date("1981-01-01"),
#'                              to = as.Date("1984-01-31"), by = "day"),
#'                  latitude = 53.932458,
#'                  longitude = -9.575556,
#'                  start_previous_year = TRUE,
#'                  shortest_day = TRUE,
#'                  yday_head = "salmonid_yday",
#'                  bio_year_head = "salmonid_year")
bio_year_photoper <- function(dates,
                              latitude,
                              longitude,
                              retain_photoper = TRUE,
                              start_previous_year = FALSE,
                              shortest_day = FALSE,
                              yday_head,
                              bio_year_head){
  # define variables


  # note default start_previous_year = TRUE for salmonids which smoltify beginning after the winter solstice

  # create a temporary dataframe
  df_photo_bio_year <- data.frame(date = dates, photoper = NA, bio_year_head = NA, yday_head = NA)
  colnames(df_photo_bio_year) <- c("date", "photoper", yday_head, bio_year_head)

  # alternative method with suncalc (better with leap years...?.... probably)
  sunrise_set <- suncalc::getSunlightTimes(date = dates,
                                  lat = latitude,
                                  lon = longitude,
                                  keep = c("sunrise", "sunset"),
                                  tz = "UTC")
  daylength_Burr <- as.numeric(sunrise_set$sunset - sunrise_set$sunrise)

  # # ----------------------------------------------------------------------- #
  # actual and relative photperiod (daylength relative to shortest day)
  # # ----------------------------------------------------------------------- #
  df_photo_bio_year$photoper <- daylength_Burr
  df_photo_bio_year$photoper_rel <- daylength_Burr - min(daylength_Burr)

  # # ----------------------------------------------------------------------- #
  # DEFINE BIOLOGICAL YEAR BASED ON PHOTOPERIOD (NEW VERSION FOR SUNCALC BASED DAY LENGTH)
  # # ----------------------------------------------------------------------- #
  # make column of year
  df_photo_bio_year$year <- lubridate::year(df_photo_bio_year$date)
  start_year <- min(unique(df_photo_bio_year$year))
  # make column of obs_id
  df_photo_bio_year$obs_id <- seq(1:length(df_photo_bio_year$date))

  # e.g., for salmonids winter solstice previous year (or even eels summer solsice previous year...)
  if(start_previous_year == TRUE){

    # identify obs number of start day of each bio year as the minimum relative day length of each year (note some years will have more than one day with minimum day length, but which.min returns the "first" min found)
    # winter solstice
    if(shortest_day == TRUE){
      list_start_days <- vector(length = 0)
      for(i in start_year:(max(unique(lubridate::year(dates)))-1)){
        list_start_days_i <- df_photo_bio_year$obs_id[df_photo_bio_year$year == i][which.min(df_photo_bio_year$photoper_rel[df_photo_bio_year$year == i])]
        list_start_days <- c(list_start_days,list_start_days_i)
      }
    }

    if(shortest_day == FALSE){
      list_start_days <- vector(length = 0)
      for(i in start_year:(max(unique(lubridate::year(dates)))-1)){
        list_start_days_i <- df_photo_bio_year$obs_id[df_photo_bio_year$year == i][which.max(df_photo_bio_year$photoper_rel[df_photo_bio_year$year == i])]
        list_start_days <- c(list_start_days,list_start_days_i)
      }
    }

    #define bio years
    bio_years <- seq(from = (start_year + 1), to = ((start_year + 1) + length(list_start_days)-1), by = 1)

    # define a table to view  obs id number of start date of each bio year
    summary_tab <- data.frame(start_date = c(1, list_start_days), bio_year = c(start_year, bio_years))

    # for each row in the summary table, fill column in original dataset with bio year day sequence
    for(i in 1:(nrow(summary_tab))){
      # re define first year start date to account for missing data from previous year (i.e., salmonid year does not start on 1 jan, it starts on winter solstice the previous year...)
      if(i == 1){
        df_photo_bio_year[[bio_year_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- summary_tab[i,2]
        df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- seq(to = 365,
                                                                                       by = 1,
                                                                                       length.out = length(df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)]))
      }

      # for all other bio years, use summary table obs id start date to populationfish bio year day column in  df
      if(i >= 2 & i < nrow(summary_tab)){
        df_photo_bio_year[[bio_year_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- summary_tab[i,2]
        df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- seq(from = 1,
                                                                                       to = length(df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)]),
                                                                                       by = 1)
      }

      # for final bio year in summary table
      if(i == nrow(summary_tab)){
        df_photo_bio_year[[bio_year_head]][summary_tab[i,1]:nrow(df_photo_bio_year)] <- summary_tab[i,2]
        df_photo_bio_year[[yday_head]][summary_tab[i,1]:nrow(df_photo_bio_year)] <- seq(from = 1,
                                                                                        to = length(df_photo_bio_year[[yday_head]][summary_tab[i,1]:nrow(df_photo_bio_year)]),
                                                                                        by = 1)
      }
    }

    # before exporting, just check if final year goes over 365 days, and if it does, make it year plus 1 before deleting

  }

  # e.g., for eels summer solstice or even (spring equinox) present year
  if(start_previous_year == FALSE){

    # identify obs number of start day of each bio year as the minimum relative day length of each year (note some years will have more than one day with minimum day length, but which.min returns the "first" min found)
    # winter solstice
    if(shortest_day == TRUE){
      list_start_days <- vector(length = 0)
      for(i in start_year:(max(unique(lubridate::year(dates)))-1)){
        list_start_days_i <- df_photo_bio_year$obs_id[df_photo_bio_year$year == i][which.min(df_photo_bio_year$photoper_rel[df_photo_bio_year$year == i])]
        list_start_days <- c(list_start_days,list_start_days_i)
      }
    }

    if(shortest_day == FALSE){
      list_start_days <- vector(length = 0)
      for(i in start_year:(max(unique(lubridate::year(dates)))-1)){
        list_start_days_i <- df_photo_bio_year$obs_id[df_photo_bio_year$year == i][which.max(df_photo_bio_year$photoper_rel[df_photo_bio_year$year == i])]
        list_start_days <- c(list_start_days,list_start_days_i)
      }
    }

    #define bio years
    bio_years <- seq(from = (start_year + 1), to = ((start_year + 1) + length(list_start_days)-1), by = 1)
    #View(bio_years)
    # define a table to view  obs id number of start date of each bio year
    summary_tab <- data.frame(start_date = c(1, list_start_days), bio_year = c(start_year-1, bio_years-1))
    #View(summary_tab)
    # for each row in the summary table, fill column in original dataset with bio year day sequence

    for(i in 1:(nrow(summary_tab))){
      # re define first year start date to account for missing data from previous year (i.e., salmonid year does not start on 1 jan, it starts on winter solstice the previous year...)
      if(i == 1){
        df_photo_bio_year[[bio_year_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- summary_tab[i,2]
        df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- seq(to = 365,
                                                                                       by = 1,
                                                                                       length.out = length(df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)]))
      }
      # for all other bio years, use summary table obs id start date to populationfish bio year day column in  df
      if(i >= 2 & i < nrow(summary_tab)){
        df_photo_bio_year[[bio_year_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- summary_tab[i,2]
        df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)] <- seq(from = 1,
                                                                                       to = length(df_photo_bio_year[[yday_head]][summary_tab[i,1]:(summary_tab[i+1,1]-1)]),
                                                                                       by = 1)
      }
      # for final bio year in summary table
      if(i == nrow(summary_tab)){
        df_photo_bio_year[[bio_year_head]][summary_tab[i,1]:nrow(df_photo_bio_year)] <- summary_tab[i,2]
        df_photo_bio_year[[yday_head]][summary_tab[i,1]:nrow(df_photo_bio_year)] <- seq(from = 1,
                                                                                        to = length(df_photo_bio_year[[yday_head]][summary_tab[i,1]:nrow(df_photo_bio_year)]),
                                                                                        by = 1)
      }
    }

  }

  # before exporting, just check if final year goes over 365 days, and if it does, make it year plus 1 before deleting
  if(any(df_photo_bio_year[[yday_head]]>=367)){
    df_photo_bio_year[[bio_year_head]] <- ifelse(df_photo_bio_year[[yday_head]]>=367,
                                                 max(df_photo_bio_year[[bio_year_head]])+1,
                                                 df_photo_bio_year[[bio_year_head]])
    # then drop last year
    df_photo_bio_year <- df_photo_bio_year[df_photo_bio_year[[bio_year_head]] != max(df_photo_bio_year[[bio_year_head]]),]
  }

  if(retain_photoper == FALSE){
    result <- df_photo_bio_year[,-c(which(colnames(df_photo_bio_year) == "photoper"),
                                    which(colnames(df_photo_bio_year) =="photoper_rel"),
                                          which(colnames(df_photo_bio_year) =="obs_id"),
                                                which(colnames(df_photo_bio_year) =="year"))]
    return(result)
  }
  result <- df_photo_bio_year[,-c(which(colnames(df_photo_bio_year) =="photoper_rel"),
                                  which(colnames(df_photo_bio_year) == "obs_id"),
                                  which(colnames(df_photo_bio_year) == "year"))]
  return(result)
}
