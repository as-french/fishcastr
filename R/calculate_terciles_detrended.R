#' Identify year days of recorded quantiles for multi-year count time series
#'
#' @description This function takes vectors of counts, biological years and
#'   biological year days and identifies the year day on which two quantiles
#'   (e.g., 33 and 66) are recorded in multi-year ecological count time series.
#'   In addition, the multi-year moving average expectation of each tercile and
#'   its 95% CI is returned in the form of a list. Moving averages are calculated
#'   from OLS lm objects, which are also returned by the function.
#'
#' @param bio_year A vector of biological years concurrent with daily counts.
#' @param counts A vector of daily counts.
#' @param yday A vector of biologically relevant year days.
#' @param terciles A vector of length = 2, detailing the percentile based
#'   central window of the count season of interest e.g., (0.33, 0.66).
#' @param unreliable_years A concatenated list of unreliable years (e.g., years
#'   during which floods led to unreliable silver eel counts).
#' @return A list of length 6: terciles_species_year; A 3 x no. years matrix of
#'   tercile dates. Each row relates to one year CI95_se_terciles.
#'   CI95_se_terciles_list_trend; A list of length equal to number of years in
#'   time series, Each list element contains a 2 x 3 matrix of fitted (mean
#'   expected) terciles (e.g., 33rd and 66th) and each's accompanying 95%CIs.
#'   lower_perc_lm; A lm object of the lower tercile (e.g., 33.3rd percentile)
#'   lower_perc_mean_95CI; A 3 x no. years matrix of fitted lower tercile year
#'   days in addition to 95%CIs upper_perc_lm;  A lm object of the upper tercile
#'   (e.g., 66.6th percentile) upper_perc_mean_95CI; A 3 x no. years matrix of
#'   fitted upper tercile year days in addition to 95% CIs.
#' @examples
#' \dontrun{
#' data_counts <-
#'   readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#' data_counts_sub <-
#'   data_counts[data_counts$salmonid_year %in% c(1999:2018), ]
#' calculate_terciles_detrended(
#'   unreliable_years = c(2000, 2004),
#'   bio_year = data_counts_sub$salmonid_year,
#'   counts = data_counts_sub$ssmolt,
#'   yday = data_counts_sub$salmonid_yday,
#'   terciles = c(1 / 3, 2 / 3)
#' )
#' }
#' @export
calculate_terciles_detrended <- function(bio_year, counts, yday, terciles, unreliable_years){
  #  View(bio_year)
  bio_year <- as.vector(bio_year)
  counts <- as.vector(counts)
  yday <- as.vector(yday)
  #  year_range = list(unique(bio_year))[[1]] # note lack of col indicator here for old version
  year_range = list(unique(bio_year))[[1]]

  # for two percentiles
  if(length(terciles) == 2){
    terciles_species <- matrix(ncol = 2,nrow = 0)
    colnames(terciles_species) <- c(paste(round(terciles[1]*100,1),"%"), paste(round(terciles[2]*100,1),"%"))

    for(i in year_range){
      datasi_species <- rep(x = yday[bio_year == i], times = counts[bio_year == i])
      terciles_species_i <- matrix(quantile(datasi_species, probs = c(terciles[1], terciles[2])),nrow = 1, ncol = 2)
      rownames(terciles_species_i) <- i
      non_zero_day_freq_i <- as.data.frame(cbind(unique(datasi_species),counts[bio_year == i][counts[bio_year == i]>0]))
      colnames(non_zero_day_freq_i) <- c("day","freq")
      terciles_species <- rbind(terciles_species,terciles_species_i)
    }

    terciles_species_year <- cbind(as.numeric(rownames(terciles_species)),terciles_species)
    colnames(terciles_species_year) <- c("year", paste(round(terciles[1]*100,1),"%"), paste(round(terciles[2]*100,1),"%"))
  }

  ## for three percentiles
  if(length(terciles) == 3){
    terciles_species <- matrix(ncol = 3,nrow = 0)
    colnames(terciles_species) <- c(paste(round(terciles[1]*100,1),"%"), paste(round(terciles[2]*100,1),"%"),paste(round(terciles[3]*100,1),"%"))

    for(i in year_range){
      datasi_species <- rep(x = yday[bio_year == i], times = counts[bio_year == i])
      terciles_species_i <- matrix(quantile(datasi_species, probs = c(terciles[1], terciles[2],terciles[3])),nrow = 1, ncol = 3)
      rownames(terciles_species_i) <- i
      non_zero_day_freq_i <- as.data.frame(cbind(unique(datasi_species),counts[bio_year == i][counts[bio_year == i]>0]))
      colnames(non_zero_day_freq_i) <- c("day","freq")
      terciles_species <- rbind(terciles_species,terciles_species_i)
    }


    terciles_species_year <- cbind(as.numeric(rownames(terciles_species)),terciles_species)
    colnames(terciles_species_year) <- c("year", paste(round(terciles[1]*100,1),"%"), paste(round(terciles[2]*100,1),"%"),paste(round(terciles[3]*100,1),"%"))
  }

  if(length(terciles) == 2){

    try(if (!missing("unreliable_years")) {
      # replace unreliable years with NAs
      missing_data <- (numeric(length = length(unreliable_years)))
      for(i in 1:length(missing_data)){
        missing_data[i] <- which(terciles_species_year[,"year"] == unreliable_years[i])
      }
      for(i in list(missing_data)[[1]]){
        terciles_species_year[i,2:3] <- NA
      }
    }, silent = TRUE)

    # identify and subset to complete cases only
    terciles_no_NAs <- terciles_species_year[complete.cases(terciles_species_year), ][,c(paste(round(terciles[1]*100,1),"%"),paste(round(terciles[2]*100,1),"%"))]

  }

  if(length(terciles) == 3){

    try(if (!missing("unreliable_years")) {
      # replace unreliable years with NAs
      missing_data <- (numeric(length = length(unreliable_years)))
      for(i in 1:length(missing_data)){
        missing_data[i] <- which(terciles_species_year[,"year"] == unreliable_years[i])
      }
      for(i in list(missing_data)[[1]]){
        terciles_species_year[i,2:4] <- NA
      }
    }, silent = TRUE)

    # identify and subset to complete cases only
    terciles_no_NAs <- terciles_species_year[complete.cases(terciles_species_year), ][,c(paste(round(terciles[1]*100,1),"%"),paste(round(terciles[2]*100,1),"%"),paste(round(terciles[3]*100,1),"%"))]

  }
  #terciles_species_year
  ### check trends in phenology
  #plot(terciles_species_year[,1], terciles_species_year[,2], ylim = c(min(terciles_species_year[,2:3], na.rm = TRUE), max(terciles_species_year[,2:3], na.rm = TRUE)))
  #points(terciles_species_year[,1], terciles_species_year[,3], col = "blue")
  #segments(x0 = as.numeric(rownames(terciles_species_year)),
  #         y0 = terciles_species_year[,2],
  #         x1 = as.numeric(rownames(terciles_species_year)),
  #         y1 = terciles_species_year[,3],
  #         col = "black",
  #         lty = 3)
  ###
  # DETRENDING

  ## obtain different mean tercile for each year based on linear detrending
  # fit a line through the 33.3 and 66.6 percentiles (these will then be the mean against which to compare each years phenonolgy)
  # lower lm
  model_data <- as.data.frame(terciles_species_year)
  colnames(model_data) <- c("year", "first_tercile", "second_tercile")
  #model_data

  lower_perc_lm <- stats::lm(first_tercile ~ year, na.action = "na.omit", data = model_data)
  lower_perc_mean_95CI <- stats::predict(lower_perc_lm, interval = "confidence", level = 0.95, se.fit = FALSE, newdata = model_data)

  # upper lm
  upper_perc_lm <- stats::lm(second_tercile ~ year, na.action = "na.omit", data = model_data)
  upper_perc_mean_95CI <- stats::predict(upper_perc_lm, interval = "confidence", level = 0.95, se.fit = FALSE, newdata = model_data)

  # plot trends
  #lines(as.numeric(names(lower_perc_mean)),as.vector(lower_perc_mean), lty = 3, col = "black")
  #lines(as.numeric(names(upper_perc_mean)),as.vector(upper_perc_mean), lty = 3, col = "blue")

  # CIs
  #lines(as.numeric(rownames(upper_perc_mean_95CI)),as.vector(upper_perc_mean_95CI[,"lwr"]), lty = 2, col = "blue")
  #lines(as.numeric(rownames(upper_perc_mean_95CI)),as.vector(upper_perc_mean_95CI[,"upr"]), lty = 2, col = "blue")
  #lines(as.numeric(rownames(lower_perc_mean_95CI)),as.vector(lower_perc_mean_95CI[,"lwr"]), lty = 2, col = "black")
  #lines(as.numeric(rownames(lower_perc_mean_95CI)),as.vector(lower_perc_mean_95CI[,"upr"]), lty = 2, col = "black")

  # create a by-year list of preds and CIs
  CI95_se_terciles_i <- matrix(nrow = 3, ncol = 2)
  rownames(CI95_se_terciles_i) <- c("mean","upper","lower")
  colnames(CI95_se_terciles_i) <- c(paste(round(terciles[1]*100,1),"%"), paste(round(terciles[2]*100,1),"%"))
  CI95_se_terciles_list_trend <- base::replicate(length(terciles_species_year[,"year"]), CI95_se_terciles_i, simplify=FALSE)

  names(CI95_se_terciles_list_trend) <- c(terciles_species_year[,"year"]) # name list element

  # populate all list elements
  for(i in 1:length(terciles_species_year[,"year"])){
    # first tercile
    CI95_se_terciles_list_trend[[i]][1,1] <- lower_perc_mean_95CI[i,1] # mean
    CI95_se_terciles_list_trend[[i]][2,1] <- lower_perc_mean_95CI[i,3] # upper CI
    CI95_se_terciles_list_trend[[i]][3,1] <- lower_perc_mean_95CI[i,2] # lower CI
    # second tercile
    CI95_se_terciles_list_trend[[i]][1,2] <- upper_perc_mean_95CI[i,1] # mean
    CI95_se_terciles_list_trend[[i]][2,2] <- upper_perc_mean_95CI[i,3] # upper CI
    CI95_se_terciles_list_trend[[i]][3,2] <- upper_perc_mean_95CI[i,2] # lower CI
  }

  # calculate mean and uncertainties including complete cases only, before returning all terciles including NAs

  #sd_terciles <- apply(X = terciles_no_NAs, MARGIN = 2, FUN = sd)
  #se_terciles <- sd_terciles/sqrt(nrow(terciles_no_NAs))
  #CI95_se_terciles <- rbind(mean_terciles, mean_terciles + 1.96*se_terciles, mean_terciles - 1.96*se_terciles)
  # percentile_mean_terciles <- apply(X = terciles_no_NAs, MARGIN = 2, FUN = quantile, probs = c(0.025,0.50, 0.975))
  #rownames(CI95_se_terciles) <- c("mean","upper","lower")
  #CI95_se_terciles

  return(list(terciles_species_year = terciles_species_year,
              CI95_se_terciles_list_trend = CI95_se_terciles_list_trend,
              lower_perc_lm = lower_perc_lm,
              lower_perc_mean_95CI = lower_perc_mean_95CI,
              upper_perc_lm = upper_perc_lm,
              upper_perc_mean_95CI = upper_perc_mean_95CI))
}
