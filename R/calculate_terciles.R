#' Identify year days of recorded quantiles for multi-year count time series
#'
#' @description This function takes vectors of counts, biological years and
#'   biological year days and identifies the year day on which two or three
#'   percentiles (e.g., 33 and 66) are recorded in multi-year ecological count
#'   time series. In addition, the multi-year average expectation of each
#'   tercile and its 95%CI is returned.
#'
#' @param bio_year A vector of biological years concurrent with daily counts.
#' @param counts A vector of daily counts.
#' @param yday A vector of biologically relevant year days.
#' @param terciles A vector of length = 2, detailing the percentile based
#'   central window of the count season of interest e.g., (0.33, 0.66).
#' @param unreliable_years A concatenated list of unreliable years (e.g., years
#'   during which floods allow fish to bypass traps).
#' @return A list of length 2. terciles_species_year; A 3 x no. years matrix of
#'   tercile dates. Each row relates to one year CI95_se_terciles; A 2 x 3
#'   matrix of mean year days for each tercile in addition to 95% CIs.
#' @examples
#' \dontrun{
#' data_counts <-
#'   readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#' data_counts_sub <-
#'   data_counts[data_counts$salmonid_year %in% c(1999:2018), ]
#' calculate_terciles(
#'   unreliable_years = c(2000, 2004),
#'   bio_year = data_counts_sub$salmonid_year,
#'   counts = data_counts_sub$ssmolt,
#'   yday = data_counts_sub$salmonid_yday,
#'   terciles = c(1 / 3, 2 / 3)
#' )
#' }
#' @export
calculate_terciles <- function(bio_year,
                               counts,
                               yday,
                               terciles,
                               unreliable_years){

  bio_year <- as.vector(bio_year)
  counts <- as.vector(counts)
  yday <- as.vector(yday)
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
    terciles_no_NAs <- terciles_species_year[complete.cases(terciles_species_year), ][,c(paste(round(terciles[1]*100,1),"%"),paste(round(terciles[2]*100,1),"%"),paste(terciles[3]*100,"%"))]

  }

  # calculate mean and uncertainties including complete cases only, before returning all terciles including NAs
  mean_terciles <- apply(X = terciles_no_NAs, MARGIN = 2, FUN = mean)
  sd_terciles <- apply(X = terciles_no_NAs, MARGIN = 2, FUN = sd)
  se_terciles <- sd_terciles/sqrt(nrow(terciles_no_NAs))
  CI95_se_terciles <- rbind(mean_terciles, mean_terciles + 1.96*se_terciles, mean_terciles - 1.96*se_terciles)
  # percentile_mean_terciles <- apply(X = terciles_no_NAs, MARGIN = 2, FUN = quantile, probs = c(0.025,0.50, 0.975))
  rownames(CI95_se_terciles) <- c("mean","upper","lower")

  return(list(terciles_species_year = round(terciles_species_year),
              CI95_se_terciles = CI95_se_terciles))
}
