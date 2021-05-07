#' Summarise percentage of counts recorded in each tercile in multi-year time
#' series
#'
#' @description This function takes vectors of counts, biological years,
#'   biological year days and the timeseries averaged year days on which two
#'   quantiles (e.g., 33 and 66) occur in multi-year ecological count time
#'   series and returnes the percentage of counts that were recorded in each
#'   tercile.
#'
#' @param bio_year A vector of biological years concurrent with daily counts.
#' @param counts A vector of daily counts.
#' @param yday A vector of biologically relevant year days.
#' @param terciles A vector of length = 2, detailing the percentile based
#'   central window of the count season of interest e.g., (0.33, 0.66).
#' @param unreliable_years A concatenated list of unreliable years (e.g., years
#'   during which floods led to unreliable silver eel counts).
#' @param plot Boolean. Should plot of raw counts be returned?
#' @param mean_tercile_dates A vector of biologically relevant year days.
#' @param ... Additional arguments to be passed to nested functions.
#' @return A list of length 5 ("year", first tercile currently labelled "early";
#'   middle tercile labelled "average"; third tercile labelled "late"; "n",
#'   number of counted individuals per year) that can be combined with
#'   do.call(cbind, result): each list element is of length no. years in time
#'   series.
#' @examples
#' \dontrun{
#' data_counts <-
#'   readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#'
#' data_counts_sub <-
#'   data_counts[data_counts$salmonid_year %in% c(1999:2018), ]
#'
#' # identify mean year days of each tercile
#' mean_terciles <- round(calculate_terciles(bio_year = data_counts_sub$salmonid_year,
#'                                           counts = data_counts_sub$ssmolt,
#'                                           yday = data_counts_sub$salmonid_yday,
#'                                           terciles = c(1/3,2/3))[["CI95_se_terciles"]][1,])
#'
#' terciles_plot_data(unreliable_years = c(2000,2004,2008),
#'                              bio_year = data_counts_sub$salmonid_year,
#'                              counts = data_counts_sub$ssmolt,
#'                              yday = data_counts_sub$salmonid_yday,
#'                              terciles = c(1/3,2/3),
#'                              plot = FALSE,
#'                              mean_tercile_dates = mean_terciles)
#' # print(do.call(cbind,result))
#' }
#' @export
terciles_plot_data <- function(counts,
                               bio_year,
                               yday,
                               terciles,
                               plot,
                               unreliable_years,
                               mean_tercile_dates = NULL,
                               ...){

  bio_year <- as.vector(bio_year)
  counts <- as.vector(counts)
  yday <- as.vector(yday)
  year_range = list(unique(bio_year))[[1]]

  try(if (!missing("unreliable_years")) {
    # call terciles function here
    mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                              counts = counts,
                                              yday = yday,
                                              unreliable_years = unreliable_years,
                                              terciles = terciles)[["CI95_se_terciles"]][1,])
  }, silent = TRUE)

  try(if (missing("unreliable_years")) {
    # call terciles function here
    mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                              counts = counts,
                                              yday = yday,
                                              terciles = terciles)[["CI95_se_terciles"]][1,])
  }, silent = TRUE)

  try(if (!is.null("mean_tercile_dates")) {
    mean_terciles <- mean_tercile_dates
  }, silent = TRUE)

  # tabulate data in relation to mean tercile limits for the entire time series
  terciles_plot_species <- matrix(ncol = 5,nrow = 0)
  colnames(terciles_plot_species) <- c("year","early","average","late","n")

  if(plot == TRUE){
    par(mfrow = c(1,1))
    plot(rep(1970, times = 2), xlim = c(min(bio_year),max(bio_year)),ylim = c(1,max(yday)), col = NULL, ylab = "ordinal day", xlab = "year")
  }

  # populate table in relation to mean tercile limits for the entire time series
  for(i in year_range){
    datasi_species <- rep(x = yday[bio_year == i], times = counts[bio_year == i])
    non_zero_day_freq_i <- as.data.frame(cbind(unique(datasi_species),
                                               counts[bio_year == i][counts[bio_year == i]>0]))
    colnames(non_zero_day_freq_i) <- c("day","freq")
    total_run_i <- sum(non_zero_day_freq_i["freq"])

    # need to make sure that mean tercile days are not whole numbers or subset will (if for example lower tercile = 127) take counts up to day 126 and after 128, thus losing information related to day 127...
    # so add 0.01 to terciles if whole numbers, or make mean terciles whole integers then greater than and including alongside less then (but not including)
    #mean_terciles
    #all.equal(mean_terciles[[1]], as.integer(mean_terciles[[1]])) # to check if integer

    terciles_plot_species_i <-
      matrix(
        c(
          i,
          100 * (sum(non_zero_day_freq_i[non_zero_day_freq_i$day < mean_terciles[paste(round(terciles[1] *
                                                                                               100, 1), "%")], "freq"]) / total_run_i),
          100 * (sum(non_zero_day_freq_i[non_zero_day_freq_i$day >= mean_terciles[paste(round(terciles[1] *
                                                                                                100, 1), "%")] &
                                           non_zero_day_freq_i$day <= mean_terciles[paste(round(terciles[2] * 100, 1), "%")], "freq"]) / total_run_i),
          100 * (sum(non_zero_day_freq_i[non_zero_day_freq_i$day > mean_terciles[paste(round(terciles[2] *
                                                                                               100, 1), "%")], "freq"]) / total_run_i),
          total_run_i
        ),
        nrow = 1,
        ncol = 5,
        dimnames = list(NULL, c("year", "early", "average", "late", "n"))
      )

    # log scale point sizes
    if(plot == TRUE){
      points(rep(i, times = nrow(non_zero_day_freq_i)),non_zero_day_freq_i[,"day"], cex = 0.8*(log(non_zero_day_freq_i[,"freq"])/log(max(non_zero_day_freq_i[,"freq"]))), pch = 19, col = (grey(level = log(1+min(non_zero_day_freq_i[,"freq"]/(max(non_zero_day_freq_i[,"freq"])))/(non_zero_day_freq_i[,"freq"]/(max(non_zero_day_freq_i[,"freq"])))))))
    }

    terciles_plot_species <- rbind(terciles_plot_species,terciles_plot_species_i)
  }
  if(plot == TRUE){
    abline(h = mean_terciles[paste(round(terciles[1]*100,1),"%")], lty = 3, col = "red")
    abline(h = mean_terciles[paste(round(terciles[2]*100,1),"%")], lty = 3, col = "red")
  }

  terciles_plot_species_full <- merge(data.frame(year=seq(from = min(year_range), to = max(year_range), by = 1)),
                                      terciles_plot_species,
                                      all.x=TRUE)

  try(if (!missing("unreliable_years")) {
    # replace unreliable years with NAs (or insert NA rows if not there in the first place...)
    # create new df with full sequence of years then merge with old and placing NA where there are gaps

    terciles_plot_species_full <- merge(data.frame(year=seq(from = min(year_range), to = max(year_range), by = 1)),
                                        terciles_plot_species,
                                        all.x=TRUE)

    missing_data <- (numeric(length = length(unreliable_years)))
    for(i in 1:length(missing_data)){
      missing_data[i] <- which(terciles_plot_species_full[,"year"] == unreliable_years[i])
    }
    for(i in list(missing_data)[[1]]){
      terciles_plot_species_full[i,2:5] <- NA
    }
  }, silent = TRUE)

  # if (missing("unreliable_years")) {
  #     # replace unreliable years with NAs (or insert NA rows if not there in the first place...)
  #     # create new df with full sequence of years then merge with old and placing NA where there are gaps
  #
  #     terciles_plot_species_full <- merge(data.frame(year=seq(from = min(year_range), to = max(year_range), by = 1)),
  #                                         terciles_plot_species,
  #                                         all.x=TRUE)
  #   }

  return(terciles_plot_species_full)
}
