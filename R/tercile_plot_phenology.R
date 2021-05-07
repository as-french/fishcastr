#' Generate tercile plot illustrating seasonal migration phenology anomalies
#' from daily ecological counts.
#'
#' @description This function takes vectors of dates, counts, biological years
#'   and biological year days and illustrates inter-annual variation in
#'   phenological anomalies relative to long term (full series consistent
#'   tercile dates) or moving average expectations (year days on which terciles
#'   occur follow a linear trend). Anomalies are illustrated using a tercile
#'   plot, which illustrates percentages of counts recorded during and either
#'   side of a central window of expectation; that is, during a defined central
#'   window (which may shift linearly with time), we expect one third of counts
#'   to be recorded plus minus uncertainty - deviations from expected
#'   percentages of counts recorded during and either side of the central window
#'   define five anomalies: early, late, bimodal, condensed and average. This
#'   function returns a multi-element list object, and three types of plots.
#'
#' @param bio_year A vector of biological years concurrent with daily counts.
#' @param counts A vector of daily counts.
#' @param yday A vector of biologically relevant year days
#' @param terciles A vector of length = 2, detailing the percentile based
#'   central window of the count season of interest e.g., (0.33, 0.66)
#' @param species_name A character string, of options ("Atlantic salmon
#'   (smolt)", "Sea trout (smolt)", "European eel (silver)").
#' @param dates A vector of class "Date".
#' @param unreliable_years A concatenated list of unreliable years (e.g., years
#'   during which floods led to unreliable silver eel counts)
#' @param return_data Boolean indicating if function should return the data used
#'   to plot the tercile plot.
#' @param plot Boolean indicating if plot should be returned (e.g., in addition
#'   to returned data).
#' @param definitions Boolean indicating if interpretive (stand alone) legend
#'   should be returned instead of data based plot.
#' @param mean_tercile_dates A list of matrices containing (moving) average
#'   biological year day quantiles (e.g., average year day of 33rd and 66th
#'   percentiles and upper and lower CIs). This argument is only required for
#'   comparing observed and simulated phenology. Default NULL.
#' @param average_perc_per_tercile A matrix of average (median) percentages of
#'   counts observed in each tercile with upper and lower bootstrapped CIs. This
#'   argument is only required for comparing observed and simulated phenology.
#'   Default NULL.
#' @param detrended Boolean to determine if quanile trends are taken into
#'   account; i.e., if detrended, the biological year day delineating each
#'   tercile may vary among years with a linear trend.
#' @param trend_plot Boolean to determine if seasonal trends are plotted. If
#'   TRUE, only trends on positions of quantiles are plotted.
#' @param bulk_pheno_three_anoms Only define three anomlalies (late, average and
#'   early). Experimental. Default NULL.
#' @param ... Additional arguments to nested functions.
#' @return If plot == TRUE, a tercile plot; if trend_plot == TRUE, a trend plot
#'   and; if return_data == TRUE, a multielement list of various matrices/lists
#'   of matrices containing: average_percent_per_tercile; A 3 x 3 matrix of
#'   expected percentage of counts per tercile and 95% bootstrapped CIs (can
#'   also be given as argument (see above). mean_terciles; A list of 3 x 2
#'   matrices (one per year) detailing the expectated (e.g. moving average mean)
#'   year days of each tercile and 95% CIs - note these means and CIs are
#'   plotted by trend_plot == TRUE if OLS linear trends are significant.
#'   tercile_data; A 5 x n years matrix showing percentages of counts per
#'   tercile per year in addition to total counts per year. average, extended,
#'   early, late, condensed; 5 separate concatenated lists of each anomaly type,
#'   whereby element values are years during which anomaly occurred.
#'   mean_terciles_raw; A 3 X n years matrix containing year days on which
#'   (e.g., 33.3 and 66.6) quantiles occur and the year during which they occur.
#'   earliest_date; earliest calendar date on which a biofix occurred during the
#'   time series. earliest_date_yday; earliest year day on which a biofix
#'   occurred during the time series.
#' @examples
#' \dontrun{
#' data_counts <-
#'   readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#'
#'  data_counts_subset <- data_counts[data_counts$salmonid_year %in% c(1981:2018),]
#' dev.new()
#'  phenology_anomalies <- tercile_plot_phenology(
#'    bio_year = data_counts_subset$salmonid_year,
#'    counts = data_counts_subset$ssmolt,
#'    yday = data_counts_subset$salmonid_yday,
#'    dates = data_counts_subset$date,
#'    terciles = c(1/3, 2/3),
#'    species_name = "Atlantic salmon (smolt)",
#'    return_data = TRUE,
#'    plot = TRUE,
#'    definitions = FALSE,
#'    detrended = FALSE,
#'    trend_plot = FALSE,
#' )
#' }
#' @export
tercile_plot_phenology <- function(bio_year,
                                   counts,
                                   yday,
                                   terciles,
                                   species_name,
                                   dates,
                                   unreliable_years = NULL,
                                   return_data,
                                   plot = TRUE,
                                   definitions,
                                   mean_tercile_dates = NULL,
                                   average_perc_per_tercile = NULL,
                                   detrended = TRUE,
                                   trend_plot = FALSE,
                                   bulk_pheno_three_anoms = FALSE,
                                   ...){

  old_state <- get_rand_state()
  on.exit(set_rand_state(old_state))

  bio_year <- as.vector(bio_year)
  unique_bio_years <- unique(bio_year)
  plot_lt_lower_trend <- TRUE
  plot_lt_upper_trend <- TRUE

  # earliest day of year biofix (to replace manual input of biofix_yday text)
  dataset <- data.frame("dates" = dates, "yday" = yday)
  earliest_date <- dataset[,"dates"][which.min(dataset[,"yday"])]
  earliest_date_yday <- lubridate::yday(earliest_date)

  counts <- as.vector(counts)
  yday <- as.vector(yday)
  year_range = list(unique(bio_year))[[1]]
  lower_tercile <- round(terciles[1][[1]]*100,1)
  upper_tercile <- round(terciles[2][[1]]*100,1)
  average_tercile <- upper_tercile - lower_tercile
  #propor_expec_outer_terciles <- (100 - average_tercile)/2

  if(is.null("unreliable_years")){
    rm(unreliable_years)
  }

  try(if(!missing("unreliable_years") ) {

    if(detrended == FALSE & is.null(mean_tercile_dates)){
      mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                counts = counts,
                                                yday = yday,
                                                unreliable_years = unreliable_years,
                                                terciles = terciles)[["CI95_se_terciles"]][1,])

    }
    if(detrended == TRUE & trend_plot == FALSE & is.null(mean_tercile_dates)){
      mean_terciles <- lapply(calculate_terciles_detrended(bio_year = bio_year,
                                                           counts = counts,
                                                           yday = yday,
                                                           unreliable_years = unreliable_years,
                                                           terciles = terciles)[["CI95_se_terciles_list_trend"]],round,0)

      # ------------------------------------------------------------------------------------------
      # CHECK IF LONG TERM TRENDS IN EACH PERCENITILE ARE SIGNIFICANT
      # ------------------------------------------------------------------------------------------
      # LOWER
      mean_tercile_trend_model_lower <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     unreliable_years = unreliable_years,
                                                                     terciles = terciles)[["lower_perc_lm"]]
      pval_lower_perc <- broom::glance(mean_tercile_trend_model_lower)[["p.value"]]

      # is linear trend significant?
      if(pval_lower_perc > 0.05){
        # set each of lower mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                unreliable_years = unreliable_years,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for lower percentile
        mean_terciles <- mapply(function(old, new) {
          old[,1] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,1]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_lower_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------
      # UPPER
      mean_tercile_trend_model_upper <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     unreliable_years = unreliable_years,
                                                                     terciles = terciles)[["upper_perc_lm"]]
      pval_upper_perc <- broom::glance(mean_tercile_trend_model_upper)[["p.value"]]

      # is linear trend significant?
      if(pval_upper_perc > 0.05){
        # set each of upper mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                unreliable_years = unreliable_years,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for upper percentile
        mean_terciles <- mapply(function(old, new) {
          old[,2] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,2]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_upper_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------

    }
    if(detrended == TRUE & trend_plot == TRUE & is.null(mean_tercile_dates)){
      # assuming linear trends are significant
      mean_terciles <- lapply(calculate_terciles_detrended(bio_year = bio_year,
                                                           counts = counts,
                                                           yday = yday,
                                                           unreliable_years = unreliable_years,
                                                           terciles = terciles)[["CI95_se_terciles_list_trend"]],round,0)

      # ------------------------------------------------------------------------------------------
      # CHECK IF LONG TERM TRENDS IN EACH PERCENITILE ARE SIGNIFICANT
      # ------------------------------------------------------------------------------------------
      # LOWER
      mean_tercile_trend_model_lower <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     unreliable_years = unreliable_years,
                                                                     terciles = terciles)[["lower_perc_lm"]]
      pval_lower_perc <- broom::glance(mean_tercile_trend_model_lower)[["p.value"]]

      # is linear trend significant?
      if(pval_lower_perc > 0.05){
        # set each of lower mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                unreliable_years = unreliable_years,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for lower percentile
        mean_terciles <- mapply(function(old, new) {
          old[,1] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,1]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_lower_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------
      # UPPER
      mean_tercile_trend_model_upper <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     unreliable_years = unreliable_years,
                                                                     terciles = terciles)[["upper_perc_lm"]]
      pval_upper_perc <- broom::glance(mean_tercile_trend_model_upper)[["p.value"]]

      # is linear trend significant?
      if(pval_upper_perc > 0.05){
        # set each of upper mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                unreliable_years = unreliable_years,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for upper percentile
        mean_terciles <- mapply(function(old, new) {
          old[,2] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,2]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_upper_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------
      mean_terciles_raw <- round(calculate_terciles(bio_year = bio_year,
                                                    counts = counts,
                                                    yday = yday,
                                                    unreliable_years = unreliable_years,
                                                    terciles = terciles)[["terciles_species_year"]])

    }

  }, silent = TRUE)


  try(if(missing("unreliable_years")) {
    if(detrended == FALSE & is.null(mean_tercile_dates)){

      mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                counts = counts,
                                                yday = yday,
                                                terciles = terciles)[["CI95_se_terciles"]][1,])
    }
    if(detrended == TRUE & trend_plot == FALSE & is.null(mean_tercile_dates)){


      mean_terciles <- lapply(calculate_terciles_detrended(bio_year = bio_year,
                                                           counts = counts,
                                                           yday = yday,
                                                           terciles = terciles)[["CI95_se_terciles_list_trend"]],round,0)

      # ------------------------------------------------------------------------------------------
      # CHECK IF LONG TERM TRENDS IN EACH PERCENITILE ARE SIGNIFICANT
      # ------------------------------------------------------------------------------------------
      # LOWER
      mean_tercile_trend_model_lower <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     terciles = terciles)[["lower_perc_lm"]]
      pval_lower_perc <- broom::glance(mean_tercile_trend_model_lower)[["p.value"]]

      # is linear trend significant?
      if(pval_lower_perc > 0.05){
        # set each of lower mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for lower percentile
        mean_terciles <- mapply(function(old, new) {
          old[,1] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,1]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_lower_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------
      # UPPER
      mean_tercile_trend_model_upper <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     terciles = terciles)[["upper_perc_lm"]]
      pval_upper_perc <- broom::glance(mean_tercile_trend_model_upper)[["p.value"]]

      # is linear trend significant?
      if(pval_upper_perc > 0.05){
        # set each of upper mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for upper percentile
        mean_terciles <- mapply(function(old, new) {
          old[,2] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,2]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_upper_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------

    }
    if(detrended == TRUE & trend_plot == TRUE & is.null(mean_tercile_dates)){

      mean_terciles <- lapply(calculate_terciles_detrended(bio_year = bio_year,
                                                           counts = counts,
                                                           yday = yday,
                                                           terciles = terciles)[["CI95_se_terciles_list_trend"]],round,0)

      # ------------------------------------------------------------------------------------------
      # CHECK IF LONG TERM TRENDS IN EACH PERCENITILE ARE SIGNIFICANT
      # ------------------------------------------------------------------------------------------
      # LOWER
      mean_tercile_trend_model_lower <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     terciles = terciles)[["lower_perc_lm"]]
      pval_lower_perc <- broom::glance(mean_tercile_trend_model_lower)[["p.value"]]

      # is linear trend significant?
      if(pval_lower_perc > 0.05){
        # set each of lower mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for lower percentile
        mean_terciles <- mapply(function(old, new) {
          old[,1] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,1]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_lower_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------
      # UPPER
      mean_tercile_trend_model_upper <- calculate_terciles_detrended(bio_year = bio_year,
                                                                     counts = counts,
                                                                     yday = yday,
                                                                     terciles = terciles)[["upper_perc_lm"]]
      pval_upper_perc <- broom::glance(mean_tercile_trend_model_upper)[["p.value"]]

      # is linear trend significant?
      if(pval_upper_perc > 0.05){
        # set each of upper mean_terciles list values for each year to long term average (i.e., with no trend)
        non_sig_trend_mean_terciles <- round(calculate_terciles(bio_year = bio_year,
                                                                counts = counts,
                                                                yday = yday,
                                                                terciles = terciles)[["CI95_se_terciles"]])

        # replace list elements for upper percentile
        mean_terciles <- mapply(function(old, new) {
          old[,2] <- new
          return(old)
        }, old = mean_terciles, new = list(non_sig_trend_mean_terciles[,2]), SIMPLIFY = FALSE)

        # assign a value to a variable to tell trend plot to plot lines and CIs or not...
        plot_lt_upper_trend <- FALSE
      }

      # ------------------------------------------------------------------------------------------

      mean_terciles_raw <- round(calculate_terciles(bio_year = bio_year,
                                                    counts = counts,
                                                    yday = yday,
                                                    terciles = terciles)[["terciles_species_year"]])
    }

    if(!is.null(mean_tercile_dates)){
      mean_terciles <- mean_tercile_dates
    }


  }, silent = TRUE)

  try(if(!missing("unreliable_years") ) {
    if(detrended == FALSE){
      tercile_data <- terciles_plot_data(bio_year = bio_year,
                                         counts = counts,
                                         yday = yday,
                                         terciles = terciles,
                                         unreliable_years = unreliable_years,
                                         plot = FALSE,
                                         mean_tercile_dates = mean_terciles)
    }

    if(detrended == TRUE){
      tercile_data <- terciles_plot_data_detrended(bio_year = bio_year,
                                                   counts = counts,
                                                   yday = yday,
                                                   terciles = terciles,
                                                   unreliable_years = unreliable_years,
                                                   plot = FALSE,
                                                   mean_tercile_dates = mean_terciles)
    }

  }, silent = TRUE)


  try(if(missing("unreliable_years")) {
    if(detrended == FALSE){

      tercile_data <- terciles_plot_data(bio_year = bio_year,
                                         counts = counts,
                                         yday = yday,
                                         terciles = terciles,
                                         plot = FALSE,
                                         mean_tercile_dates = mean_terciles)
    }

    if(detrended == TRUE){
      tercile_data <- terciles_plot_data_detrended(bio_year = bio_year,
                                                   counts = counts,
                                                   yday = yday,
                                                   terciles = terciles,
                                                   plot = FALSE,
                                                   mean_tercile_dates = mean_terciles)
    }

  }, silent = TRUE)

  #tercile_data

  if(definitions == TRUE){
    tercile_data <- data.frame("year" = c(1970:1986),
                               "early" = c(60,50,50,40,40,37.5,30,20,20,30,25,20,30,20,30,20,40),
                               "average" = c(20,30,20,30,40,37.5,50,60,50,40,37.5,40,30,30,20,20,20),
                               "late" = c(20,20,30,30,20,25,20,20,30,30,37.5,40,40,50,50,60,40),
                               "n" = c(rep(1000, times = 17)))
  }

  try(if(!missing("mean_tercile_dates") ) {

    tercile_data <- terciles_plot_data(bio_year = bio_year,
                                       counts = counts,
                                       yday = yday,
                                       terciles = terciles,
                                       unreliable_years = unreliable_years,
                                       plot = FALSE,
                                       mean_tercile_dates = mean_terciles)

  }, silent = TRUE)

  #tercile_data

  # ------------------------------------------------------------------------------------------------------- #
  # TREND PLOT (ONLY PLOT SIGNIFICANT TRENDS)
  # ------------------------------------------------------------------------------------------------------- #
  if(detrended == TRUE & trend_plot == TRUE){

    if(species_name == "Atlantic salmon (smolt)"){
      species_name_title <- "Atlantic salmon (smolt)"
    }
    if(species_name == "Sea trout (smolt)"){
      species_name_title <- "Anadromous brown trout (smolt)"

    }
    if(species_name == "European eel (silver)"){
      species_name_title <- "European eel (silver)"

    }

    # plot the raw percentile data and trend lines
    par(mfrow = c(1,1), mgp = c(3,1,0), oma = c(6,4,2,1), mar = c(4,4,1,1))
    plot(mean_terciles_raw[,"year"],
         mean_terciles_raw[,"66.6 %"],
         ylab = paste("Ordinal days relative to ",earliest_date_yday," DOY", sep = ""),
         xlab = "Year",
         ylim = c(min(mean_terciles_raw[,"33.3 %"], na.rm = TRUE), max(mean_terciles_raw[,"66.6 %"], na.rm = TRUE)),
         col = NULL, tck = 0.02, xaxt = "n",
         xpd = NA,
         font.lab = 2,
         font = 2)
    title(main = species_name_title, line = -1.0, cex.main = 0.85, font = 2, xpd = NA, adj = 0.95)
    axis(side = 1, at = seq(from = min(mean_terciles_raw[,"year"]), to = max(mean_terciles_raw[,"year"]), by = 1), tck = 0.02, labels = FALSE)
    #  axis(side = 1, at = seq(from = min(mean_terciles_raw[,"year"]), to = max(mean_terciles_raw[,"year"]), by = 5), tck = 0.02, labels = seq(from = min(mean_terciles_raw[,"year"]), to = max(mean_terciles_raw[,"year"]), by = 5))


    text(x = seq(from = min(mean_terciles_raw[,"year"]),
                 to = max(mean_terciles_raw[,"year"]),
                 by = 5),
         y = rep(min(mean_terciles_raw[,"33.3 %"], na.rm = TRUE)-4,
                 times = length(seq(from = min(mean_terciles_raw[,"year"]),
                                    to = max(mean_terciles_raw[,"year"]),
                                    by = 5))),
         srt = 90,
         xpd = NA,
         labels = seq(from = min(mean_terciles_raw[,"year"]),
                      to = max(mean_terciles_raw[,"year"]), by = 5),
         adj =1,
         font = 2)

    trend_mean_list <- do.call(rbind, (lapply(mean_terciles, function(x) x[1,])))
    trend_upr_list <- do.call(rbind, (lapply(mean_terciles, function(x) x[2,])))
    trend_lwr_list <- do.call(rbind, (lapply(mean_terciles, function(x) x[3,])))

    if(plot_lt_lower_trend == TRUE){
      col_poly <- col2rgb("blue")
      col_poly_alpha1 <- rgb(red = col_poly[1], green = col_poly[2], blue = col_poly[3], alpha = 60, maxColorValue = 255)
      polygon(x = c(rownames(trend_mean_list), rev(rownames(trend_mean_list))),
              y = c(trend_lwr_list[,1],rev(trend_upr_list[,1])),
              col= col_poly_alpha1,
              lty = 0)
    }

    if(plot_lt_upper_trend == TRUE){
      col_poly <- col2rgb("red")
      col_poly_alpha2 <- rgb(red = col_poly[1], green = col_poly[2], blue = col_poly[3], alpha = 60, maxColorValue = 255)
      polygon(x = c(rownames(trend_mean_list), rev(rownames(trend_mean_list))),
              y = c(trend_lwr_list[,2],rev(trend_upr_list[,2])),
              col= col_poly_alpha2,
              lty = 0)
    }

    segments(x0 = mean_terciles_raw[,"year"],
             x1 = mean_terciles_raw[,"year"],
             y0 = mean_terciles_raw[,"33.3 %"],
             y1 = mean_terciles_raw[,"66.6 %"],
             col = "black",
             lwd = 1,
             lty = 3)
    points(mean_terciles_raw[,"year"],
           mean_terciles_raw[,"66.6 %"],
           col = "red", pch = 19, cex = 1.0)
    points(mean_terciles_raw[,"year"],
           mean_terciles_raw[,"33.3 %"],
           col = "blue", pch = 19, cex = 1.0)

    if(plot_lt_lower_trend == TRUE){
      lines(rownames(trend_mean_list), trend_mean_list[,1], col = "blue",type = "l")
    }
    if(plot_lt_upper_trend == TRUE){
      lines(rownames(trend_mean_list), trend_mean_list[,2], col = "red",type = "l")
    }

    if(plot_lt_lower_trend == TRUE & plot_lt_upper_trend == TRUE){

      legend("bottomleft",
             legend = c("33.3%", "66.6%", "33.3% trend", "66.6% trend"),
             pch = c(16,16,15,15),
             lty = c(0,0,1,1),
             pt.cex = c(2,2,2.5,2.5),
             text.col = "white",
             col = c("blue", "red", col_poly_alpha1, col_poly_alpha2),
             xpd = NA,
             bty = "n",
             inset = c(0.0,-0.55))
    }

    legend("bottomleft",
           legend = c("33.3%", "66.6%", "33.3% trend", "66.6% trend"),
           pch = c(16,16,22,22),
           lty = c(0,0,1,1),
           pt.cex = c(2,2,2.5,2.5),
           col = c(NULL, NULL, "blue", "red"),
           xpd = NA,
           bty = "n",
           inset = c(0.0,-0.55))
    #  title(adj = 0, main = paste(species_name," ", season, " migration phenology for 33.3% and 66.6% percentiles of the run\n", sep = ""), xpd = NA, line = 0.5, cex.main = 1.0)


  }
  # ------------------------------------------------------------------------------------------------------- #


  # # replace unreliable year data with NAs if not already done
  # missing_data <- (numeric(length = length(unreliable_years)))
  # for(i in 1:length(missing_data)){
  #   missing_data[i] <- which(tercile_data[,"year"] == unreliable_years[i])
  # }
  #
  # for(i in list(missing_data)[[1]]){
  #   tercile_data[i,2:5] <- NA
  # }
  #tercile_data
  # add extra row with 100% to scale colours, but don't plot it..

  #tercile_data_extra <- matrix(c(unique_bio_years-1,100,100,100,100),nrow = 1, ncol = 5)
  #tercile_data <- rbind(tercile_data_extra, tercile_data)

  t.color <- tercileBrewerColorRamp(10) # set haxadecimal #rrggbb colour ramps

  if(definitions == TRUE){
    t.color <- tercileColorRamp(15) # AF customised function provided by UNICAN
  }
  t.color[1,] <- c(NA,NA,NA) # make up to X% transparent

  # plot the terciles
  x <- tercile_data[,"year"]
  y <- c(1.9,2.0,2.1)

  #tercile_data

  # Different colours for each tercile to aid interpretation...
  z_early <- matrix(c(tercile_data[,"early"],rep(0, times = nrow(tercile_data)),rep(0, times = nrow(tercile_data))),ncol = 3)
  z_average <- matrix(c(rep(0, times = nrow(tercile_data)),tercile_data[,"average"],rep(0, times = nrow(tercile_data))),ncol = 3)
  z_late <- matrix(c(rep(0, times = nrow(tercile_data)),rep(0, times = nrow(tercile_data)),tercile_data[,"late"]),ncol = 3)

  if(plot == TRUE){

    #    pdf(NULL)
    #   dev.control(displaylist="enable")
#    suppressMessages(require(pryr))

    #    pheno_plot %<a-% {

    if(definitions == FALSE){
      #      dev.new()

      if(species_name == "Atlantic salmon (smolt)"){
        species_name_title <- "Atlantic salmon (smolt)"
      }
      if(species_name == "Sea trout (smolt)"){
        species_name_title <- "Anadromous brown trout (smolt)"

      }
      if(species_name == "European eel (silver)"){
        species_name_title <- "European eel (silver)"

      }
      #dev.new()
      par(mfrow = c(2,1), mar = c(1,4,0,5), oma = c(14,3,14,0))
      # overall counts trend (NOTE THAT THE AXIS FOR THE COUNTS MAY NEED TO SHIFT DEPENDING ON NUMBER OF YEARS IN SERIES...) ( + 1.3 for full dataset 1970-2018, 0.4 for reanalaysis 1982 to 2009 - implement this in script to sort this automatically...)
      xno = c(27,48)
      yno = c(0.4,1.3)
      datafrm <- data.frame("yno"= c(0.4,1.3), "xno" = c(27,48))
      lin_mod <- stats::lm(yno~xno, data = datafrm)
      #no . years in series
      xnum = length(tercile_data[,"year"])
      dfm2 <- data.frame("xno" = xnum)
      yno = stats::predict(lin_mod, newdata = dfm2)

      plot(tercile_data[,"year"], tercile_data[,"n"], lwd = 5, xlab = "", ylab = "Count", cex.lab = 0.8, type = "h", lend = 1, xaxt = "n",yaxt = "n", xlim = c(min(tercile_data[,"year"])+yno[[1]],max(tercile_data[,"year"])-yno[[1]]), bty = "n", ylim = c(0, max(tercile_data[,"n"],na.rm = TRUE)),xpd = NA)
      axis(side = 2, at = c(0,3000,6000,9000,12000,15000,18000), labels = c(0,3000,6000,9000,12000,15000,18000), cex.axis = 0.7, las = 1)

      title(adj = 0, main = paste(species_name_title," ", "migration phenology anomaly for central " ,average_tercile," % of the run\n", sep = ""), xpd = NA, line = 1.5, cex.main = 0.7)

      if(detrended == FALSE){
        mtext(adj = 0, paste("Annual anomalies relative to ",min(year_range)," - ",max(year_range)," average","\n(On average, the central ",average_tercile,"% of migrating fish are counted between ordinal days ", round(mean_terciles)[[1]]," and ",round(mean_terciles)[[2]],")", sep = ""), xpd = NA, line = 0.4, cex = 0.55, side = 3)
        mtext(side = 2, paste("Ordinal day\n (relative to day ",earliest_date_yday,")", sep = ""), xpd = NA, line = 5, cex = 0.8)
      }
      #par(mar=c(4,4,0,2))
      image(x,y,z_early, col =  t.color[,1],xaxt = "n",yaxt = "n", xlab = "", ylab = "", bty = "n", oldstyle = TRUE, zlim = c(0,100))
      image(x,y,z_average, col =  t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", add = TRUE, oldstyle = TRUE, zlim = c(0,100))
      image(x,y,z_late, col =  t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", add = TRUE, oldstyle = TRUE, zlim = c(0,100))

      if(detrended == TRUE){
        mtext(side = 2, paste("Ordinal day\n (relative to day ",earliest_date_yday," DOY)", sep = ""), xpd = NA, line = 5, cex = 0.8)
        #print(mean_terciles[[1]][1,1])
        #    print(mean_terciles)[[length(mean_terciles)]][1]
        axis(side = 2, at = y, labels = c(paste("Before ",(mean_terciles)[[1]][1,1], sep = ""), paste("Between\n",(mean_terciles)[[1]][1,1],"and",(mean_terciles)[[1]][1,2]), paste("After ",(mean_terciles)[[1]][1,2], sep = "")), cex.axis = 0.7, las = 1)
        axis(side = 4, at = y, labels = c(paste("Before ",(mean_terciles)[[length(mean_terciles)]][1,1], sep = ""), paste("Between\n",(mean_terciles)[[length(mean_terciles)]][1,1],"and",(mean_terciles)[[length(mean_terciles)]][1,2]), paste("After ",(mean_terciles)[[length(mean_terciles)]][1,2], sep = "")), cex.axis = 0.7, las = 1)
      }

      axis(side = 1, at = c(seq(from = min(tercile_data[,"year"]), to = max(tercile_data[,"year"]), by = 5)), labels = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 5)), cex.axis = 0.7, las = 2)
      axis(side = 1, at = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)), labels = c(rep("", length(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)))), cex.axis = 0.7, las = 2)
      axis(side = 3, at = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)), labels = c(rep("", length(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)))), cex.axis = 0.7, las = 2)

      if(detrended == FALSE){
        axis(side = 2, at = y, labels = c(paste("Before ",round(mean_terciles)[[1]], sep = ""), paste("Between\n",round(mean_terciles)[[1]],"and",round(mean_terciles)[[2]]), paste("After ",round(mean_terciles)[[2]], sep = "")), cex.axis = 0.7, las = 1)
      }

    }
    if(definitions == TRUE){
      par(mfrow = c(2,1), mar = c(1,4,0,2), oma = c(7,3,1,0))
      # overall counts trend
      #plot(tercile_data[,"year"], tercile_data[,"n"], lwd = 5, xlab = "", ylab = "", cex.lab = 0.8, type = "h", lend = 1, xaxt = "n",yaxt = "n", xlim = c(min(tercile_data[,"year"])+1.3,max(tercile_data[,"year"])-1.3), bty = "n", ylim = c(0, max(tercile_data[,"n"],na.rm = TRUE)), col = NA)
      #axis(side = 2, at = c(0,3000,6000,9000,12000,15000,18000), labels = c(0,3000,6000,9000,12000,15000,18000), cex.axis = 0.7, las = 1)

      #title(adj = 0, main = paste(species_name_title," ", " migration phenology for central " ,average_tercile," % of the run\n", sep = ""), xpd = NA, line = 1.5, cex.main = 0.7)
      #mtext(adj = 0, paste("Annual anomalies relative to ",min(year_range)," - ",max(year_range)," average","\n(On average, the central ",average_tercile,"% of migrating fish are counted between ordinal days ", round(mean_terciles)[[1]]," and ",round(mean_terciles)[[2]],")", sep = ""), xpd = NA, line = 0.4, cex = 0.55, side = 3)

      #par(mar=c(4,4,0,2))
      image(x,y,z_early, col =  t.color[,1],xaxt = "n",yaxt = "n", xlab = "", ylab = "", bty = "n", oldstyle = TRUE, zlim = c(0,100))
      image(x,y,z_average, col =  t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", add = TRUE, oldstyle = TRUE, zlim = c(0,100))
      image(x,y,z_late, col =  t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", add = TRUE, oldstyle = TRUE, zlim = c(0,100))
      #mtext(side = 2, paste("Ordinal day\n (relative to ",biofix_yday,")", sep = ""), xpd = NA, line = 5, cex = 0.8)

      # add definitions column to tercile data
      pBrackets::brackets(x1 = 1970, y1 = 1.83, x2 = 1975, y2 = 1.83, type = 1, col = "black", ticks = 0.5, h = -0.05, xpd = NA)
      pBrackets::brackets(x1 = 1976, y1 = 1.83, x2 = 1979, y2 = 1.83, type = 1, col = "black", ticks = 0.5, h = -0.05, xpd = NA)
      pBrackets::brackets(x1 = 1980, y1 = 1.83, x2 = 1985, y2 = 1.83, type = 1, col = "black", ticks = 0.5, h = -0.05, xpd = NA)
#      pBrackets::brackets(x1 = 1986, y1 = 1.83, x2 = 1987, y2 = 1.83, type = 1, col = "black", ticks = 0.5, h = -0.05, xpd = NA)

      #definition_col <- c("Early", "Condensed", "Late", "Bimodal")
      axis(side = 1, lwd = 0, line = 1, at = c(1972.5, 1977.5, 1982.5, 1986), labels = c("Early", "Condensed", "Late", "Bimodal"), cex.axis = 0.7, las = 2)
      abline(v = 1975.5, lty = 3)
      abline(v = 1979.5, lty = 3)
      abline(v = 1985.5, lty = 3)

      #axis(side = 1, at = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)), labels = c(rep("", length(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)))), cex.axis = 0.7, las = 2)
      #axis(side = 3, at = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)), labels = c(rep("", length(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)))), cex.axis = 0.7, las = 2)
      axis(side = 2, at = y, line = 1, labels = c("First tercile \n (Beginning of run)", "Second tercile \n (Middle of run)", "Third tercile \n (End of run)"), cex.axis = 0.7, las = 1)

    }
    #}

  }
  # ----------------------------------------------------------------------------------------------------------#
  # ADD MARKERS to indicate anomalies (more than 60% of the run were (early)/(late) definition; and more than 70% in
  # ----------------------------------------------------------------------------------------------------------#
  # central window (condensed); less than 50% in all terciles (extended))
  # average years have 27 to 39 in each tercile
  # early or late years have more than 66% in a single tercile
  ##  tercile_data_anomalies <- matrix(ncol = 5, nrow = nrow(tercile_data))
  ##  colnames(tercile_data_anomalies) <- colnames(tercile_data)
  ##  tercile_data_anomalies[,"year"] <- tercile_data[,"year"]
  ##  tercile_data_anomalies[,"n"] <- tercile_data[,"n"]
  # ----------------------------------------------------------------------------------------------------------#

  #tercile_data

  # ----------------------------------------------------------------------------------------------------------#
  # CALUCALTE AVERAGE PERCENTAGES IN EACH TERCILE AND 95% (Wald or precentile bootstrapped) CIs (also check there is no long-term trend that requires detrending...)
  # ----------------------------------------------------------------------------------------------------------#
  #par(mfrow = c(1,1))
  #plot(tercile_data$year,tercile_data$average)
  #plot(tercile_data$year,tercile_data$early, col = "blue")
  #plot(tercile_data$year,tercile_data$late, col = "red")

  #mean_tercile_data
  #x = tercile_data[,2]
  #n = 999
  #average_FUN = median
  #percentile = 0.50

  mean_tercile_data <- apply(X = tercile_data, MARGIN = 2, FUN = median, na.rm = TRUE) # mean percentage in each tercile (should this be median? would it then be 33%...?)
  #sd_tercile_data <-  apply(X = tercile_data, MARGIN = 2, FUN = sd, na.rm = TRUE) # sd for each tercile
  #n_tercile_data <- apply(X = tercile_data, MARGIN = 2, FUN = length) # number of terciles used to calc mean # 49

  #upper_CI_Wald <- mean_tercile_data + 1.96*(sd_tercile_data/sqrt(n_tercile_data)) # upper CI
  #lower_CI_Wald <- mean_tercile_data - 1.96*(sd_tercile_data/sqrt(n_tercile_data)) # lower CI

  # Bootstrapped CIs (set seed for repeatability)
  #set.seed(12345)
  upper_CI_boot <- apply(X = tercile_data, MARGIN = 2, FUN = boot_average_CI, n = 999, average_FUN = median, percentile = 0.975) # upper CI
  #set.seed(12345)
  lower_CI_boot <- apply(X = tercile_data, MARGIN = 2, FUN = boot_average_CI, n = 999, average_FUN = median, percentile = 0.025) # upper CI

  average_percent_per_tercile <- rbind(upper_CI_boot[-c(1,5)], mean_tercile_data[-c(1,5)], lower_CI_boot[-c(1,5)])
  rownames(average_percent_per_tercile) <- c("upper", "mean", "lower")
  if(!is.null(average_perc_per_tercile)){
    average_percent_per_tercile <- average_perc_per_tercile
  }

  #average_percent_per_tercile[,"early"]["upper"]
  #average_percent_per_tercile
  # ----------------------------------------------------------------------------------------------------------#

  #tercile_data
  #average_tercile
  #tercile_data_anomalies
  # ----------------------------------------------------------------------------------------------------------#
  # DEFINE TYPES OF ANOMALIES BASED ON AVERAGE PERCENTAGES FOUND INN EACH TERCILE (THIS IS FINE IN TERMS OF DETRENDING BECAUSE THE TERCILES HAVE BEEN DYNAMICALLY DEFNINED FOR EACH YEAR...) #codes   # early 1  #late 2  #bimodal 3  #average 4 #skew early 5, #skew late 6 # condensed # 7
  # ----------------------------------------------------------------------------------------------------------#
  if(bulk_pheno_three_anoms == FALSE){
    #   # early anomaly
    # tercile_data_anomalies[,"early"] <- ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],1,
    #                                            ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],1,
    #                                                   ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"]& tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],1,
    #                                                          ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                 ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                        ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                               ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                                      ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                             ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                                    ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                                           ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                                                  ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"],2,
    #                                                                                                                         ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"],2,
    #                                                                                                                                ifelse(tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],1,
    #                                                                                                                                       ifelse(tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],1,
    #                                                                                                                                                   4)))))))))))))))
    # #tercile_data
    # #average_percent_per_tercile
    # # late anomaly
    # tercile_data_anomalies[,"late"] <- ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"],2,
    #                                           ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"],2,
    #                                                  ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"]& tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"],2,
    #                                                         ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                       ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                              ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                                     ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                            ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                                   ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                                          ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                                                 ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"],2,
    #                                                                                                                        ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"],2,
    #                                                                                                                               ifelse(tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],1,
    #                                                                                                                                      ifelse(tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],1,
    #                                                                                                                                                        4)))))))))))))))
    #
    # # condensed, extended anomaly (or slight skew 0)
    # tercile_data_anomalies[,"average"] <- ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],7,
    #                                              ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],7,
    #                                                     ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"],7,
    #                                                            ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],7,
    #                                                                   ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                          ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                 ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                        ifelse(tercile_data[,"average"]<average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"],3,
    #                                                                                               ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["lower"],2,
    #                                                                                                      ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"early"]<average_percent_per_tercile[,"early"]["upper"],2,
    #                                                                                                             ifelse(tercile_data[,"late"]<average_percent_per_tercile[,"late"]["lower"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],1,
    #                                                                                                                    ifelse(tercile_data[,"late"]<average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],1,
    #                                                                                                                                                4))))))))))))
    #
    # anomaly_matrix <- data.frame(tercile_data_anomalies)
    #
    # anomaly_data_frame <- data.frame(year = as.numeric(as.character(anomaly_matrix[,"year"])),
    #                                  early = as.numeric(as.character(anomaly_matrix[,"early"])),
    #                                  average = as.numeric(as.character(anomaly_matrix[,"average"])),
    #                                  late = as.numeric(as.character(anomaly_matrix[,"late"])),
    #                                  n = as.numeric(as.character(anomaly_matrix[,"n"])))
    #
    # suppressMessages(require(dplyr))
    # average_window_years <- filter(anomaly_data_frame, early == 4 & average == 4 & late == 4)
    # #average_window_years <- subset(anomaly_data_frame, average == 4)
    # exended_window_years <- subset(anomaly_data_frame, average == 3)
    # early_years <- subset(anomaly_data_frame, early == 1)
    # late_years <- subset(anomaly_data_frame, late == 2)
    # condensed_years <- subset(anomaly_data_frame, average == 7)
    #

    ##############################################################################################################
    # TEST NEW DEFINITIONS
    ##############################################################################################################
    # define dataframe to polulate (with 1 row...)
    #  print(tercile_data)
    tercile_data_anomalies <- as.data.frame(matrix(ncol = 3, nrow = nrow(tercile_data)))
    colnames(tercile_data_anomalies) <- c(colnames(tercile_data)[-c(2:4)],"anom")
    #print(tercile_data_anomalies)
    tercile_data_anomalies[,"year"] <- tercile_data[,"year"]
    tercile_data_anomalies[,"n"] <- tercile_data[,"n"]
    #tercile_data_anomalies[,"member"] <- tercile_data[,"member"]

    # populate anom column (this is the function equivalent of anomaly relative to mean() in lake model scripts)
    # early 1  #late 2  #bimodal 3  #average 4  # condensed # 7
    # early tercile
    tercile_data_anomalies[,"anom"] <- ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["lower"],"early",
                                              ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["lower"],"early",
                                                     ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["upper"]& tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],"early",
                                                            ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["lower"],"late",
                                                                   ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["lower"],"late",
                                                                          ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["upper"]& tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"],"late",
                                                                                 ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["lower"],"condensed",
                                                                                        ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],"condensed",
                                                                                               ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["lower"],"condensed",
                                                                                                      ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"],"condensed",
                                                                                                             ifelse(tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],"bimodal",
                                                                                                                    ifelse(tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"],"late",
                                                                                                                           ifelse(tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["upper"],"early",
                                                                                                                                  ifelse(tercile_data[,"average"]<=average_percent_per_tercile[,"average"]["lower"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"]& tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"]& tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["upper"],"bimodal",
                                                                                                                                         ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["lower"],"late",
                                                                                                                                                ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"late"]["upper"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["lower"] & tercile_data[,"early"]<=average_percent_per_tercile[,"early"]["upper"],"late",
                                                                                                                                                       ifelse(tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["lower"]  & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],"early",
                                                                                                                                                              ifelse(tercile_data[,"late"]<=average_percent_per_tercile[,"late"]["upper"] & tercile_data[,"late"]>average_percent_per_tercile[,"late"]["lower"] & tercile_data[,"average"]>average_percent_per_tercile[,"average"]["upper"] & tercile_data[,"early"]>average_percent_per_tercile[,"early"]["upper"],"early","average"))))))))))))))))))


    anomaly_matrix <- tercile_data_anomalies

    anomaly_data_frame <- data.frame(year = as.numeric(as.character(anomaly_matrix[,"year"])),
                                     anom = as.character(as.character(anomaly_matrix[,"anom"])),
                                     n = as.numeric(as.character(anomaly_matrix[,"n"])))

#    suppressMessages(require(dplyr))
    average_window_years <- anomaly_data_frame[anomaly_data_frame$anom == "average",]
    exended_window_years <- anomaly_data_frame[anomaly_data_frame$anom == "bimodal",]
    early_years <- anomaly_data_frame[anomaly_data_frame$anom == "early",]
    late_years <- anomaly_data_frame[anomaly_data_frame$anom == "late",]
    condensed_years <- anomaly_data_frame[anomaly_data_frame$anom == "condensed",]

#    average_window_years <- subset(anomaly_data_frame, anom == "average")
#    exended_window_years <- subset(anomaly_data_frame, anom == "bimodal")
#    early_years <- subset(anomaly_data_frame, anom == "early")
#    late_years <- subset(anomaly_data_frame, anom == "late")
#    condensed_years <- subset(anomaly_data_frame, anom == "condensed")

  }
  ##############################################################################################################

  # ----------------------------------------------------------------------------------------------------------#
  # BULK PHENOLOGY (THREE ANOMS ONLY - FOR ROBUST STATISTICAL VALIDATION WITH 50 YEARS DATA
  # ----------------------------------------------------------------------------------------------------------#
  # # early 1  #late 2  #bimodal 3  #average 4 #skew early 5, #skew late 6 # condensed # 7
  if(bulk_pheno_three_anoms == TRUE){
    #define anomalies
    # early anomaly
    tercile_data_anomalies[,"early"] <- ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"average"]["upper"],1,
                                               ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"average"]["upper"],2,
                                                      ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"],4,
                                                             4)))
    #tercile_data
    #average_percent_per_tercile
    # late anomaly
    tercile_data_anomalies[,"late"] <- ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"average"]["upper"],1,
                                              ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"average"]["upper"],2,
                                                     ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"],4,
                                                            4)))

    # condensed, extended anomaly (or slight skew 0)
    tercile_data_anomalies[,"average"] <- ifelse(tercile_data[,"early"]>average_percent_per_tercile[,"average"]["upper"],1,
                                                 ifelse(tercile_data[,"late"]>average_percent_per_tercile[,"average"]["upper"],2,
                                                        ifelse(tercile_data[,"average"]>average_percent_per_tercile[,"average"]["lower"],4,
                                                               4)))

    anomaly_matrix <- data.frame(tercile_data_anomalies)

    anomaly_data_frame <- data.frame("year" = as.numeric(as.character(anomaly_matrix[,"year"])),
                                     "early" = as.numeric(as.character(anomaly_matrix[,"early"])),
                                     "average" = as.numeric(as.character(anomaly_matrix[,"average"])),
                                     "late" = as.numeric(as.character(anomaly_matrix[,"late"])),
                                     "n" = as.numeric(as.character(anomaly_matrix[,"n"])))

    average_window_years <- anomaly_data_frame[anomaly_data_frame$early == 4 & anomaly_data_frame$average == 4 & anomaly_data_frame$late == 4,]
    #average_window_years <- dplyr::filter(anomaly_data_frame, early == 4 & average == 4 & late == 4)
    early_years <- anomaly_data_frame[anomaly_data_frame$early == 1,]
    late_years <- anomaly_data_frame[anomaly_data_frame$late == 2,]
    #early_years <- subset(anomaly_data_frame, early == 1)
    #late_years <- subset(anomaly_data_frame, late == 2)
  }
  # ----------------------------------------------------------------------------------------------------------#

  if(definitions == TRUE){
    exended_window_years <- data.frame("year" = c(1986))
    early_years <- data.frame("year" = c(1970:1975))
    late_years <- data.frame("year" = c(1980:1985))
    condensed_years <- data.frame("year" = c(1976:1979))
    average_window_years <- data.frame("year" = c(NA))
  }

  # ----------------------------------------------------------------------------------------------------------#
  # ----------------------------------------------------------------------------------------------------------#
  if(plot == TRUE & bulk_pheno_three_anoms == FALSE){
    # early
    points(early_years[,"year"], rep(1.9, times = length(early_years[,"year"])), col = "blue", lwd = 2, pch = 21, bg = "white")
    # late
    points(late_years[,"year"], rep(2.1, times = length(late_years[,"year"])), col = "red", lwd = 2, pch = 21, bg = "white")
    # condensed
    points(condensed_years[,"year"], rep(2.0, times = length(condensed_years[,"year"])), col = "purple", lwd = 2, pch = 21, bg = "purple")
    # extended window anomalies
    segments(x0 = exended_window_years$year,
             y0 = rep(1.9, times = nrow(exended_window_years)), x1 = exended_window_years$year, y1 = rep(2.1, times = nrow(exended_window_years)), col="green3", lwd = 4, lend = 1)
    points(exended_window_years[,"year"], rep(1.9, times = length(exended_window_years[,"year"])), col = "green3", lwd = 2, pch = 21, bg = "green3")
    points(exended_window_years[,"year"], rep(2.1, times = length(exended_window_years[,"year"])), col = "green3", lwd = 2, pch = 21, bg = "green3")

    # average
#    segments(x0 = average_window_years$year,
#             y0 = rep(1.9, times = nrow(average_window_years)), x1 = average_window_years$year, y1 = rep(2.1, times = nrow(average_window_years)), col="black", lwd = 4, lend = 1)
    points(average_window_years[,"year"], rep(2.0, times = length(average_window_years[,"year"])), col = "black", lwd = 2, pch = 21, bg = "white")

    # ----------------------------------------------------------------------------------------------------------#
    #
    # ----------------------------------------------------------------------------------------------------------#
    # ADD LEGEND
    # ----------------------------------------------------------------------------------------------------------#

    if(definitions == TRUE){
      par(oma=c(4,4,0,0))
      fields::image.plot(add = TRUE, x,y,z_early, col = t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.38,0.45), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_average, col = t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.28,0.35), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_late, col = t.color[,1],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.cex = 0.8,legend.line= 3, legend.lab = "% counts per tercile \n (Mean % counts per tercile CI: 24 - 35%)",legend.only = TRUE, smallplot = c(0.40,0.85,0.18,0.25), axis.args=list(cex.axis=0.60,at=c(0,24,35,100), labels=c(0,24,35,100)), zlim = c(0,100))
    }
    if(definitions == FALSE){
      par(oma=c(7,4,0,0))
      fields::image.plot(add = TRUE, x,y,z_early, col = t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.25,0.28), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_average, col = t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.20,0.23), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_late, col = t.color[,1],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.cex = 0.8, legend.lab = "% counts per tercile",legend.only = TRUE, smallplot = c(0.40,0.85,0.15,0.18), lab.breaks=c(seq(from = 0, to = 100, by = 10)), axis.args=list(cex.axis=0.65),zlim = c(0,100))
    }

    # legend for phenology
    if(definitions == FALSE){
      legend("bottomleft",
             inset = c(-0.15,-0.06),
             pch = c(1,21,1,21,1),
             lty = c(0,1,0,0,0),
             lwd = c(2,4,2,2,2),
             pt.bg = c(NA,"green3",NA,"purple",NA),
             col = c("red","green3","blue","purple","black"),
             legend = c("Late","Bimodal","Early","Condensed","Average"),
             cex = c(0.8),
             pt.cex = c(1,1,1,1,1),
             bty = "n",
             xpd = NA)
    }
    # ----------------------------------------------------------------------------------------------------------#
    pheno_plot <- recordPlot()
    #  invisible(dev.off())
  }
  # ----------------------------------------------------------------------------------------------------------#
  # ----------------------------------------------------------------------------------------------------------#

  # ----------------------------------------------------------------------------------------------------------#
  # ----------------------------------------------------------------------------------------------------------#
  if(plot == TRUE & bulk_pheno_three_anoms == TRUE){
    points(early_years[,"year"], rep(1.9, times = length(early_years[,"year"])), col = "blue", lwd = 2, pch = 21, bg = "white")
    points(late_years[,"year"], rep(2.1, times = length(late_years[,"year"])), col = "red", lwd = 2, pch = 21, bg = "white")
    # extended window anomalies

    # ----------------------------------------------------------------------------------------------------------#
    #
    # ----------------------------------------------------------------------------------------------------------#
    # ADD LEGEND
    # ----------------------------------------------------------------------------------------------------------#

    if(definitions == TRUE){
      par(oma=c(4,4,0,0))
      fields::image.plot(add = TRUE, x,y,z_early, col = t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.38,0.45), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_average, col = t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.28,0.35), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_late, col = t.color[,1],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.cex = 0.8,legend.line= 3, legend.lab = "% counts per tercile \n (Mean % counts per tercile CI: 24 - 35%)",legend.only = TRUE, smallplot = c(0.40,0.85,0.18,0.25), axis.args=list(cex.axis=0.60,at=c(0,24,35,100), labels=c(0,24,35,100)), zlim = c(0,100))
    }
    if(definitions == FALSE){
      par(oma=c(7,4,0,0))
      fields::image.plot(add = TRUE, x,y,z_early, col = t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.25,0.28), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_average, col = t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.20,0.23), lab.breaks=c(rep("", nrow(t.color)+1)))
      fields::image.plot(add = TRUE, x,y,z_late, col = t.color[,1],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.cex = 0.8, legend.lab = "% counts per tercile",legend.only = TRUE, smallplot = c(0.40,0.85,0.15,0.18), lab.breaks=c(seq(from = 0, to = 100, by = 10)), axis.args=list(cex.axis=0.65),zlim = c(0,100))
    }

    # legend for phenology
    if(definitions == FALSE){
      legend("bottomleft", inset = c(0.0,-0.06), pch = c(1,1), lty = c(0,0), lwd = c(2,2), col = c("red","blue"), legend = c("Late", "Early"), cex = c(0.8), pt.cex = c(1,1), bty = "n", xpd = NA)
    }
    # ----------------------------------------------------------------------------------------------------------#
    pheno_plot <- recordPlot()
    #    invisible(dev.off())
  }
  # ----------------------------------------------------------------------------------------------------------#
  # ----------------------------------------------------------------------------------------------------------#
  # COMPILE TABLE WITH ALL FORECASTS FOR EACH MEMBER THROUGHOUT THE TIME SERIES
  # early_df = ifelse(!is.null("early_years"),
  #                   data.frame("year" = early_years[,"year"],
  #                              "anomaly" = "early"),NULL)
  # late_df = ifelse(!is.null("late_years"),
  #                  data.frame("year" = late_years[,"year"],
  #                             "anomaly" = "late"),NULL)
  # bimodal_df = ifelse(!is.null("exended_window_years"),
  #                     data.frame("year" = exended_window_years[,"year"],
  #                                "anomaly" = "extended"),NULL)
  # condensed_df = ifelse(!is.null("condensed_years"),
  #                       data.frame("year" = condensed_years[,"year"],
  #                                  "anomaly" = "condensed"),NULL)
  # average_df = ifelse(!is.null("average_window_years"),
  #                     data.frame("year" = average_window_years[,"year"],
  #                                "anomaly" = "average"),NULL)

  #anom_df <- Reduce(function(x, y) merge(x, y, by = "year"),
  #                  list(early_df,
  #                       late_df,
  #                       bimodal_df,
  #                       condensed_df,
  #                       average_df))

  #anomaly_table <- anom_df[order(anom_df$year),]

  if(return_data == TRUE){
    if(trend_plot == TRUE){
      return(list(average_percent_per_tercile = average_percent_per_tercile,
                  mean_terciles = mean_terciles,
                  tercile_data = tercile_data,
                  average = average_window_years[,"year"],
                  extended = exended_window_years[,"year"],
                  early = early_years[,"year"],
                  late = late_years[,"year"],
                  condensed = condensed_years[,"year"],
                  mean_terciles_raw = mean_terciles_raw,
                  earliest_date = earliest_date,
                  earliest_date_yday = earliest_date_yday))
    }
    if(trend_plot == FALSE & bulk_pheno_three_anoms == FALSE & plot == TRUE){
      return(list(average_percent_per_tercile = average_percent_per_tercile,
                  mean_terciles = mean_terciles,
                  tercile_data = tercile_data,
                  average = average_window_years[,"year"],
                  extended = exended_window_years[,"year"],
                  early = early_years[,"year"],
                  late = late_years[,"year"],
                  condensed = condensed_years[,"year"],
                  earliest_date = earliest_date,
                  earliest_date_yday = earliest_date_yday,
                  pheno_plot = pheno_plot))
    }
    if(trend_plot == FALSE & bulk_pheno_three_anoms == TRUE & plot == TRUE){
      return(list(average_percent_per_tercile = average_percent_per_tercile,
                  mean_terciles = mean_terciles,
                  tercile_data = tercile_data,
                  average = average_window_years[,"year"],
                  early = early_years[,"year"],
                  late = late_years[,"year"],
                  earliest_date = earliest_date,
                  earliest_date_yday = earliest_date_yday,
                  pheno_plot = pheno_plot))
    }
    if(trend_plot == FALSE & bulk_pheno_three_anoms == FALSE & plot == FALSE){
      return(list(average_percent_per_tercile = average_percent_per_tercile,
                  mean_terciles = mean_terciles,
                  tercile_data = tercile_data,
                  average = average_window_years[,"year"],
                  extended = exended_window_years[,"year"],
                  early = early_years[,"year"],
                  late = late_years[,"year"],
                  condensed = condensed_years[,"year"],
                  earliest_date = earliest_date,
                  earliest_date_yday = earliest_date_yday))
    }
    if(trend_plot == FALSE & bulk_pheno_three_anoms == TRUE & plot == FALSE){
      return(list(average_percent_per_tercile = average_percent_per_tercile,
                  mean_terciles = mean_terciles,
                  tercile_data = tercile_data,
                  average = average_window_years[,"year"],
                  early = early_years[,"year"],
                  late = late_years[,"year"],
                  earliest_date = earliest_date,
                  earliest_date_yday = earliest_date_yday))
    }
  }
}
