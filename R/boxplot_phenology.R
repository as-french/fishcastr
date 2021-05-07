#' Generate boxplot illustrating migration phenology anomalies from daily
#' ecological counts.
#'
#' @description This function takes vectors of dates, counts, biological years
#'   and biological year days and illustrates inter-annual variation in
#'   phenological anomalies relative to long term (full series average; mean or
#'   median) or moving average expectations (year days on which mean or median
#'   is expected to occur follow a linear trend). Anomalies are illustrated
#'   using a colour coded boxplot, which illustrates whether each summary
#'   statistic (e.g., mean or median) is assigned to lower, middle or upper
#'   tercile; that is, during a defined central expectation window (e.g., mean
#'   +- 95% CI, which may shift linearly with time). We expect one third of
#'   means to be recorded in each tercile and we define three anomalies: early,
#'   late, average. The user can specify if anomalies are calculated using the
#'   mean-tercile method (described above), or a variation of this method, the
#'   median-tercile, or based on bulk movements of 33.3% and 66.6% percentiles
#'   (as described in fishcastr::tercile_plot_phenology()). Can also choose a
#'   percentile of interest, (e.g., 0.05) for the beginning of a fish migration
#'   run. The purpose of this plot is to illustrate to the user that arbitrary
#'   selection of summary statistics to infer long term trends in phenology can
#'   be misleading in cases where phenology curves are not symmetrical (e.g., in
#'   diadromous fish count time series). Bulk movements may be more useful for
#'   multimodal phenology curves.
#'
#' @param bio_year A vector of biological years concurrent with daily counts.
#' @param counts A vector of daily counts.
#' @param yday A vector of biologically relevant year days.
#' @param method A character string, specifying the method for calculating
#'   anomalies (e.g., "mean","median",or "33_66"). Default "mean". Can also
#'   choose a percentile of interest, (currently: "5th", "25th", "33rd", "66th",
#'   "75th" and "95") for the percentiles of fish migration run. IMPORTANT -
#'   Note that the anomaly colour codings in plots will relate directly to the
#'   chosen method, and will therefore not necessarily represent anomalies for
#'   central measures (mean, medians), particularly where data are not unimodal.
#' @param species_name A character string.
#' @param season A character string.
#' @param dates A vector of class "Date".
#' @param unreliable_years A concatenated list of unreliable years (e.g., years
#'   during which floods led to unreliable silver eel counts)
#' @param plot Boolean indicating if plot should be returned (e.g., in addition
#'   to returned data). See note for method for explanation of colours.
#' @param mean_tercile_dates A list of matrices containing (moving) average
#'   biological year day quantiles (e.g., average year day of 33rd and 66th
#'   percentiles and upper and lower CIs). This argument is only required for
#'   comparing observed and simulated phenology. Default NULL.
#' @param average_perc_per_tercile A matrix of average (median) percentages of
#'   counts observed in each tercile with upper and lower bootstrapped CIs. This
#'   argument is only required for comparing observed and simulated phenology.
#'   Default NULL.
#' @param detrended Boolean to determine if quantile trends are taken into
#'   account; i.e., if detrended, the biological year day delineating the mean
#'   +- 95% CIs may vary among years with a linear trend. Default TRUE.
#' @param summary_stat_calculation_months The moths for which counts should be
#'   used to calculate migration summary statistics (e.g., MAMJ; c(3:6))
#' @param ... Additional arguments to nested functions.
#' @return A list. "summary_stats", A 12 X no. years summary table, containing
#'   summary statistics for ecological time series count data; namely: mean,
#'   median, and 5th, 25th, 33rd, 50th, 66th, 75th, 95th percentile biological
#'   year days - this table also contains the assigned anomaly for each year in
#'   the series and plots it colour coded (i.e., blue: early; red: late; yellow:
#'   no different to average); "mean_terciles", the long term (or moving)
#'   average position of the 33rd and 66th percentiles of the selected summary
#'   statistic (e.g., the mean); "earliest_date_yday", the lowest calendar year
#'   day (i.e., not forecast year day) of the time series (e.g., 60 for salmon
#'   smolts, which is 60th day of the year, not 60 days post winter solstice) -
#'   this is not used within the function; "summary_stat_calculation_months",
#'   returns the selected target season in a concatenated list of months (e.g.,
#'   2:7).
#' @examples
#' \dontrun{
#'  data_counts <-
#'    readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#'
#'  data_counts_subset <- data_counts[data_counts$salmonid_year %in% c(1981:2018),]
#'
#' # boxplot and return list
#'  test <- fishcastr::boxplot_phenology(
#'      bio_year = data_counts_subset$salmonid_year,
#'      counts = data_counts_subset$ssmolt,
#'      yday = data_counts_subset$salmonid_yday,
#'      dates = data_counts_subset$date,
#'      method = "mean",
#'      species_name = "Atlantic salmon (smolt)",
#'      season = "Spring",
#'      summary_stat_calculation_months = c(3:7),
#'      plot = TRUE,
#'      detrended = FALSE
#'    )
#'}
#' @export
boxplot_phenology <- function(bio_year,
                              counts,
                              yday,
                              method = "mean",
                              species_name,
                              season,
                              dates,
                              unreliable_years = NULL,
                              plot = TRUE,
                              mean_tercile_dates = NULL,
                              average_perc_per_tercile = NULL,
                              detrended = TRUE,
                              summary_stat_calculation_months = NULL,
                              ...) {
  # reformat data into bioydays to calculate means, medians, percentiles etc.,
  # load in the data...

  # earliest day of year biofix (to replace manual input of biofix_yday text)
  dataset <- data.frame("dates" = dates, "yday" = yday)
  earliest_date <- dataset[,"dates"][which.min(dataset[,"yday"])]
  earliest_date_yday <- lubridate::yday(earliest_date)

  # subset to forecast window months (summary_stat_calculation_months)
  if(!is.null(summary_stat_calculation_months)){
    df <- data.frame("bio_year" = bio_year,
                     "counts" = counts,
                     "yday" = yday,
                     "dates" = dates,
                     "months" = lubridate::month(dates))

    df_subset <- df[df$months %in% summary_stat_calculation_months, ]

    dataset <- data.frame("dates" = df_subset$dates, "yday" = df_subset$yday)
    earliest_date <- dataset[,"dates"][which.min(dataset[,"yday"])]
    earliest_date_yday <- lubridate::yday(earliest_date)

    bio_year = df_subset$bio_year
    counts = df_subset$counts
    yday = df_subset$yday
    dates = df_subset$dates
  }

  # calculate median year day for all years and plot (if required uncomment)
  #x = 1996
  summary_stat_list <- lapply(
    unique(bio_year),
    FUN = function(x) {

      expand <- rep(x = yday[bio_year == x], times = counts[bio_year == x])
      #median_yday <- median(expand)
      mean_yday <- (mean(expand))
      percentiles <-
        quantile(x = expand,
                 probs = c(0.05, 0.25, 1/3, 0.50, 2/3, 0.75, 0.95))
      perc_5 <- (percentiles[1])
      perc_25 <- (percentiles[2])
      perc_33 <- (percentiles[3])
      perc_50 <- (percentiles[4])
      perc_66 <- (percentiles[5])
      perc_75 <- (percentiles[6])
      perc_95 <- (percentiles[7])
      num_counts <- sum(counts[bio_year == x])

      result = c(x, mean_yday, perc_5, perc_25, perc_33, perc_50, perc_66, perc_75, perc_95,num_counts)
      names(result) <-
        c(
          "year",
          "mean",
          "5th",
          "25th",
          "33rd",
          "50th",
          "66th",
          "75th",
          "95th",
          "n"
        )
      return(result)
    }
  )

  # unlist result
  summary_stats <- as.data.frame(do.call(rbind, summary_stat_list))

  if(!is.null(unreliable_years)){
    summary_stats <- summary_stats[!(summary_stats$year %in% unreliable_years),]
  }

  if (method == "mean" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)


    #summary_stats$mean check not rounded

    #middle_tercile_bondaries <- as.data.frame(matrix(quantile(summary_stats$mean,
    #                                                          probs = c(1/3, 2/3)),
    #                                                 ncol = 2,
    #                                                 dimnames = list("mean",c("33.3%","66.6%"))))

    ########
    # 23 Feb 2021 force split into equal group occupancy to replace old method
    # below (assuming no repeated mean day values, so do not round before this operation)
    #require(Hmisc)
   # max(summary_stats$mean)
    #table(test)
    #test <- as.numeric(Hmisc::cut2(summary_stats$mean, g=3, onlycuts = TRUE))[2:3]

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$mean,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))
    ##########

    summary_stats$anom <-
      ifelse(
        summary_stats$mean < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$mean >= middle_tercile_bondaries[[1]] &
            summary_stats$mean < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$mean < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$mean >= middle_tercile_bondaries[[1]] &
            summary_stats$mean < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "mean" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$mean ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$mean)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$mean,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$mean < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$mean >= mean_detrended_resids_lower_lim &
              summary_stats$mean < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$mean < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$mean >= mean_detrended_resids_lower_lim &
              summary_stats$mean < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatibility with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

      })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$mean,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$mean < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$mean >= middle_tercile_bondaries[[1]] &
              summary_stats$mean < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$mean < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$mean >= middle_tercile_bondaries[[1]] &
              summary_stats$mean < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "50th" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    # although dimnames says mean, it is the 50th...
    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`50th`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`50th` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`50th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`50th` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )
    summary_stats$anom_col <-
      ifelse(
        summary_stats$`50th` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`50th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`50th` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "50th" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)
    #lm
    mod <- lm(summary_stats$`50th` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`50th`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`50th`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended



      summary_stats$anom <-
        ifelse(
          summary_stats$`50th` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`50th` >= mean_detrended_resids_lower_lim &
              summary_stats$`50th` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`50th` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`50th` >= mean_detrended_resids_lower_lim &
              summary_stats$`50th` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatibility with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })

    }

    if (pval >= 0.05) {

      # although dimnames says mean, it is the 50th
      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`50th`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`50th` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`50th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`50th` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )
      summary_stats$anom_col <-
        ifelse(
          summary_stats$`50th` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`50th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`50th` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "33_66" & detrended == FALSE) {
    phenology_anomalies <- tercile_plot_phenology(
      bio_year = bio_year,
      counts = counts,
      yday = yday,
      dates = dates,
      terciles = c(1/3, 2/3),
      species_name = species_name,
      season = season,
      return_data = TRUE,
      plot = FALSE,
      definitions = FALSE,
      detrended = detrended,
      trend_plot = FALSE,
      unreliable_years = unreliable_years
    )
    #ave
    ave_mat <-
      cbind(
        rep("average", times = length(phenology_anomalies$average)),
        rep("darkgrey", times = length(phenology_anomalies$average)),
        phenology_anomalies$average
      )
    late_mat <-
      cbind(
        rep("late", times = length(phenology_anomalies$late)),
        rep("red", times = length(phenology_anomalies$late)),
        phenology_anomalies$late
      )
    early_mat <-
      cbind(
        rep("early", times = length(phenology_anomalies$early)),
        rep("blue", times = length(phenology_anomalies$early)),
        phenology_anomalies$early
      )
    bimodal_mat <-
      cbind(
        rep("bimodal", times = length(phenology_anomalies$extended)),
        rep("green3", times = length(phenology_anomalies$extended)),
        phenology_anomalies$extended
      )
    condensed_mat <-
      cbind(
        rep("condensed", times = length(phenology_anomalies$condensed)),
        rep("purple", times = length(phenology_anomalies$condensed)),
        phenology_anomalies$condensed
      )
    anom_mat <-
      as.data.frame(rbind(ave_mat, late_mat, early_mat, bimodal_mat, condensed_mat))
    #anom_mat
    anom_mat_ordered <- anom_mat[order(anom_mat$V3), ]
    summary_stats$anom <- anom_mat_ordered$V1
    summary_stats$anom_col <- as.character(anom_mat_ordered$V2)
    middle_tercile_bondaries <- phenology_anomalies$mean_terciles
  }

  if (method == "33_66" & detrended == TRUE) {
    phenology_anomalies <- tercile_plot_phenology(
      bio_year = bio_year,
      counts = counts,
      yday = yday,
      dates = dates,
      terciles = c(1/3, 2/3),
      species_name = species_name,
      season = season,
      return_data = TRUE,
      plot = FALSE,
      definitions = FALSE,
      detrended = detrended,
      trend_plot = FALSE,
      unreliable_years = unreliable_years
    )
    #ave
    ave_mat <-
      cbind(
        rep("average", times = length(phenology_anomalies$average)),
        rep("darkgrey", times = length(phenology_anomalies$average)),
        phenology_anomalies$average
      )
    late_mat <-
      cbind(
        rep("late", times = length(phenology_anomalies$late)),
        rep("red", times = length(phenology_anomalies$late)),
        phenology_anomalies$late
      )
    early_mat <-
      cbind(
        rep("early", times = length(phenology_anomalies$early)),
        rep("blue", times = length(phenology_anomalies$early)),
        phenology_anomalies$early
      )
    bimodal_mat <-
      cbind(
        rep("bimodal", times = length(phenology_anomalies$extended)),
        rep("green3", times = length(phenology_anomalies$extended)),
        phenology_anomalies$extended
      )
    condensed_mat <-
      cbind(
        rep("condensed", times = length(phenology_anomalies$condensed)),
        rep("purple", times = length(phenology_anomalies$condensed)),
        phenology_anomalies$condensed
      )
    anom_mat <-
      as.data.frame(rbind(ave_mat, late_mat, early_mat, bimodal_mat, condensed_mat))
    #anom_mat
    anom_mat_ordered <- anom_mat[order(anom_mat$V3), ]
    summary_stats$anom <- anom_mat_ordered$V1
    summary_stats$anom_col <- as.character(anom_mat_ordered$V2)

    # extract detrended 33 and 66 expectation (mean) yday positions...
    mean_ext <-
      lapply(
        phenology_anomalies$mean_terciles,
        FUN = function(x) {
          x["mean", ]
        }
      )
    mean_33_66 <- do.call(rbind, mean_ext)
    middle_tercile_bondaries <- mean_33_66
  }

  # ADDED METHODS 2020-12-17

  if (method == "5th" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`5th`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`5th` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`5th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`5th` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$`5th` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`5th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`5th` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "5th" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$`5th` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`5th`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`5th`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$`5th` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`5th` >= mean_detrended_resids_lower_lim &
              summary_stats$`5th` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`5th` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`5th` >= mean_detrended_resids_lower_lim &
              summary_stats$`5th` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatability with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`5th`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`5th` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`5th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`5th` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`5th` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`5th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`5th` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "25th" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`25th`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    #testgrps <- as.numeric(Hmisc::cut2(summary_stats$`25th`,
    #                                   g=3,
    #                                   onlycuts = FALSE))
    #table(testgrps)
    summary_stats$anom <-
      ifelse(
        summary_stats$`25th` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`25th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`25th` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$`25th` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`25th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`25th` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "25th" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$`25th` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`25th`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`25th`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$`25th` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`25th` >= mean_detrended_resids_lower_lim &
              summary_stats$`25th` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`25th` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`25th` >= mean_detrended_resids_lower_lim &
              summary_stats$`25th` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatability with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`25th`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`25th` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`25th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`25th` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`25th` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`25th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`25th` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "33rd" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`33rd`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`33rd` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`33rd` >= middle_tercile_bondaries[[1]] &
            summary_stats$`33rd` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$`33rd` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`33rd` >= middle_tercile_bondaries[[1]] &
            summary_stats$`33rd` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "33rd" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$`33rd` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`33rd`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`33rd`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$`33rd` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`33rd` >= mean_detrended_resids_lower_lim &
              summary_stats$`33rd` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`33rd` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`33rd` >= mean_detrended_resids_lower_lim &
              summary_stats$`33rd` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatability with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`33rd`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`33rd` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`33rd` >= middle_tercile_bondaries[[1]] &
              summary_stats$`33rd` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`33rd` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`33rd` >= middle_tercile_bondaries[[1]] &
              summary_stats$`33rd` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "66th" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`66th`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`66th` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`66th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`66th` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$`66th` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`66th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`66th` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "66th" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$`66th` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`66th`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`66th`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$`66th` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`66th` >= mean_detrended_resids_lower_lim &
              summary_stats$`66th` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`66th` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`66th` >= mean_detrended_resids_lower_lim &
              summary_stats$`66th` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatability with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`66th`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`66th` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`66th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`66th` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`66th` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`66th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`66th` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "75th" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`75th`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`75th` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`75th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`75th` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$`75th` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`75th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`75th` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "75th" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$`75th` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`75th`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`75th`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$`75th` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`75th` >= mean_detrended_resids_lower_lim &
              summary_stats$`75th` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`75th` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`75th` >= mean_detrended_resids_lower_lim &
              summary_stats$`75th` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatability with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`75th`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`75th` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`75th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`75th` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`75th` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`75th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`75th` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

  if (method == "95th" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`95th`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`95th` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`95th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`95th` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$`95th` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`95th` >= middle_tercile_bondaries[[1]] &
            summary_stats$`95th` < middle_tercile_bondaries[[2]],
          "yellow",
          "red"
        )
      )
  }

  if (method == "95th" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$`95th` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`95th`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`95th`,
                               g=3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$`95th` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`95th` >= mean_detrended_resids_lower_lim &
              summary_stats$`95th` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`95th` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`95th` >= mean_detrended_resids_lower_lim &
              summary_stats$`95th` < mean_detrended_resids_upper_lim,
            "yellow",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind((mean_detrended_resids_lower_lim),
              (mean_detrended_resids_upper_lim))

      # reformat to list object for compatability with plot_historical_context_op_forecast
      middle_tercile_bondaries <- lapply(1:nrow(middle_tercile_bondaries_table),
                                         FUN = function(x){
                                           result = as.data.frame(matrix(middle_tercile_bondaries_table[x,],
                                                                         ncol = 2,
                                                                         dimnames = list("mean",c("33.3%","66.6%"))))
                                           return(result)

                                         })


    }

    if (pval >= 0.05) {

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`95th`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`95th` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`95th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`95th` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`95th` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`95th` >= middle_tercile_bondaries[[1]] &
              summary_stats$`95th` < middle_tercile_bondaries[[2]],
            "yellow",
            "red"
          )
        )
    }

  }

if(plot == TRUE){
  plot(
    summary_stats$year,
    summary_stats$mean,
    cex = 1,
    ylim = c(min(summary_stats[, 2:7]), max(summary_stats[, 2:7])),
    col = NULL,
    ylab = "Biological year day",
    xlab = "Year"
  )
  # range 5 to 95th
  segments(
    x0 = summary_stats$year,
    y0 = summary_stats$`5th`,
    x1 = summary_stats$year,
    y1 = summary_stats$`95th`,
    col = summary_stats$anom_col,
    lty = 1,
    lwd = 1,
    lend = 2
  )
  # range 33 tp 66
  segments(
    x0 = summary_stats$year,
    y0 = summary_stats$`33rd`,
    x1 = summary_stats$year,
    y1 = summary_stats$`66th`,
    col = "black",
    lty = 1,
    lwd = 13,
    lend = 2
  )
  segments(
    x0 = summary_stats$year,
    y0 = summary_stats$`33rd`,
    x1 = summary_stats$year,
    y1 = summary_stats$`66th`,
    col = summary_stats$anom_col,
    lty = 1,
    lwd = 11,
    lend = 2
  )
  # 50th
  segments(
    x0 = summary_stats$year - 0.25,
    y0 = summary_stats$`50th`,
    x1 = summary_stats$year + 0.25,
    y1 = summary_stats$`50th`,
    col = "black",
    lty = 1,
    lwd = 1,
    lend = 2
  )
  #points(summary_stats$year,summary_stats$`50th`,col = "purple", pch = )
  points(
    summary_stats$year,
    summary_stats$mean,
    col = "black",
    lwd = 1,
    pch = 21,
    bg = "white"
  )

  if (method %in% c("mean", "50th")) {
    if (detrended == FALSE) {
      abline(h = middle_tercile_bondaries, lty = c(2, 2))
    }

    if(detrended == TRUE){

    if (pval >= 0.05) {
      abline(h = middle_tercile_bondaries, lty = c(2, 2))
    }

    if (pval < 0.05) {
      lines(x = summary_stats$year,
            y = middle_tercile_bondaries[, 1],
            lty = 2)
      lines(x = summary_stats$year,
            y = middle_tercile_bondaries[, 2],
            lty = 2)
    }
    }

  }

  if (method == "33_66") {
    if (detrended == FALSE) {
      abline(h = middle_tercile_bondaries, lty = c(2, 2))
    }
    if (detrended == TRUE) {

      if(!is.null(unreliable_years)){
        middle_tercile_bondaries <- middle_tercile_bondaries[!(rownames(middle_tercile_bondaries) %in% unreliable_years),]
      }

      lines(x = summary_stats$year,
            y = middle_tercile_bondaries[, 1],
            lty = 2)
      lines(x = summary_stats$year,
            y = middle_tercile_bondaries[, 2],
            lty = 2)
    }
  }
}

  return(list(summary_stats = summary_stats,
              mean_terciles = (middle_tercile_bondaries),
              earliest_date_yday = earliest_date_yday,
              summary_stat_calculation_months = summary_stat_calculation_months))

}
