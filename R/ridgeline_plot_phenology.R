#' Plot phenology anomalies for multi-year dataset in "ridge line"/"joyplot"
#' style
#'
#' @description This function is intended for use with multi-decade ecological
#'   count time series to facilitate illustration of interannual variation in
#'   phenology. The function takes vectors of dates, counts, biological years
#'   and biological year days and illustrates inter-annual variation in
#'   phenological anomalies relative to long term (full series average; mean or
#'   median) or moving average expectations (year days on which mean or median
#'   is expected to occur follow a linear trend). Anomalies are illustrated
#'   using a colour coded joyplot/ridge line plot, which illustrates whether
#'   each summary statistic (i.e., mean or median) is assigned to lower, middle
#'   or upper tercile; that is, during a defined central expectation window
#'   (e.g., mean +- 95% CI, which may shift linearly with time). We expect one
#'   third of means to be recorded in each tercile and we define three
#'   anomalies: early, late, average. The user can specify if anomalies are
#'   calculated using the mean-tercile method (described above), or a variation
#'   of this method, the median-tercile, or based on bulk movements of 33.3% and
#'   66.6% percentiles (as described in tercile_plot_phenology()). The purpose
#'   of this plot is to illustrate to the user that arbitrary selection of
#'   summary statistics to infer long term trends in phenology can be misleading
#'   in cases where phenology curves are not symmetrical (e.g., in diadromous
#'   fish count time series).
#'
#' @param bio_year A vector of biological years concurrent with daily counts.
#' @param counts A vector of daily counts.
#' @param yday A vector of biologically relevant year days.
#' @param method A character string, specifying the method for calculating
#'   anomalies (i.e., "mean","median",or "33_66"). Default "mean".
#' @param species_name A character string.
#' @param season A character string.
#' @param dates A vector of class "Date".
#' @param unreliable_years A concatenated list of unreliable years (e.g., years
#'   during which floods led to unreliable silver eel counts)
#' @param plot Boolean indicating if plot should be returned (e.g., in addition
#'   to returned data).
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
#' @param plot_005_995_perc Boolean to determine if all data or 0.5th to 99.5th
#'   percentile of non-zero counts are plotted. Default is TRUE. If FALSE, plots
#'   all data.
#' @param xlims_plot A concatenated vector defining x axis limits in terms of
#'   count percentiles.
#' @param ... Additional arguments to nested functions.
#' @return A 9 X no. years summary table, containing summary statistics for
#'   ecological time series count data; namely: mean, median, and 5th, 33rd,
#'   50th, 66th, 95th percentile biological year days.
#' @examples
#' \dontrun{
#'data_counts <- readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'   ))
#'data_counts_subset <- data_counts[data_counts$salmonid_year %in% c(1981:2018),]
#'test <- ridgeline_plot_phenology(bio_year = data_counts_subset$salmonid_year,
#'                          counts = data_counts_subset$ssmolt,
#'                          yday = data_counts_subset$salmonid_yday,
#'                          dates = data_counts_subset$date,
#'                          method = "mean",
#'                          species_name = "Atlantic salmon (smolt)",
#'                          season = "Spring",
#'                          plot = TRUE,
#'                          detrended = TRUE)
#'}
#' @export
ridgeline_plot_phenology <- function(bio_year,
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
                              plot_005_995_perc = TRUE,
                              xlims_plot = NULL,
                              ...) {

  # calculate percentage of run counts vector
  # df
  df <- data.frame("dates" = dates, "counts" = counts, "yday" = yday, "bio_year" = bio_year)

  props <-  lapply(unique(df$bio_year), function(x){
    result <- 100*(df$counts[df$bio_year == x]/sum(df$counts[df$bio_year == x]))
    return(result)
  })
  df$prop_counts <- do.call("c",props)

  # reformat data into bioydays to calculate means, medians, percentiles etc.,
  # load in the data...

  # calculate median year day for all years and plot (if required uncomment)
  summary_stat_list <- lapply(
    unique(bio_year),
    FUN = function(x) {

      expand <- rep(x = yday[bio_year == x], times = counts[bio_year == x])
      #median_yday <- median(expand)
      mean_yday <- (mean(expand))
      percentiles <-
        quantile(x = expand,
                 probs = c(0.05, 1/3, 0.50, 2/3, 0.95))
      perc_5 <- (percentiles[1])
      perc_33 <- (percentiles[2])
      perc_50 <- (percentiles[3])
      perc_66 <- (percentiles[4])
      perc_95 <- (percentiles[5])
      num_counts <- sum(counts[bio_year == x])
      #  abline(v=median_yday, lty = 3, col = "red")
      result = c(x, mean_yday, perc_5, perc_33, perc_50, perc_66, perc_95,num_counts)
      names(result) <-
        c(
          "year",
          "mean_yday",
          "5th_perc",
          "33rd_perc",
          "50th_perc",
          "66th_perc",
          "95th_perc",
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

    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$mean_yday,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$mean_yday < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$mean_yday >= middle_tercile_bondaries[[1]] &
            summary_stats$mean_yday < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )

    summary_stats$anom_col <-
      ifelse(
        summary_stats$mean_yday < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$mean_yday >= middle_tercile_bondaries[[1]] &
            summary_stats$mean_yday < middle_tercile_bondaries[[2]],
          "black",
          "red"
        )
      )
  }

  if (method == "mean" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)

    #lm
    mod <- lm(summary_stats$mean_yday ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$mean_yday)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$mean_yday,
                               g = 3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended

      summary_stats$anom <-
        ifelse(
          summary_stats$mean_yday < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$mean_yday >= mean_detrended_resids_lower_lim &
              summary_stats$mean_yday < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$mean_yday < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$mean_yday >= mean_detrended_resids_lower_lim &
              summary_stats$mean_yday < mean_detrended_resids_upper_lim,
            "black",
            "red"
          )
        )
      # this needs to be a list of dfs to be compatible with plotting historic context

      middle_tercile_bondaries_table <-
        cbind(round(mean_detrended_resids_lower_lim),
              round(mean_detrended_resids_upper_lim))

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

      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$mean_yday,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$mean_yday < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$mean_yday >= middle_tercile_bondaries[[1]] &
              summary_stats$mean_yday < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$mean_yday < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$mean_yday >= middle_tercile_bondaries[[1]] &
              summary_stats$mean_yday < middle_tercile_bondaries[[2]],
            "black",
            "red"
          )
        )
    }

  }

  if (method == "median" & detrended == FALSE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)
    middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`50th_perc`,
                                                                            g=3,
                                                                            onlycuts = TRUE))[2:3],
                                                     ncol = 2,
                                                     dimnames = list("mean",c("33.3%","66.6%"))))

    summary_stats$anom <-
      ifelse(
        summary_stats$`50th_perc` < middle_tercile_bondaries[[1]],
        "early",
        ifelse(
          summary_stats$`50th_perc` >= middle_tercile_bondaries[[1]] &
            summary_stats$`50th_perc` < middle_tercile_bondaries[[2]],
          "average",
          "late"
        )
      )
    summary_stats$anom_col <-
      ifelse(
        summary_stats$`50th_perc` < middle_tercile_bondaries[[1]],
        "blue",
        ifelse(
          summary_stats$`50th_perc` >= middle_tercile_bondaries[[1]] &
            summary_stats$`50th_perc` < middle_tercile_bondaries[[2]],
          "black",
          "red"
        )
      )
  }

  if (method == "median" & detrended == TRUE) {
    # add colour based on category of anomaly based on terciles (detrended or not...)
    #lm
    mod <- lm(summary_stats$`50th_perc` ~ summary_stats$year)
    # is trend significant?
    pval <- broom::glance(mod)[["p.value"]]

    if (pval < 0.05) {
      mean_detrended <-
        (coef(mod)[1] + coef(mod)[2] * summary_stats$year) - mean(summary_stats$`50th_perc`)

      middle_tercile_bondaries_ave <-
        as.numeric(Hmisc::cut2(summary_stats$`50th_perc`,
                               g = 3,
                               onlycuts = TRUE))[2:3]
      mean_detrended_resids_lower_lim <-
        middle_tercile_bondaries_ave[1] + mean_detrended
      mean_detrended_resids_upper_lim <-
        middle_tercile_bondaries_ave[2] + mean_detrended



      summary_stats$anom <-
        ifelse(
          summary_stats$`50th_perc` < mean_detrended_resids_lower_lim,
          "early",
          ifelse(
            summary_stats$`50th_perc` >= mean_detrended_resids_lower_lim &
              summary_stats$`50th_perc` < mean_detrended_resids_upper_lim,
            "average",
            "late"
          )
        )

      summary_stats$anom_col <-
        ifelse(
          summary_stats$`50th_perc` < mean_detrended_resids_lower_lim,
          "blue",
          ifelse(
            summary_stats$`50th_perc` >= mean_detrended_resids_lower_lim &
              summary_stats$`50th_perc` < mean_detrended_resids_upper_lim,
            "black",
            "red"
          )
        )
      middle_tercile_bondaries_table <-
        cbind(round(mean_detrended_resids_lower_lim),
              round(mean_detrended_resids_upper_lim))

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
      middle_tercile_bondaries <- as.data.frame(matrix(as.numeric(Hmisc::cut2(summary_stats$`50th_perc`,
                                                                              g=3,
                                                                              onlycuts = TRUE))[2:3],
                                                       ncol = 2,
                                                       dimnames = list("mean",c("33.3%","66.6%"))))

      summary_stats$anom <-
        ifelse(
          summary_stats$`50th_perc` < middle_tercile_bondaries[[1]],
          "early",
          ifelse(
            summary_stats$`50th_perc` >= middle_tercile_bondaries[[1]] &
              summary_stats$`50th_perc` < middle_tercile_bondaries[[2]],
            "average",
            "late"
          )
        )
      summary_stats$anom_col <-
        ifelse(
          summary_stats$`50th_perc` < middle_tercile_bondaries[[1]],
          "blue",
          ifelse(
            summary_stats$`50th_perc` >= middle_tercile_bondaries[[1]] &
              summary_stats$`50th_perc` < middle_tercile_bondaries[[2]],
            "black",
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
      terciles = c(0.333, 0.666),
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
        rep("black", times = length(phenology_anomalies$average)),
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
      terciles = c(0.333, 0.666),
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
        rep("black", times = length(phenology_anomalies$average)),
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
#  if(plot == TRUE){

    # set new axis for ridge plots to be looped over for each year... might need to be log scale...
#View(df)
    if(!is.null(unreliable_years)){
      df_unreliable_filt <- df[!(df$bio_year %in% unreliable_years),]
      df <- df_unreliable_filt
      }

    # earliest day of year biofix (to replace manual input of biofix_yday text)
    dataset <- data.frame("dates" = df$dates, "yday" = df$yday)
    earliest_date <- dataset[,"dates"][which.min(dataset[,"yday"])]
    earliest_date_yday <- lubridate::yday(earliest_date)

# set xlims as earliest and latest days during which counts were observed
df_no_zeros <- df[df$counts>=1,]
#xlims_ridge <- c(min(df_no_zeros$yday),max(df_no_zeros$yday))
# set to include 99.9% of data for better use of plotting space
# for each year find percentiles 0.01 and 0.99
#xlims_ridge <- quantile(x = df_no_zeros$yday,probs = c(0.01,0.99))

for_i <- unique(df$bio_year)
# 1 to 99 percentile lims for all years
plot_lims <- lapply(for_i,FUN = function(x){
  res <- quantile(x = df_no_zeros$yday[df_no_zeros$bio_year == x],probs = c(0.025,0.975))
})
plot_lims_tab <- do.call(rbind,plot_lims)
rownames(plot_lims_tab) = for_i

if(!is.null(xlims_plot)){
  xlims_ridge = round(quantile(x = df_no_zeros$yday,probs = c(xlims_plot[1],xlims_plot[2])))
}

if(is.null(xlims_plot)){
# limits based on showing minimum breadth in EACH year
#xlims_ridge <-c(min(plot_lims_tab[,1]),max(plot_lims_tab[,2]))

# ALL data version...
#xlims_ridge <- round(quantile(x = df_no_zeros$yday,probs = c(0.01,0.975)))
xlims_ridge <- round(quantile(x = df_no_zeros$yday,probs = c(0,1)))
}

if(plot == TRUE){

#xlims_ridge <- c(75,250)
par(mfrow = c(1,1), oma = c(20.5,12,0,12), mar = c(8.5,4.5,3,4.5), font.lab = 2, font.axis = 1)
#dev.new()
    plot(
      df$counts,
      df$prop_counts,
      cex = 1,
      xlim = xlims_ridge,
      col = NULL,
      ylab = "",
      ylim = c(0,250),
      xlab = "",
      bty = "n",
      axes = FALSE,
      cex.lab = 5,
    )

    # bio year day axis
    # approx date axis Salmonids
    if(species_name %in% c("Atlantic salmon (smolt)","Sea trout (smolt)","Anadromous brown trout (smolt)")){
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = seq(from = min(xlims_ridge),
                                                     to = max(xlims_ridge),
                                                     by = 10),
           labels = round(seq(from = min(xlims_ridge),
                              to = max(xlims_ridge),by = 10)),line = -0.5,tck = 0.02, cex.axis = 5)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = seq(from = min(xlims_ridge),
                                                     to = max(xlims_ridge),
                                                     by = 5),
           labels = NA,line = -0.5,tck = 0.02)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = seq(from = min(xlims_ridge),
                                                     to = max(xlims_ridge),
                                                     by = 1),
           labels = NA,line = -0.5,tck = 0.01)

      mtext(text = "Days after winter solstice",side = 1,line = 7.0,font = 2, cex = 5, xpd = NA)

      axis(side = 1,lwd = 5,at = c(seq(from = xlims_ridge[1], to = xlims_ridge[2], by = 10)),
           labels = c(format(as.Date(round(seq(from = xlims_ridge[1]-10, to = xlims_ridge[2]-10, by = 10)),
                                     origin = "2019-01-01"),format = "%b-%d")),
           line = 10.5,tck = -0.02, cex.axis = 5,mgp = c(6,6,0))
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = c(seq(from = xlims_ridge[1], to = xlims_ridge[2], by = 5)),
           labels = NA,
           line = 10.5,tck = -0.02, cex.axis = 1)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = c(seq(from = xlims_ridge[1], to = xlims_ridge[2], by = 1)),
           labels = NA,
           line = 10.5,tck = -0.01, cex.axis = 1)
      mtext(text = "Approximate calendar date\n(+-2 adjusting for solstice date and leap years)",side = 1,line = 26,font = 2, cex = 5, xpd = NA)


    }

    # approx date axis Eels
    if(species_name %in% c("European eel (silver)")){

      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = seq(from = min(xlims_ridge),
                                                     to = max(xlims_ridge),
                                                     by = 10),
           labels = round(seq(from = min(xlims_ridge),
                              to = max(xlims_ridge),by = 10)),line = -0.5,tck = 0.02, cex.axis = 5)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = seq(from = min(xlims_ridge),
                                                     to = max(xlims_ridge),
                                                     by = 5),
           labels = NA,line = -0.5,tck = 0.02)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = seq(from = min(xlims_ridge),
                                                     to = max(xlims_ridge),
                                                     by = 1),
           labels = NA,line = -0.5,tck = 0.01)
      mtext(text = paste0("Days after calendar year day ", earliest_date_yday),side = 1,line = 7.0,font = 2, cex = 5, xpd = NA)

      # plot x axes####
      #axis(side = 1,at = round(c(seq(from = xlims_ridge[1], to = xlims_ridge[2], length.out = 8))),labels = round(c(seq(from = xlims_ridge[1], to = xlims_ridge[2], length.out = 8))),line = 0,tck = 0.02, cex.axis = 2.25)

      # cal_date_earliest_count
      axis(side = 1,mgp = c(6,6,0), lwd = 5,at = c(seq(from = xlims_ridge[1], to = xlims_ridge[2], by = 10)),labels = c(format(seq(from = earliest_date+xlims_ridge[1], to = earliest_date+xlims_ridge[2], by = 10),format = "%b-%d")),line = 10.5,tck = -0.02, cex.axis = 5, xpd = NA)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = c(seq(from = xlims_ridge[1], to = xlims_ridge[2], by = 5)),
           labels = NA,
           line = 10.5,tck = -0.02, cex.axis = 1)
      axis(side = 1, mgp = c(7,3,0),lwd = 5,at = c(seq(from = xlims_ridge[1], to = xlims_ridge[2], by = 1)),
           labels = NA,
           line = 10.5,tck = -0.01, cex.axis = 1)
      mtext(text = "Approximate calendar date\n(i.e., +-1 adjusting for leap years)",side = 1,line = 26,font = 2, cex = 5, xpd = NA)

    }

    # number of years to plot
    no_plot_years <- length(unique(bio_year)) + length(unreliable_years)

    # # for 49 years
    side2_ticks <- (no_plot_years/5)*25
    # seq(from = 0,
    #     to = 245,
    #     by = 25)
    #
    # # for 26 years
    #(27/5)*25
    #seq(from = 0,
    #         to = 135,
    #         by = 25)

    axis(side = 2,mgp = c(7,1.5,0),lwd = 5,
         at = seq(from = 0,
                  to = side2_ticks,
                  by = 25),
         labels = seq(from = max(unique(bio_year)),
                      by = -5,
                      length.out = length(seq(from = 0,
                                              to = side2_ticks,
                                              by = 25))),
         las = 2, tck = 0.02, cex.axis = 5)
    axis(side = 2, lwd = 5,at = seq(from = 0,
                           to = side2_ticks,
                           by = 5),
         labels = NA,las = 2, tck = 0.02)
    mtext(text = "Year",side = 2,line = 12,font = 2, cex = 5, xpd = NA)

    # add axis showing y scale for individual years
    axis(side = 4,mgp = c(7,1.5,0),lwd = 5,at = seq(from = 0,
                                                    to = 50,
                                                    by = 10),
         labels = seq(from = 0,
                      to = 50,
                      by = 10),las = 2, tck = 0.02, cex.axis = 5)

    axis(side = 4,mgp = c(7,1.5,0),lwd = 5,at = seq(from = 0,
                                                    to = 50,
                                                    by = 1),
         labels = NA,las = 2, tck = 0.01, cex.axis = 5)
    mtext(text = "% run",side = 4,line = 12,font = 2, cex = 5, xpd = NA,adj = 0.1)


    # line and polygon
    lapp_x <- unique(df$bio_year)
    no_unreliable_years <- length(unreliable_years)
    #x = lapp_x[1]
    lin_poly <-  sapply(lapp_x, function(x){

      # for each year, find earliest and latest count and then filter data subset between these dates
      df_x <- df[df$bio_year == x,]
      df_x_min_max_no_zero_yday <- c(min(df_x$yday[df_x$counts >= 1]),max(df_x$yday[df_x$counts >= 1]))
      # subset
      df_x_subset <- df_x[data.table::between(x = df_x$yday,lower = df_x_min_max_no_zero_yday[1],upper = df_x_min_max_no_zero_yday[2]),]
      # if zero count, insert NAs for prop count more intuitive lines plotted in ridge plot

      # "s" line type polygon
      xvals = rep(df_x_subset$yday[df_x_subset$bio_year == x], each = 2)
      #xvals = df_x_subset$yday[df_x_subset$bio_year == x]
      #yvals = df_x_subset$prop_counts[df_x_subset$bio_year == x]
      yval = (side2_ticks)+ df_x_subset$prop_counts[df_x_subset$bio_year == x] - 5*(x - (min(unique(df$bio_year))-1))
      yvals_stagger = c(yval[1],rep(yval, each = 2))
      yvals = yvals_stagger[-length(yvals_stagger)]
      # effective zero
      eff_zero = (side2_ticks) - 5*(x - (min(unique(df$bio_year))-1))
      col_anom <- summary_stats$anom_col[which(summary_stats$year == x)]
      rgb_col1 <- col2rgb(summary_stats$anom_col[which(summary_stats$year == x)])
      rgb_col1_transp <- rgb(red = rgb_col1[1], green = rgb_col1[2], blue = rgb_col1[3], alpha = 80, maxColorValue = 255)

      # polygon
      polygon(x = c(xvals, rev(xvals)),
              y = c(yvals,rev(rep(x = eff_zero,times = length(yvals)))),
              col=rgb_col1_transp,lty = 0)

      # lines to plot only if non_zero

      df_x_subset$prop_counts[1] <- ifelse(df_x_subset$counts[1] == 0 & df_x_subset$counts[2] == 0, NA, df_x_subset$prop_counts[1])

      for(j in 2:(length(df_x_subset$counts)-1)){
        df_x_subset$prop_counts[j] <- ifelse(df_x_subset$counts[j] == 0 & df_x_subset$counts[j-1] == 0 & df_x_subset$counts[j+1] == 0, NA, df_x_subset$prop_counts[j])
      }

      df_x_subset$prop_counts[length(df_x_subset$counts)] <- ifelse(df_x_subset$counts[length(df_x_subset$counts)] == 0 & df_x_subset$counts[length(df_x_subset$counts)-1] == 0, NA, df_x_subset$prop_counts[length(df_x_subset$counts)])

      lines(df_x_subset$yday[df_x_subset$bio_year == x],
            (side2_ticks)+ df_x_subset$prop_counts[df_x_subset$bio_year == x] - 5*(x - (min(unique(df$bio_year))-1)),
            lwd = 5,
            cex = 0.5,
            col = col_anom,
            type= "s",lend = 0)

      })

  }

  return(list(summary_stats = summary_stats,
              mean_terciles = (middle_tercile_bondaries),
              earliest_date_yday = earliest_date_yday))

}
