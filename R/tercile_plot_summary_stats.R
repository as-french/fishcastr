#' Generate seasonal forecast tercile plots and summary statistics
#'
#' @description This function combines the data returned by boxplot_phenology()
#'   (i.e., based on observed counts) in addition to a list of returned results
#'   from boxplot_phenology() (i.e., based on predicted counts for multiple
#'   members) , to plot all reforecast predictions and an operational year (the
#'   final year) in a tercile plot. Each row of the tercile plot grid represents
#'   an anomaly type (i.e., early, late, average). The Receiver Operating
#'   Characteristic Skill Score (ROCSS) is presented where there is sufficient
#'   statistical power to facilitate "reasonable" interpretation.
#'
#' @param seasonal_forecast_predictions An n x m matrix containing all member
#'   specific classified anomalies. Number of rows is equal to number of
#'   reforecast years and number of columns is equal to number of members in
#'   ensemble.
#' @param observed_anomalies A list returned by boxplot_phenology().
#' @param species_name A character string to be printed as a title in the
#'   tercile plot. This currently can be: "Atlantic salmon (smolt)", "Anadromous
#'   brown trout (smolt)", or "European (silver) eel".
#' @param season A character string to be printed as a subtitle of the tercile
#'   plot.
#' @param method A character string indicating summary statistic method (i.e.,
#'   "mean" or "median")
#' @param plot Boolean. Plot seasonal forecast terciles along with ROCSS per
#'   anomaly type?
#' @return A list of length two. The first element of the list is a ten column
#'   data.frame of the data used to produce the plot: i.e., year column, year;
#'   three columns containing respective percentage of members that forecast
#'   each phenology anomaly category (average, early, late); a column total
#'   counts, n; the observed anomaly, anom; and colour (col) of each observed
#'   anomaly plotted. The second list element is the ROCSS and boolean
#'   significance of the score at the 0.05 level (default for
#'   rocss_fun_visualizeR).
#' @examples
#' \dontrun{
#' # load predictions in list format
#' seasonal_forecast_list_dfs_ssmolt_daily_stoc <-
#'   readRDS(paste0(
#'     system.file("vignettes", package = "fishcastr"),
#'     "/vignette_data/SF_salmon_ideal_loo_cv.rds"
#'   ))
#'
#' summary_stats = c("mean", "50th", "5th", "25th", "33rd", "66th", "75th", "95th")
#' pred_or_sim = "forecast_sim_counts"
#'
#' dirName <- paste0(system.file("vignettes", package = "fishcastr"),
#'                   "/vignette_figures/Fig5a/")
#' dir.create(dirName, showWarnings = FALSE, mode = "0777")
#'
#' # --------------------------------------------------------------------------------- #
#' # EXTRACT TERCILE CLASSES FOR OBS DATA ----
#' # --------------------------------------------------------------------------------- #
#' seas_fore_df_ssmolt_daily_full_obs <-
#'   do.call(rbind,
#'           lapply(
#'             X = seasonal_forecast_list_dfs_ssmolt_daily_stoc,
#'             FUN = function(x) {
#'               x[[1]]
#'             }
#'           ))
#'
#' seas_fore_df_ssmolt_daily <-
#'   seas_fore_df_ssmolt_daily_full_obs[seas_fore_df_ssmolt_daily_full_obs$salmonid_year <= 2019, ]
#'
#' # add observed 2019 smolts to df
#' data_ssmolt <- fishcastr::data_ssmolt
#' NA_dates_2019 <-
#'   seas_fore_df_ssmolt_daily$date[which(is.na(seas_fore_df_ssmolt_daily$ssmolt))]
#' data_ssmolt$ssmolt[data_ssmolt$date %in% NA_dates_2019]
#' seas_fore_df_ssmolt_daily$ssmolt[seas_fore_df_ssmolt_daily$date %in% NA_dates_2019] <-
#'   data_ssmolt$ssmolt[data_ssmolt$date %in% NA_dates_2019]
#'
#' seas_fore_df_ssmolt_daily_tercile_data <-
#'   fishcastr::boxplot_phenology(
#'     bio_year = seas_fore_df_ssmolt_daily$salmonid_year,
#'     counts = seas_fore_df_ssmolt_daily$ssmolt,
#'     yday = seas_fore_df_ssmolt_daily$salmonid_yday,
#'     dates = seas_fore_df_ssmolt_daily$date,
#'     method = "mean",
#'     species_name = "Atlantic salmon (smolt)",
#'     season = "Spring",
#'     summary_stat_calculation_months = c(3:7),
#'     plot = FALSE,
#'     detrended = FALSE
#'   )
#'
#' obs <- seas_fore_df_ssmolt_daily_tercile_data
#'
#' # --------------------------------------------------------------------------------- #
#' # EXTRACT TERCILE CLASSES FOR FORECAST DATA ----
#' # --------------------------------------------------------------------------------- #
#' seas_list <- plyr::llply(
#'   .data = 1:25,
#'   .fun = function(y) {
#'     #y = 1
#'     seas_fore_df_ssmolt_daily_full <-
#'       do.call(rbind,
#'               lapply(
#'                 X = seasonal_forecast_list_dfs_ssmolt_daily_stoc,
#'                 FUN = function(x) {
#'                   x[[y]]
#'                 }
#'               ))
#'     seas_fore_df_ssmolt_daily <-
#'       seas_fore_df_ssmolt_daily_full[seas_fore_df_ssmolt_daily_full$salmonid_year <= 2019, ]
#'
#'     # 3. use existing function to calculate quantile trends in observed data
#'     seas_fore_df_ssmolt_daily_tercile_data_new <-
#'       fishcastr::boxplot_phenology(
#'         bio_year = seas_fore_df_ssmolt_daily$salmonid_year,
#'         counts = seas_fore_df_ssmolt_daily[[pred_or_sim]],
#'         yday = seas_fore_df_ssmolt_daily$salmonid_yday,
#'         dates = seas_fore_df_ssmolt_daily$date,
#'         method = "mean",
#'         species_name = "Atlantic salmon (smolt)",
#'         season = "Spring",
#'         summary_stat_calculation_months = c(3:7),
#'         plot = FALSE,
#'         detrended = FALSE
#'       )
#'
#'     reforecasts <-
#'       seas_fore_df_ssmolt_daily_tercile_data_new$summary_stats
#'     return(
#'       list(
#'         reforecasts_list = reforecasts,
#'         mean_terciles = seas_fore_df_ssmolt_daily_tercile_data_new$mean_terciles
#'       )
#'     )
#'   }
#' )
#' #
#' names(seas_list) <- 1:25
#'
#' all_sum_stats_seas_list <- lapply(
#'   X = seas_list,
#'   FUN = function(x) {
#'     result <- cbind(x[["reforecasts_list"]][["year"]],
#'                     x[["reforecasts_list"]][["mean"]])
#'     return(result)
#'   }
#' )
#'
#' # bind summ stats together
#' all_sum_stats_seas_list.bind <-
#'   do.call(cbind, all_sum_stats_seas_list)
#' # subset columns to just one year column and 25 members
#' all_sum_stats_seas_list.bind.sub <-
#'   as.data.frame(cbind(
#'     all_sum_stats_seas_list.bind[, 1],
#'     all_sum_stats_seas_list.bind[, seq(
#'       from = 2,
#'       to = ncol(all_sum_stats_seas_list.bind),
#'       by = 2
#'     )]
#'   ))
#' colnames(all_sum_stats_seas_list.bind.sub) <-
#'   c("year", paste0("member_", 1:25))
#'
#' # compute terciles using all member and year summary stats
#' all_member_sum_stats <-
#'   c(as.matrix(all_sum_stats_seas_list.bind.sub[, -1]))
#' tercile_bnds <- as.numeric(Hmisc::cut2(all_member_sum_stats,
#'                                        g = 3,
#'                                        onlycuts = TRUE))[2:3]
#'
#' # assign each member a tercile anomaly
#' tercile_anoms_sf_ensemble <-
#'   cbind(ifelse(
#'     all_sum_stats_seas_list.bind.sub[, -1] < tercile_bnds[1],
#'     "early",
#'     ifelse(
#'       all_sum_stats_seas_list.bind.sub[, -1] >= tercile_bnds[2],
#'       "late",
#'       "average"
#'     )
#'   ))
#'
#' rownames(tercile_anoms_sf_ensemble) <-
#'   all_sum_stats_seas_list.bind.sub[, 1]
#' colnames(tercile_anoms_sf_ensemble) <- 1:25
#'
#' # --------------------------------------------------------------------------------- #
#' # TERCILE PLOT ----
#' # --------------------------------------------------------------------------------- #
#' dirName <- paste0(system.file("vignettes", package = "fishcastr"),
#'                   "/vignette_figures/Fig5a/salmon/")
#' dir.create(dirName, showWarnings = FALSE, mode = "0777")
#'
#' type_of_pred <-
#'   ifelse(pred_or_sim == "forecast_sim_counts", "sim", "pred")
#'
#' png(
#'   file = paste0(dirName, "Fig5a_ideal_", type_of_pred, "_", "mean", ".png"),
#'   width = 2100,
#'   height = 2200,
#'   res = 300
#' )
#' terciles_summary_stat_plot <-
#'   fishcastr::tercile_plot_summary_stats(
#'     seasonal_forecast_predictions = tercile_anoms_sf_ensemble,
#'     observed_anomalies = obs,
#'     species_name = "Atlantic salmon (smolt)",
#'     season = "spring",
#'     method = "mean",
#'     plot = FALSE
#'   )
#' invisible(dev.off())
#' }
#'@export
tercile_plot_summary_stats <- function(seasonal_forecast_predictions,
                                       observed_anomalies,
                                       species_name,
                                       season,
                                       method,
                                       plot = TRUE
){

# --------------------------------------------------------------------------------- #
# SUMMARISE MEMBER FORECASTS INTO TABLE FOR PLOTTING TERCILE PLOT
# summary_reforecast_data <- do.call(cbind,lapply(X = seasonal_forecast_predictions,
#                                                 FUN = function(x){x[[1]][,"anom"]}))
# #seasonal_forecast_predictions[["1"]]$reforecasts_list$year
# rownames(summary_reforecast_data) <- seasonal_forecast_predictions[["1"]]$reforecasts_list$year
# #summary_reforecast_data_tab <- apply(summary_reforecast_data, MARGIN = 1, FUN = table)

summary_reforecast_data_tab <- plyr::alply(.data = seasonal_forecast_predictions,
                                           .margins =  1,
                                           .fun = table,
                                           .dims = TRUE)

# something odd here for eels seasonal forecast 33,66,75 and 95 percentiles - result of apply is a table, whereas is a list for 5th 25th percentiles. No issue at all for salmon and trout, so why does it affect some of tthe eel analysis. I have added a method to "catch" if a table is produced and then convert it into a list... does the job, but there must be something I don't know about apply() or table()... ## UPDATE - apply returns a list or a matrix depending on the distribution of data in the columns of input matrix (now using plyr::alply to sort this as it works the same as apply, but always returns a list - leave old lapply workaround below in comments for now 22_12_2020##)

# if(!is.list(summary_reforecast_data_tab)){
#   test_tab <- lapply(1:ncol(summary_reforecast_data_tab),function(x){summary_reforecast_data_tab[,x]})
#   names(test_tab) <- colnames(summary_reforecast_data_tab)
#   summary_reforecast_data_tab <- test_tab
# }

# check if any elements are missing
summary_reforecast_data_numeric <- lapply(summary_reforecast_data_tab,FUN = function(x){
  # are any columns missing
  if(!any(names(x) %in% c("early"))){
    x["early"] <- 0
  }
  if(!any(names(x) %in% c("late"))){
    x["late"] <- 0
  }
  if(!any(names(x) %in% c("average"))){
    x["average"] <- 0
  }
  # order tables alphabetically
  y <- x[sort(names(x))]
  return(y)
})

summary_reforecast_data_numeric_xtab <- do.call(rbind,summary_reforecast_data_numeric)

# convert to percentages
summary_reforecast_data_numeric_xtab_perc <- as.data.frame(summary_reforecast_data_numeric_xtab/25)

# add year column
summary_reforecast_data_numeric_xtab_perc$year <- rownames(summary_reforecast_data_numeric_xtab_perc)

# add obs and n (match years in obs with years in forecast predictions)
df_obs_anom <- observed_anomalies$summary_stats[, c("year","anom","n")]
summary_reforecast_data_numeric_xtab_perc_merge <- merge(summary_reforecast_data_numeric_xtab_perc,
                                                         df_obs_anom,
                                                         by = "year",
                                                         all = TRUE) # note NA added by all = TRUE if operational year is present...

# add n
#summary_reforecast_data_numeric_xtab_perc$n <- observed_anomalies$summary_stats[, "n"]

#reorder columns
tercile_data <- data.frame("year" = as.integer(summary_reforecast_data_numeric_xtab_perc_merge$year),
                           "early" = 100*summary_reforecast_data_numeric_xtab_perc_merge$early,
                           "average" = 100*summary_reforecast_data_numeric_xtab_perc_merge$average,
                           "late" = 100*summary_reforecast_data_numeric_xtab_perc_merge$late,
                           "anom" = summary_reforecast_data_numeric_xtab_perc_merge$anom,
                           "n" = as.integer(summary_reforecast_data_numeric_xtab_perc_merge$n))

# plot reforecasts in tercile plot format
t.color <- tercileColorRampCustom(10,custom_cols = c("blue","grey","red")) # set haxadecimal #rrggbb colour ramps

t.color[1,] <- c(NA,NA,NA) # make up to X% transparent

# insert missing (unreliable) years in sequence as NAs
all_years <- seq(from = min(tercile_data[,"year"]), to = max(tercile_data[,"year"]), by = 1)
missing_years <- all_years[!match(seq(from = min(tercile_data[,"year"]), to = max(tercile_data[,"year"]), by = 1),
                 tercile_data[,"year"],
                 nomatch=FALSE)]

df_to_merge <- data.frame(matrix(ncol = length(names(tercile_data)), nrow = length(missing_years)))
names(df_to_merge) = names(tercile_data)
df_to_merge$year = missing_years
class(df_to_merge$year) = "integer"
class(df_to_merge$early) = "numeric"
class(df_to_merge$average) = "numeric"
class(df_to_merge$late) = "numeric"
df_to_merge$anom = as.factor(df_to_merge$anom)
levels(df_to_merge$anom) = levels(tercile_data$anom)
class(df_to_merge$n) = "integer"
tercile_data_merge <- merge(tercile_data, df_to_merge,all = TRUE)
tercile_data <- tercile_data_merge
# plot the terciles
x <- tercile_data[,"year"]
y <- c(1.9,2.0,2.1)

# Different colours for each tercile to aid interpretation...
z_early <- matrix(c(tercile_data[,"early"],rep(0, times = nrow(tercile_data)),rep(0, times = nrow(tercile_data))),ncol = 3)
z_average <- matrix(c(rep(0, times = nrow(tercile_data)),tercile_data[,"average"],rep(0, times = nrow(tercile_data))),ncol = 3)
z_late <- matrix(c(rep(0, times = nrow(tercile_data)),rep(0, times = nrow(tercile_data)),tercile_data[,"late"]),ncol = 3)

if(species_name == "Atlantic salmon (smolt)"){
  species_name_title <- "Atlantic salmon (smolt)"
}
if(species_name == "Sea trout (smolt)"){
  species_name_title <- "Anadromous brown trout (smolt)"

}
if(species_name == "European eel (silver)"){
  species_name_title <- "European eel (silver)"

}
# dev.new()
par(mfrow = c(2,1), mar = c(1,4,0,5), oma = c(14,3,14,2))
# overall counts trend (NOTE THAT THE AXIS FOR THE COUNTS MAY NEED TO SHIFT DEPENDING ON NUMBER OF YEARS IN SERIES...) ( + 1.3 for full dataset 1970-2018, 0.4 for reanalaysis 1982 to 2009 - implement this in script to sort this automatically...)
xno = c(27,48)
yno = c(0.4,1.3)
datafrm <- data.frame("yno"= c(0.4,1.3), "xno" = c(27,48))
lin_mod <- stats::lm(yno~xno, data = datafrm)
#no . years in series
xnum = length(tercile_data[,"year"])
dfm2 <- data.frame("xno" = xnum)
yno = stats::predict(lin_mod, newdata = dfm2)

# plot counts -------
plot(tercile_data[,"year"], tercile_data[,"n"], lwd = 5, xlab = "", ylab = "Count", cex.lab = 0.8, type = "h", lend = 1, xaxt = "n",yaxt = "n", xlim = c(min(tercile_data[,"year"])+yno[[1]],max(tercile_data[,"year"])-yno[[1]]), bty = "n", ylim = c(0, max(tercile_data[,"n"],na.rm = TRUE)),xpd = NA)

#axis(side = 2, at = c(0,3000,6000,9000,12000,15000,18000), labels = c(0,3000,6000,9000,12000,15000,18000), cex.axis = 0.7, las = 1)

axis(side = 2, at = round(seq(from = 0, to = max(tercile_data[,"n"],na.rm = TRUE),length.out = 4)), labels = round(seq(from = 0, to = max(tercile_data[,"n"],na.rm = TRUE),length.out = 4)), cex.axis = 0.7, las = 1)

#method = "mean"
title(adj = 0, main = paste(species_name_title," ", "migration phenology anomaly for " ,method,"\n", sep = ""), xpd = NA, line = 1.5, cex.main = 0.7)

# add tercile data ---------
#bio_year = seas_fore_df_ssmolt_daily$salmonid_year
year_range <- range(tercile_data$year)
#mean_terciles <- observed_anomalies$mean_terciles
#earliest_date_yday <- observed_anomalies$earliest_date_yday

###if(detrended == FALSE){
#  mtext(adj = 0, paste("Annual anomalies relative to ",min(year_range)," - ",max(year_range)," average","\n(On average, the  ",method," date of migration occurs between ordinal days ", round(mean_terciles)[[1]]," and ",round(mean_terciles)[[2]],")", sep = ""), xpd = NA, line = 0.4, cex = 0.55, side = 3)
  #mtext(side = 2, paste("Ordinal day\n (relative to day ",earliest_date_yday,")", sep = ""), xpd = NA, line = 5, cex = 0.8)
###}
#par(mar=c(4,4,0,2))
image(x,y,z_early, col =  t.color[,1],xaxt = "n",yaxt = "n", xlab = "", ylab = "", bty = "n", oldstyle = TRUE, zlim = c(0,100))
image(x,y,z_average, col =  t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", add = TRUE, oldstyle = TRUE, zlim = c(0,100))
image(x,y,z_late, col =  t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", add = TRUE, oldstyle = TRUE, zlim = c(0,100))

###if(detrended == TRUE){
  #        mtext(side = 2, paste("Ordinal day\n (relative to day ",earliest_date_yday," DOY)", sep = ""), xpd = NA, line = 5, cex = 0.8)
  #print(mean_terciles[[1]][1,1])
  #    print(mean_terciles)[[length(mean_terciles)]][1]
  #        axis(side = 2, at = y, labels = c(paste("Before ",(mean_terciles)[[1]][1,1], sep = ""), paste("Between\n",(mean_terciles)[[1]][1,1],"and",(mean_terciles)[[1]][1,2]), paste("After ",(mean_terciles)[[1]][1,2], sep = "")), cex.axis = 0.7, las = 1)
  #        axis(side = 4, at = y, labels = c(paste("Before ",(mean_terciles)[[length(mean_terciles)]][1,1], sep = ""), paste("Between\n",(mean_terciles)[[length(mean_terciles)]][1,1],"and",(mean_terciles)[[length(mean_terciles)]][1,2]), paste("After ",(mean_terciles)[[length(mean_terciles)]][1,2], sep = "")), cex.axis = 0.7, las = 1)
  axis(side = 2, at = y, labels = c(paste0("Early"), paste0("Average"), paste0("Late")), cex.axis = 0.7, las = 1)

###}

axis(side = 1, at = c(seq(from = min(tercile_data[,"year"]), to = max(tercile_data[,"year"]), by = 5)), labels = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 5)), cex.axis = 0.7, las = 2)
axis(side = 1, at = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)), labels = c(rep("", length(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)))), cex.axis = 0.7, las = 2)
axis(side = 3, at = c(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)), labels = c(rep("", length(seq(from = min(tercile_data[,"year"]), to =  max(tercile_data[,"year"]), by = 1)))), cex.axis = 0.7, las = 2)


###if(detrended == FALSE){
  #        axis(side = 2, at = y, labels = c(paste("Before ",round(mean_terciles)[[1]], sep = ""), paste("Between\n",round(mean_terciles)[[1]],"and",round(mean_terciles)[[2]]), paste("After ",round(mean_terciles)[[2]], sep = "")), cex.axis = 0.7, las = 1)
#  axis(side = 2, at = y, labels = c(paste0("Early"), paste0("Average"), paste0("Late")), cex.axis = 0.7, las = 1)
###}

# add observations -----

points(tercile_data[,"year"][which(tercile_data[,"anom"] == "early")], rep(1.9, times = length(tercile_data[,"year"][which(tercile_data[,"anom"] == "early")])), col = "blue", lwd = 2, pch = 21, bg = "white")
points(tercile_data[,"year"][which(tercile_data[,"anom"] == "late")], rep(2.1, times = length(tercile_data[,"year"][which(tercile_data[,"anom"] == "late")])), col = "red", lwd = 2, pch = 21, bg = "white")
points(tercile_data[,"year"][which(tercile_data[,"anom"] == "average")], rep(2.0, times = length(tercile_data[,"year"][which(tercile_data[,"anom"] == "average")])), col = "black", lwd = 2, pch = 21, bg = "white")

# Note comparison of tercile breaks between forecasted and observed data indicates bias direction and magnitude. Bias correction of model forecasts would not be simple here...

# calculate and add ROCSS -----
# --------------------------------------------------------------------------------------------------
# POWER ANALYSIS TO EXCLUDE ANLAYSIS OF ANOMALIES WITH INSUFFICIENT SAMPLES FOR ACCEPTABLE POWER
# --------------------------------------------------------------------------------------------------
# rocss <- 2*(auc_r - 0.5) so... Joliffe
rocss <- 0.75
auc_raw <- (rocss/2)+0.5
power_ana <- pROC::power.roc.test(auc = auc_raw,
                                  sig.level = 0.05,
                                  power = 0.90,
                                  kappa = 3)

cases_req <- round(power_ana$ncases)
controls_req <- round(power_ana$ncontrols)
case_control_req <- matrix(nrow = 1, c(cases_req, controls_req))
#print(case_control_req)
colnames(case_control_req) <- c("cases", "controls")
#print(case_control_req)
# NOTE ROCSS MUST BE CALCULATED USING DATA FROM REFORECAST YEARS ALONE (otherwise NA in operational year will cause to fail)
#tercile_plot_data <- tercile_plot_data_full[!is.na(tercile_plot_data_full$anom),]
#tercile_data

# remove operational year row from tercile data (i.e., where obs are NA)
tercile_data_no_na <- tercile_data[!is.na(tercile_data$anom),]

no_early_cases <- sum(ifelse(tercile_data_no_na$anom == "early", 1,0))
no_late_cases <- sum(ifelse(tercile_data_no_na$anom == "late", 1,0))
no_average_cases <- sum(ifelse(tercile_data_no_na$anom == "average", 1,0))
# --------------------------------------------------------------------------------------------------

# need to make sure that at least x event and y non event occurs in result plot, otherwise rocss with 0.80 power to detect significant ROCSS 0.8 cannot be calculated... also make sure that forecast predicts at least x events occurring

if(any(tercile_data_no_na$early != 0) & (any(tercile_data_no_na$anom == "early")) & (any(tercile_data_no_na$anom != "early"))){
  early_ROC <- rocss_fun_visualizeR(pred = tercile_data_no_na$early/100,
                                     obs = ifelse(tercile_data_no_na$anom == "early", 1,0),
                                     conf.level = 0.95)
}

if(any(tercile_data_no_na$late != 0) & any(tercile_data_no_na$anom == "late") & (any(tercile_data_no_na$anom != "late"))){
  late_ROC <- rocss_fun_visualizeR(pred = tercile_data_no_na$late/100,
                                    obs = ifelse(tercile_data_no_na$anom == "late", 1,0),
                                    conf.level = 0.95)
}

if(any(tercile_data_no_na$average != 0) & any(tercile_data_no_na$anom == "average") & (any(tercile_data_no_na$anom != "average"))){
  average_ROC <- rocss_fun_visualizeR(pred = tercile_data_no_na$average/100,
                                       obs = ifelse(tercile_data_no_na$anom == "average", 1,0),
                                       conf.level = 0.95)
}

max_year_pos <- max(year_range[2])+3
text(labels = "ROCSS",x = max_year_pos, y = 2.175, xpd = NA, cex = 0.7)

# add rocss labels if they exist...and significance if possible to calculate
if(exists('early_ROC') & no_early_cases >= cases_req & no_early_cases <= controls_req){
  sig <- early_ROC[[2]]
  na_sig <- (is.na(early_ROC[[2]]))
  sig_sign <- ifelse(na_sig == TRUE, "",ifelse(sig == TRUE,"*",""))
  text(labels = paste(round(early_ROC[[1]],2),
                      sig_sign,
                      sep = ""),
       x = max_year_pos,
       y = 1.9, xpd = NA, cex = 0.7)
}
#  print(paste(round(early_ROC[[1]],2),
#              ifelse(early_ROC[[2]] == TRUE,"*",
#                     ifelse(early_ROC[[2]] == FALSE," ",
#                            ifelse(na_sig == TRUE," "," "))),sep = ""))

if(exists('late_ROC') & no_late_cases >= cases_req & no_late_cases <= controls_req){
  sig <- late_ROC[[2]]
  na_sig <- (is.na(late_ROC[[2]]))
  sig_sign <- ifelse(na_sig == TRUE, "",ifelse(sig == TRUE,"*",""))
  text(labels = paste(round(late_ROC[[1]],2),
                      sig_sign,
                      sep = ""),
       x = max_year_pos,
       y = 2.1, xpd = NA, cex = 0.7)
}

if(exists('average_ROC') & no_average_cases >= cases_req & no_average_cases <= controls_req){
  sig <- average_ROC[[2]]
  na_sig <- (is.na(average_ROC[[2]]))
  sig_sign <- ifelse(na_sig == TRUE, "",ifelse(sig == TRUE,"*",""))
  text(labels = paste(round(average_ROC[[1]],2),
                      sig_sign,
                      sep = ""),
       x = max_year_pos,
       y = 2.0, xpd = NA, cex = 0.7)
}

# add legend -----
par(oma=c(7,4,0,0))
fields::image.plot(add = TRUE, x,y,z_early, col = t.color[,3],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.25,0.28), lab.breaks=c(rep("", nrow(t.color)+1)))
fields::image.plot(add = TRUE, x,y,z_average, col = t.color[,2],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.only = TRUE, smallplot = c(0.40,0.85,0.20,0.23), lab.breaks=c(rep("", nrow(t.color)+1)))
fields::image.plot(add = TRUE, x,y,z_late, col = t.color[,1],xaxt = "n", xlab = "", yaxt = "n", ylab = "", bty = "n", horizontal = TRUE, legend.cex = 0.8, legend.lab = "% member predictions assigned per anomaly type",legend.only = TRUE, smallplot = c(0.40,0.85,0.15,0.18), lab.breaks=c(seq(from = 0, to = 100, by = 10)), axis.args=list(cex.axis=0.65),zlim = c(0,100))

legend("bottomleft",
       inset = c(-0.15,-0.10),
       pch = c(1,1,1),
       lty = c(0,0,0),
       lwd = c(2,2,2),
       col = c("blue","black","red"),
       legend = c("Early","Average","Late"),
       cex = c(0.8),
       pt.cex = c(1,1,1),
       bty = "n",
       xpd = NA)

return(list("tercile_data" = tercile_data,
            "ROCSS" = list("early_ROC" = list("score" = round(early_ROC[[1]],2),
                                              "significant_0_05" = early_ROC[[2]]),
                           "average_ROC" = list("score" = round(average_ROC[[1]],2),
                                                "significant_0_05" = average_ROC[[2]]),
                           "late_ROC" = list("score" = round(late_ROC[[1]],2),
                                             "significant_0_05" = late_ROC[[2]]))))
}
