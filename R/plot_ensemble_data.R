#' Plot time series of ensemble model chain predictions with observations
#' overlay
#'
#' @description This function is experimental. The function takes ensemble
#'   seasonal forecast predictions/simulations (e.g., 25 member predictions for
#'   daily fish counts; these are contained in the argument which is the
#'   returned result of fishcastr::compile_seasonal_forecast_glmm). The
#'   multi-panel plots returned by this figure are experimental, but are
#'   intended to inform model checking. The function also returns a table
#'   containing predictions for each member of the forecast for all forecast
#'   years. The plots returned show 95% percentile prediction intervals and
#'   ensemble medians for the variable of interest.
#'
#' @importFrom utils write.csv
#' @param seasonal_ensemble A seasonal forecast list object returned by
#'   fishcastr::compile_seasonal_forecast_glmm.
#' @param pseudo_obs_ensemble A seasonal forecast list object returned by
#'   fishcastr::compile_seasonal_forecast_glmm.
#' @param var_interest A character string (e.g., "forecast_sim_counts").
#' @param obs_count_name A character string (e.g., "ssmolt").
#' @param bio_yday A character string (e.g., "salmonid_yday").
#' @param bio_year A character string (e.g., "salmonid_year").
#' @param colour_theme A character string (e.g., "green3").
#' @param ensemble_name A character string (must be "ERA5" or "SEAS5", while
#'   experimental).
#' @param target_season An integer concatenated (e.g., 3:7 for March to July).
#' @param lead_month An integer (e.g., 1 for January).
#' @param export_to_file Boolean. Export all member predictions in addition to
#'   multi-panel plot?
#' @param ensemble.lty A character (e.g., "s" for daily counts, "l" for
#'   continuous variables).
#' @param plot_climatology Experimental.
#' @param ylabel A character string (common y variable name e.g., "fish
#'   counts").
#' @param export_file_folder A file path character string.
#' @param multi_panel_plot Boolean. plot or not.
#' @param target_season_only Boolean. TRUE Crops plot to show only target season
#'   predictions.
#' @param fill_obs_counts Boolean. Fills step style polygons for fish counts
#'   grey.
#' @return A data.frame containing all member predictions for all re-forecast
#'   years is exported to a csv. A multi-panel plot is exported to a png.
#' @examples
#' \dontrun{
#' # ERA5 salmon (note code in the salmon workflow vignette must be run to obtain
#' # this rds object)
#' seasonal_forecast_list_dfs_ssmolt_daily_stoc_era5 <-
#'   readRDS(file = paste0(
#'     system.file("vignettes"),
#'     "/vignette_data\\SF_salmon_ideal_loo_cv_23456.rds"
#'   ))
#'
#' # add 2019 counts to forecast list object
#' fishcastr::download_fish_data()
#' data_ssmolt <- fishcastr::import_fish_data(species = "ssmolt")
#'
#' seasonal_forecast_list_dfs_ssmolt_daily_stoc_era5[[39]] <-
#'   lapply(seasonal_forecast_list_dfs_ssmolt_daily_stoc_era5[[39]], function(x) {
#'     NA_dates_2019 <- x$date[which(is.na(x$ssmolt))]
#'     x$ssmolt[x$date %in% NA_dates_2019] <-
#'       data_ssmolt$ssmolt[data_ssmolt$date %in% NA_dates_2019]
#'     return(x)
#'   })
#'
#' fish_counts_ERA5 <-
#'   plot_ensemble_data(
#'     seasonal_ensemble = seasonal_forecast_list_dfs_ssmolt_daily_stoc_era5,
#'     pseudo_obs_ensemble = seasonal_forecast_list_dfs_ssmolt_daily_stoc_era5,
#'     var_interest = "forecast_sim_counts",
#'     obs_count_name = "ssmolt",
#'     bio_yday = "salmonid_yday",
#'     bio_year = "salmonid_year",
#'     colour_theme = "green3",
#'     ensemble_name = "ERA5",
#'     target_season = 3:7,
#'     lead_month = 1,
#'     export_to_file = TRUE,
#'     ensemble.lty = "s",
#'     plot_climatology = FALSE,
#'     ylabel = expression("Fish count"),
#'     export_file_folder = paste0(
#'       system.file("extdata", package = "fishcastr"),
#'       "/eel_wflow_figs/"
#'     ),
#'     multi_panel_plot = TRUE,
#'     target_season_only = TRUE,
#'     fill_obs_counts = TRUE
#'   )
#' }
#' @export
plot_ensemble_data <- function(seasonal_ensemble,
                               pseudo_obs_ensemble,
                               var_interest,
                               obs_count_name,
                               bio_yday,
                               bio_year,
                               colour_theme,
                               ensemble_name,
                               target_season,
                               lead_month,
                               export_to_file = FALSE,
                               ensemble.lty = "l",
                               plot_climatology = TRUE,
                               ylabel,
                               export_file_folder,
                               multi_panel_plot = FALSE,
                               target_season_only = FALSE,
                               fill_obs_counts = FALSE){

  # --------------------------------------------------------------------------------------- #
  # PROCESS SEAS5 ----
  # --------------------------------------------------------------------------------------- #

  # # add count proportions to seasonal ensemble object ----
  # forecast_list_object <-
  #   lapply(seasonal_ensemble, function(x) {
  #     sub_list <- x[(1:(length(x)))]
  #     result_outer <- lapply(sub_list, function(y) {
  #       y[[paste0(obs_count_name, "_run_prop")]] <-
  #         100 * (y[[obs_count_name]] / sum(y[[obs_count_name]]))
  #       y[[paste0("lwr_ci", "_run_prop")]] <-
  #         100 * (y[["lwr_ci"]] / sum(y[[obs_count_name]]))
  #       y[[paste0("upr_ci", "_run_prop")]] <-
  #         100 * (y[["upr_ci"]] / sum(y[[obs_count_name]]))
  #       y[[paste0(obs_count_name, "_run_prop")]] <-
  #         100 * (y[[obs_count_name]] / sum(y[[obs_count_name]]))
  #
  #       result_inner <- y
  #       return(result_inner)
  #     })
  #     return(result_outer)
  #   })

  # extract all environmental data into single table per list element year ----
  res_SEAS5 <- lapply(X = seasonal_ensemble,FUN = function(x){

    lapply(X = names(x),FUN = function(y){
      result <- data.frame("date" = x[[y]][["date"]],
                           bio_year = x[[y]][[bio_year]],
                           bio_yday = x[[y]][[bio_yday]],
                           var_interest = x[[y]][[var_interest]])
      colnames(result)[4] <- var_interest
      return(result)
    })

  })

  res2_SEAS5 <- lapply(X = res_SEAS5,FUN = function(z){
    result <- do.call(cbind,z)
  })

  # keep just one column of ydays and dates ----
  res_sub_SEAS5 <- lapply(X = res2_SEAS5,FUN = function(p){
    result <- p[,c(1:3,seq(from = 4, to = ncol(p), by = 4))]
  })


  # ------------------------------------------------ #
  # subset to remove all data beyond target season
  res_sub_SEAS5.sub <- lapply(X = res_sub_SEAS5,FUN = function(x){
    p_year <- unique(x$bio_year) #  should be only one unique bio year if all else correct
    #  lwr_bnd <- formatC(target_season[1], width=2, flag="0")
    upr_bnd <- formatC(target_season[length(target_season)], width=2, flag="0")
    if(max(target_season)<12){
      upr_bnd_dy <- lubridate::day(as.Date(paste0(p_year,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)
    }
    if(max(target_season) == 12){
      upr_bnd_dy <- lubridate::day(as.Date(paste0(p_year+1,"-",formatC(1,width=2, flag="0"),"-01"))-1)
    }
    result <- x[x$date <= as.Date(paste0(p_year,"-",upr_bnd,"-",upr_bnd_dy)),]
  })

  # bind target season data
  #res_sub_SEAS5.sub.rbind <- do.call(rbind,res_sub_SEAS5.sub)
  #colnames(res_sub_SEAS5.sub.rbind)[4:ncol(res_sub_SEAS5.sub.rbind)] <- paste0("member_",1:25)
  # ------------------------------------------------ #

  # bind all SEAS5 data ----
  res_sub.rbind_SEAS5 <- do.call(rbind,res_sub_SEAS5.sub)
  colnames(res_sub.rbind_SEAS5)[4:ncol(res_sub.rbind_SEAS5)] <- paste0("member_",1:25)

  # #################
  # # export SEAS5 forecast to file -----
  if(export_to_file == TRUE){
    #   # if writing to file, round numeric modelled values. Ideally, round to same precision as raw input data to model.
    res_sub.rbind_SEAS5.round <- cbind(res_sub.rbind_SEAS5[,1:3],round(res_sub.rbind_SEAS5[,4:ncol(res_sub.rbind_SEAS5)],digits = 1))
    write.csv(x = res_sub.rbind_SEAS5.round, file = paste0(export_file_folder,var_interest,"_",ensemble_name,".csv"),row.names = FALSE)
  }
  # ################
  # -------------------------------------------------------------------------------------- #
  # Process ERA5 ----
  # --------------------------------------------------------------------------------------- #

  # extract all environmental data into single table per list element year ----
  res_ERA5 <- lapply(X = pseudo_obs_ensemble,FUN = function(x){

    lapply(X = names(x),FUN = function(y){
      result <- data.frame("date" = x[[y]][["date"]],
                           bio_year = x[[y]][[bio_year]],
                           bio_yday = x[[y]][[bio_yday]],
                           obs_count_name = x[[y]][[obs_count_name]])
      colnames(result)[4] <- obs_count_name
      return(result)
    })

  })

  res2_ERA5 <- lapply(X = res_ERA5,FUN = function(z){
    result <- do.call(cbind,z)
  })

  # keep just one column of ydays and dates ----
  res_sub_ERA5 <- lapply(X = res2_ERA5,FUN = function(p){
    result <- p[,c(1:3,4)]
  })


  # ------------------------------------------------ #
  # subset to remove all data beyond target season
  res_sub_ERA5.sub <- lapply(X = res_sub_ERA5,FUN = function(x){
    p_year <- unique(x$bio_year) #  should be only one unique bio year if all else correct
    #  lwr_bnd <- formatC(target_season[1], width=2, flag="0")
    upr_bnd <- formatC(target_season[length(target_season)], width=2, flag="0")

    if(max(target_season)<12){
      upr_bnd_dy <- lubridate::day(as.Date(paste0(p_year,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)
    }
    if(max(target_season) == 12){
      upr_bnd_dy <- lubridate::day(as.Date(paste0(p_year+1,"-",formatC(1,width=2, flag="0"),"-01"))-1)
    }

    #  upr_bnd_dy <- lubridate::day(as.Date(paste0(p_year,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)
    result <- x[x$date <= as.Date(paste0(p_year,"-",upr_bnd,"-",upr_bnd_dy)),]
  })

  # bind target season data
  #res_sub_SEAS5.sub.rbind <- do.call(rbind,res_sub_SEAS5.sub)
  #colnames(res_sub_SEAS5.sub.rbind)[4:ncol(res_sub_SEAS5.sub.rbind)] <- paste0("member_",1:25)
  # ------------------------------------------------ #


  # bind all ERA5 data ----
  res_sub.rbind_ERA5 <- do.call(rbind,res_sub_ERA5.sub)
  #colnames(res_sub.rbind_ERA5)[4:ncol(res_sub.rbind_ERA5)] <- paste0("member_",1:25)

  pseudo_obs <- res_sub.rbind_ERA5

  # extract column 4 (member_1) as all members are identical if model is used to predict (ie.., discharge and water temp models) rather than simulate (fish count model)
  sim_members <- res_sub.rbind_SEAS5

  sim_members_ensemble_stats <- cbind(sim_members[,1:3], t(apply(X = sim_members[,4:ncol(sim_members)],
                                                                 MARGIN = 1,quantile,
                                                                 probs = c(0.025,0.25,0.50,0.75,0.975),
                                                                 na.rm = TRUE)))

  # merge obs
  sim_members_ensemble_stats_obs <- merge(sim_members_ensemble_stats,pseudo_obs[,c("date",obs_count_name)], by = "date")

  # --------------------------------------------------------------------------------------- #
  # plot all simulations for each year ----
  # --------------------------------------------------------------------------------------- #
  #length(unique(sim_members_ensemble_stats_obs$bio_year))
  if(multi_panel_plot == TRUE){
    if(ensemble_name == "ERA5"){
    png(filename = paste0(export_file_folder,ensemble_name,"_",var_interest,".png"), width=2000, height=2800, res=300)
      par(mfcol = c(13,3), oma = c(3,3,0,0), mar = c(1,1,0,0), mgp = c(1,0.1,0))
    }
    if(ensemble_name == "SEAS5"){
    png(filename = paste0(export_file_folder,ensemble_name,"_",var_interest,".png"), width=2000, height=2000, res=300)
      par(mfcol = c(9,3), oma = c(3,3,0,0), mar = c(1,1,0,0), mgp = c(1,0.1,0))
    }

  }
  #par(mfrow = c(1,1), mar = c(2,4,0,0), oma = c(1,1,1,1), mgp = c(2,0.5,0))
  #n_years <- length(unique(res_sub.rbind_SEAS5$bio_year))
  #for(i in unique(res_sub.rbind_SEAS5$bio_year)[c(1,2,n_years-1,n_years)]){
  #i = unique(sim_members_ensemble_stats_obs$bio_year)[c(1)]
  for(i in unique(sim_members_ensemble_stats_obs$bio_year)){

    # prepare climatology data and set plot colours ----
    xvals_clim <- sim_members_ensemble_stats_obs$bio_yday[sim_members_ensemble_stats_obs$bio_year == i]
    yvals_clim_lw <- sim_members_ensemble_stats_obs$`2.5%`[sim_members_ensemble_stats_obs$bio_year == i]
    yvals_clim_up <- sim_members_ensemble_stats_obs$`97.5%`[sim_members_ensemble_stats_obs$bio_year == i]
    yvals_clim_mid_lwr <- sim_members_ensemble_stats_obs$`25%`[sim_members_ensemble_stats_obs$bio_year == i]
    yvals_clim_mid_upr <- sim_members_ensemble_stats_obs$`75%`[sim_members_ensemble_stats_obs$bio_year == i]

    yvals_clim_med <- sim_members_ensemble_stats_obs$`50%`[sim_members_ensemble_stats_obs$bio_year == i]


    col_var_clim <- "black"
    rgb_col1_clim <- col2rgb(col_var_clim)
    rgb_col1_transp_clim <- rgb(red = rgb_col1_clim[1],
                                green = rgb_col1_clim[2],
                                blue = rgb_col1_clim[3],
                                alpha = 20, maxColorValue = 255)

    # set ensemble plot colour ----
    col_var <- colour_theme
    rgb_col1 <- col2rgb(col_var)
    rgb_col1_transp_forecast <-
      rgb(
        red = rgb_col1[1],
        green = rgb_col1[2],
        blue = rgb_col1[3],
        alpha = 60,
        maxColorValue = 255
      )


    #p_year <- unique(x$bio_year) #  should be only one unique bio year if all else correct

    # highlight target seasons
    lwr_bnd <- formatC(target_season[1], width=2, flag="0")
    upr_bnd <- formatC(target_season[length(target_season)], width=2, flag="0")

    if(max(target_season)<12){
      upr_bnd_dy <- lubridate::day(as.Date(paste0(i,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)
    }
    if(max(target_season) == 12){
      upr_bnd_dy <- lubridate::day(as.Date(paste0(i+1,"-",formatC(1,width=2, flag="0"),"-01"))-1)
    }

    #    upr_bnd_dy <- lubridate::day(as.Date(paste0(i,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)
    ld_mnth <- formatC((target_season[1] - lead_month), width=2, flag="0")

    #    upr_bnd <- formatC(target_season[length(target_season)], width=2, flag="0")
    #    upr_bnd_dy <- lubridate::day(as.Date(paste0(i,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)

    #for(i in unique(res_sub.rbind_SEAS5$bio_year)[c(1)]){
    if(multi_panel_plot == FALSE){
      png(filename = paste0(export_file_folder,"Fig2_sf_ens",var_interest,i,".png"), width=2500, height=1400, res=300)
      par(mfrow = c(1,1), mar = c(0.5,0.5,0,0), oma = c(6,4,1,1), mgp = c(2,0.5,0))
    }
    # set plot limits from climatology and forecast data
    ylims_plot <- c(min(sim_members_ensemble_stats_obs[,c(4:ncol(sim_members_ensemble_stats_obs))][sim_members_ensemble_stats_obs$bio_year == i,],na.rm = TRUE),
                    max(sim_members_ensemble_stats_obs[,c(4:ncol(sim_members_ensemble_stats_obs))][sim_members_ensemble_stats_obs$bio_year == i,],na.rm = TRUE))

    # plot climatology backdrop or not
    target_x <- sim_members$bio_yday[which(sim_members$date == as.Date(paste0(i,"-",upr_bnd,"-",upr_bnd_dy)))]
    target_x_lwr <- 0

    if(target_season_only == TRUE){
      target_x_lwr <- sim_members$bio_yday[which(sim_members$date == as.Date(paste0(i,"-",lwr_bnd,"-","01")))]
    }

    if(!is.null(ylabel)){
      ylabels = ylabel
    }
    if(is.null(ylabel)){
      ylabels = var_interest
    }

    plot(xvals_clim,yvals_clim_lw, col = NULL,
         ylim = ylims_plot,xlim = c(target_x_lwr,target_x),tck = 0.03, xlab = "Forecast year day", ylab = ylabels, xaxt="none")

    if(multi_panel_plot == FALSE){
      axis(side = 1,at = seq(from = 0,by = 50, to = target_x),tck = 0.02,
           labels = seq(from = 0,by = 50, to = target_x))
    }

    axis(side = 1,at = seq(from = 0,by = 10, to = target_x),tck = 0.02,labels = NA)

    if(ensemble_name == "ERA5"){

      if(multi_panel_plot == TRUE & i %in% c(1993,2006,2019) & bio_year == "salmonid_year"){
        axis(side = 1,at = seq(from = 0,by = 50, to = target_x),tck = 0.02,
             labels = seq(from = 0,by = 50, to = target_x))
      }

      if(multi_panel_plot == TRUE & i %in% c(1995,2008,2019) & bio_year == "eel_year"){
        axis(side = 1,at = seq(from = 0,by = 50, to = target_x),tck = 0.02,
             labels = seq(from = 0,by = 50, to = target_x))
      }
    }

    if(ensemble_name == "SEAS5"){

      if(multi_panel_plot == TRUE & i %in% c(2001,2010,2019) & bio_year == "salmonid_year"){
        axis(side = 1,at = seq(from = 0,by = 50, to = target_x),tck = 0.02,
             labels = seq(from = 0,by = 50, to = target_x))
      }

      if(multi_panel_plot == TRUE & i %in% c(2001,2010,2019) & bio_year == "eel_year"){
        axis(side = 1,at = seq(from = 0,by = 50, to = target_x),tck = 0.02,
             labels = seq(from = 0,by = 50, to = target_x))
      }
    }

    # -------------------------------------------------------------------------------------- #
    # colour each period of forecast using polygons (highlighting the target season)
    # beginning of lead-time period (end of antecedent)
    ante_x <- sim_members_ensemble_stats_obs$bio_yday[which(sim_members_ensemble_stats_obs$date == as.Date(paste0(i,"-",ld_mnth,"-01")))]
    # beginning of target season (end of lead time)
    lead_x <- sim_members_ensemble_stats_obs$bio_yday[which(sim_members_ensemble_stats_obs$date == as.Date(paste0(i,"-",lwr_bnd,"-01")))]
    # end of target season
    #  target_x <- res_sub.rbind_SEAS5$bio_yday[which(res_sub.rbind_SEAS5$date == as.Date(paste0(i,"-",upr_bnd,"-",upr_bnd_dy)))]

    yvals_all <- c(-20,-20,3000,3000) # excessive limit, but ok for now

    # antecedent
    xvals_ante <- c(0,ante_x,ante_x,0)
    polygon(x = c(xvals_ante),
            y = c(yvals_all),
            col=rgb_col1_transp_clim,lty = 0)

    # lead time
    xvals_lead <- c(0,lead_x,lead_x,0)
    polygon(x = c(xvals_lead),
            y = c(yvals_all),
            col=rgb_col1_transp_clim,lty = 0)
    # # after target season
    # xvals_target <- c(target_x,
    #                   max(res_sub.rbind_SEAS5$bio_yday),
    #                   max(res_sub.rbind_SEAS5$bio_yday),
    #                   target_x)
    # polygon(x = c(xvals_target),
    #         y = c(yvals_all),
    #         col=rgb_col1_transp_clim,lty = 0)
    if(multi_panel_plot == FALSE){
      text(labels = c("Antecedent","Lead-time","Target season"),
           x = c(1,ante_x,lead_x)+5,y = c(ylims_plot[2],ylims_plot[2],ylims_plot[2]),srt = c(90),adj = 1)
    }
    # -------------------------------------------------------------------------------------- #

    title(main = i, line = -2, adj = 0.90)

    # # polygon climatology

    # polygon.step function written by Patrick Breheny
    # https://github.com/pbreheny/breheny/blob/master/R/polygon-step.R
    # and licensed under GPL v3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
    # no modifications here.
    polygon.step <- function(x, y1, y2, border=FALSE, ...) {
      nx <- length(x)
      ny <- length(y1)
      if (length(y2)!=ny) stop("y1 and y2 must be the same length")
      if (nx != (ny+1)) stop("x must be one longer than y")
      xx <- c(x[1], rep(x[-c(1,nx)], rep(2,nx-2)), x[nx])
      xxx <- c(xx, rev(xx))
      yy1 <- rep(y1, rep(2,ny))
      yy2 <- rep(y2, rep(2,ny))
      yyy <- c(yy1, rev(yy2))
      polygon(xxx, yyy, border=border, ...)
    }

    # if(plot_climatology == TRUE){
    # polygon(x = c(xvals_clim, rev(xvals_clim)),
    #         y = c(yvals_clim_up,rev(yvals_clim_lw)),
    #         col=rgb_col1_transp_forecast,lty = 0)
    #
    # polygon(x = c(xvals_clim, rev(xvals_clim)),
    #         y = c(yvals_clim_mid_upr,rev(yvals_clim_mid_lwr)),
    #         col=rgb_col1_transp_forecast,lty = 0)

    # -------------------------------------------------------------------------------- #
    # replace 0 with NA for purpose of line plotting clarity
    # obs counts or other data
    y_valsi <- sim_members_ensemble_stats_obs[[obs_count_name]][sim_members_ensemble_stats_obs$bio_year == i]
    xvalsi <- sim_members_ensemble_stats_obs$bio_yday[sim_members_ensemble_stats_obs$bio_year == i]

    y_vals_non_zeroi <- vector(length = length(y_valsi))
    y_vals_non_zeroi[1] <-
      ifelse(y_valsi[1] == 0 & y_valsi[2] == 0, NA, y_valsi[1])

    for (k in 2:(length(y_valsi) - 1)) {
      y_vals_non_zeroi[k] <-
        ifelse(y_valsi[k] == 0 &
                 y_valsi[k - 1] == 0 & y_valsi[k + 1] == 0, NA, y_valsi[k])

    }
    y_vals_non_zeroi[length(y_valsi)] <-
      ifelse(y_valsi[length(y_valsi)] == 0 &
               y_valsi[length(y_valsi) - 1] == 0, NA, y_valsi[length(y_valsi)])

    if(fill_obs_counts == FALSE){
      lines(xvalsi,
            y_vals_non_zeroi,
            col = "black",
            type = ensemble.lty, lwd = 0.5)
    }

    # fill obs counts
    if(fill_obs_counts == TRUE){
      polygon.step(x = xvalsi,
                   y1 = rep(0,times = (length(y_vals_non_zeroi)-1)),
                   y2 = y_vals_non_zeroi[-length(y_vals_non_zeroi)],
                   col="darkgrey",lty = 0)
    }
    # -------------------------------------------------------------------------------- #

    # -------------------------------------------------------------------------------- #
    # plot CI polygons
    polygon.step(x = xvals_clim,
                 y1 = yvals_clim_lw[-length(yvals_clim_lw)],
                 y2 = yvals_clim_up[-length(yvals_clim_up)],
                 col=rgb_col1_transp_forecast,lty = 0)

    polygon.step(x = xvals_clim,
                 y2 = yvals_clim_mid_upr[-length(yvals_clim_mid_upr)],
                 y1 = yvals_clim_mid_lwr[-length(yvals_clim_mid_lwr)],
                 col=rgb_col1_transp_forecast,lty = 0)

    # replace 0 with NA for purpose of line plotting clarity
    # simulated median
    y_valsi <- yvals_clim_med
    xvalsi <- xvals_clim

    y_vals_non_zeroi <- vector(length = length(y_valsi))
    y_vals_non_zeroi[1] <-
      ifelse(y_valsi[1] == 0 & y_valsi[2] == 0, NA, y_valsi[1])

    for (k in 2:(length(y_valsi) - 1)) {
      y_vals_non_zeroi[k] <-
        ifelse(y_valsi[k] == 0 &
                 y_valsi[k - 1] == 0 & y_valsi[k + 1] == 0, NA, y_valsi[k])

    }
    y_vals_non_zeroi[length(y_valsi)] <-
      ifelse(y_valsi[length(y_valsi)] == 0 &
               y_valsi[length(y_valsi) - 1] == 0, NA, y_valsi[length(y_valsi)])

    lines(xvalsi,
          y_vals_non_zeroi,
          col = col_var,
          type = ensemble.lty, lwd = 0.5)
    # -------------------------------------------------------------------------------- #

    # }

    # lines(xvals_clim,yvals_clim_mid, lwd = 3, col = rgb_col1_transp_clim, type = "l")

    ###########
    # # highlight target seasons
    # lwr_bnd <- formatC(target_season[1], width=2, flag="0")
    # upr_bnd <- formatC(target_season[length(target_season)], width=2, flag="0")
    # upr_bnd_dy <- lubridate::day(as.Date(paste0(i,"-",formatC(target_season[length(target_season)]+1,width=2, flag="0"),"-01"))-1)
    # ld_mnth <- formatC((target_season[1] - lead_month), width=2, flag="0")

    # extract obs
    #obsy <- sim_members_ensemble_stats_obs$ssmolt[sim_members_ensemble_stats_obs$bio_year == i]
    #obsy_na <- ifelse(obsy == 0, NA,obsy)
    #pad

    # plot obs
    #  lines(sim_members_ensemble_stats_obs$bio_yday[sim_members_ensemble_stats_obs$bio_year == i],
    #        sim_members_ensemble_stats_obs$ssmolt[sim_members_ensemble_stats_obs$bio_year == i], type = "s", col = #"black")

    # --------------------------------------------------- #
    # # beginning of lead-time period (end of antecedent)
    # abline(v = res_sub.rbind_SEAS5$bio_yday[which(res_sub.rbind_SEAS5$date == as.Date(paste0(i,"-",ld_mnth,"-01")))],
    #        lty = 2, col = "black")
    # # beginning of target season (end of lead time)
    # abline(v = res_sub.rbind_SEAS5$bio_yday[which(res_sub.rbind_SEAS5$date == as.Date(paste0(i,"-",lwr_bnd,"-01")))],
    #        lty = 2, col = "black")
    # # end of target season
    # abline(v = res_sub.rbind_SEAS5$bio_yday[which(res_sub.rbind_SEAS5$date == as.Date(paste0(i,"-",upr_bnd,"-",upr_bnd_dy)))],
    #        lty = 2, col = "black")
    # --------------------------------------------------- #
    if(multi_panel_plot == FALSE){
      dev.off()
    }
  }
  if(multi_panel_plot == TRUE){
    mtext(text = "Forecast year day",side = 1,line = 1,outer = TRUE,adj = 0.5)
    mtext(text = ylabel,side = 2,line = 1,outer = TRUE,adj = 0.5)
    dev.off()
  }
  # return all simulations and obs or pseudo obs
  return(list("sims" = res_sub.rbind_SEAS5.round,
              "obs" = sim_members_ensemble_stats_obs))
}
