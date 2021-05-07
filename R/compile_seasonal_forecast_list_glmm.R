#'Generate seasonal forecast
#'
#'@description This function prepares environmental data and generates seasonal
#'  forecasts. The function appends reanalysis (lead-in period) and seasonal
#'  forecast ensemble members (forecast period) in a list of data.frames (one
#'  per reforecast and member; i.e., 25 data frames per year for 1979:2019 for
#'  salmon smolt example). In addition, non-climate variables (e.g., moonlight
#'  exposure, potential evapotranspiration, catchment discharge, photoperiod and
#'  "delta" variables) are calculated for each reforecast. Subsequently, each
#'  data.frame is used to generate simulated counts from a fitted
#'  glmmTMB::glmmTMB model object; note, a list of models and scaling parameters
#'  are supplied to the function to ensure "leave-one-year-out" method applies
#'  for validation purposes.
#'
#'@param calibrated_physio_model A list of length 3 dereived from fitted
#'  bbmle::mle2 object, obj, containing list("data" = obj@data, "coefs" =
#'  bbmle::coef(obj), and "vcov_mat" = bbmle::vcov(obj)).
#'@param calibrated_daily_model A list of glmmTMB::glmmTMB models (one per
#'  reforecast).
#'@param predictor_scaling_params A list of caret::preProcess objects (one per
#'  reforecast).
#'@param hydro_GR4J_model_params A 1 X 4 table of four parameters for running
#'  GR4J lumped hydrological rainfall run-off model.
#'@param air_to_water_model_params A 1 X 4 table of four parameters for running
#'  air to water temperature empirical model.
#'@param seasonal_forecast_grid A multi-member grid object (e.g., 25 member
#'  ECMWF SEAS5).
#'@param reanalysis_data The data.frame containing climate reanalysis data up to the
#'  end of antecedent conditions of the operational year (column order must be
#'  date, tas, pr, petH).
#'@param counts_and_antecedent_conditions A data.frame containing historic fish
#'  counts, forecast year days and forecast years and antecedent environmental
#'  conditions.
#'@param species_bio_year_name A charachter string identifying each unique
#'  reforecast period (e.g., the 1979:2019 salmon smolt runs at the Burrishoole
#'  catchment, Ireland)
#'@param species_bio_yday_name A character string identifying the relvant
#'  biological year day; e.g., for salmon, a header that relates to a column
#'  containing days since winter solstice to winter solstice, i.e., sequences of
#'  1:365 or 1:366.
#'@param fish_reaction_time An integer. No. days over which average (linear)
#'  temperature change is perceivable by fish (e.g., European eels, which might
#'  migrate in response to decreasing temperatures).
#'@param initialisation_month An integer. Calendar month number during which
#'  seasonal forecast ensemble is initialised (e.g., Feb, 2, for salmonid
#'  smolts).
#'@param no_forecast_months An integer. Duration of seasonal forecast in months
#'  (e.g., 6 for salmonids - F|MAMJJ).
#'@param operational_year Integer. Operational year.
#'@param forecast_method If "stochastic", generates mvrnorm based simulations,
#'  whereby a single mvrnorm sample is taken (i.e., taking into account glmmTMB
#'  model parameter uncertainty) from which conditional means are calculated.
#'  See population prediction intevals (see B Bolker's Ecological Models and
#'  Data with R.); i.e., uncertainty in conditional mean owing to parameter
#'  uncertainty. If "determinstic", generates simulations using model point
#'  estimates to calculate conditonal mean, and simulates random deviates about
#'  that conditional mean.
#'@param parallel Boolean. Default FALSE. Use parallel processing?
#'@param parallel_seed_no Boolean. Default FALSE. Use parallel processing?
#'@param ... Other arguments to nested functions.
#'@return A list of data frames containing all simulations (i.e., a data.frame
#'  for each reforecast in addition to the operational forecast).
#'@examples
#' \dontrun{
#'   # load physio response model
#'   mod_physio_expmG <- fishcastr::model_physio_expmG_salmon
#'
#'   # load and subset list into model and scaling lists for relevant years only 1993
#'   # - 2019
#'   model_list_1of2 <-
#'     readRDS(paste0(
#'       system.file("vignettes", package = "fishcastr"),
#'       "/vignette_data/mTrain_genpois_salmon_list_1of2.rds"
#'     ))
#'   model_list_2of2 <-
#'     readRDS(paste0(
#'       system.file("vignettes", package = "fishcastr"),
#'       "/vignette_data/mTrain_genpois_salmon_list_2of2.rds"
#'     ))
#'
#'   mTrain_genpois_list <- list()
#'   mTrain_genpois_list[1:length(model_list_1of2)] <- model_list_1of2
#'   mTrain_genpois_list[(1 + length(model_list_1of2)):(length(model_list_1of2) +
#'                                                        length(model_list_2of2))] <- model_list_2of2
#'
#'   # add 2019 data
#'   mTrain_genpois_salmon_2019 <-
#'     model_list_1of2 <-
#'     readRDS(paste0(
#'       system.file("vignettes", package = "fishcastr"),
#'       "/vignette_data/mTrain_genpois_salmon_2019.rds"
#'     ))
#'   pP_params_salmon_2019 <-
#'     model_list_1of2 <-
#'     readRDS(paste0(
#'       system.file("vignettes", package = "fishcastr"),
#'       "/vignette_data/pP_params_salmon_2019.rds"
#'     ))
#'
#'   mTrain_genpois_list[[39]] <- list(mTrain_genpois_salmon_2019,
#'                                     pP_params_salmon_2019)
#'
#'   names(mTrain_genpois_list) <- 1981:2019
#'
#'   # subset to 2016 - 2019 models for testing function
#'   mTrain_genpois_list_sub <-
#'     mTrain_genpois_list[which(names(mTrain_genpois_list) %in% c(2018:2019))]
#'
#'   # model list
#'   mTrain_genpois_list_sub_mod <-
#'     sapply(mTrain_genpois_list_sub, "[", 1)
#'   # scaling param list
#'   mTrain_genpois_list_sub_scPara <-
#'     sapply(mTrain_genpois_list_sub, "[", 2)
#'
#'   # reanalysis data subset to 2019 for (antecedent in conditions must run to end
#'   # of Jan 2019)
#'   data_ERA5_1979_2019_Jan_bc <-
#'     fishcastr::convert_grid_to_dataframe(grid_obj = fishcastr::grid_ERA5_1979_2019_Jan_bc)[, -2]
#'   names(data_ERA5_1979_2019_Jan_bc)[which(names(data_ERA5_1979_2019_Jan_bc) == "dates1")] <-
#'     "date"
#'
#'   data_ssmolt_enviro_1981_2019 <-
#'     readRDS(paste0(
#'       system.file("vignettes", package = "fishcastr"),
#'       "/vignette_data/data_ssmolt_enviro_1981_2019.rds"
#'     ))
#'
#'   # subset the seasonal forecast grid to test using 5 years of data for example
#'   # check
#'   seas_subset <-
#'     lapply(fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc,
#'            function(x) {
#'              transformeR::subsetGrid(x, years = 2018:2019)
#'            })
#'
#'   system.time({
#'     seasonal_forecast_list_dfs_ssmolt_daily_stoc <-
#'       fishcastr::compile_seasonal_forecast_list_glmm(
#'         calibrated_physio_model = mod_physio_expmG,
#'         calibrated_daily_model = mTrain_genpois_list_sub_mod,
#'         predictor_scaling_params = mTrain_genpois_list_sub_scPara,
#'         hydro_GR4J_model_params = fishcastr::GR4J_Burr_params_ERA5_bcc,
#'         air_to_water_model_params = fishcastr::air_to_water_Feeagh_params_ERA5_bcc,
#'         seasonal_forecast_grid = seas_subset,
#'         reanalysis_data = data_ERA5_1979_2019_Jan_bc,
#'         counts_and_antecedent_conditions = data_ssmolt_enviro_1981_2019,
#'         species_bio_year_name = "salmonid_year",
#'         species_bio_yday_name = "salmonid_yday",
#'         fish_reaction_time = 20,
#'         initialisation_month = 2,
#'         no_forecast_months = 6,
#'         forecast_method = "stochastic",
#'         parallel = FALSE,
#'         parallel_seed_no = 23456,
#'         operational_year = 2019
#'       )
#'   })
#'}
#'@export
compile_seasonal_forecast_list_glmm <-
  function(calibrated_physio_model,
           calibrated_daily_model,
           predictor_scaling_params,
           hydro_GR4J_model_params,
           air_to_water_model_params,
           seasonal_forecast_grid,
           reanalysis_data,
           counts_and_antecedent_conditions,
           species_bio_year_name,
           species_bio_yday_name,
           fish_reaction_time,
           initialisation_month,
           no_forecast_months,
           operational_year = NULL,
           forecast_method,
           parallel,
           parallel_seed_no,
           ...) {

    # -------------------------------------------------------------------------------------------------- #
    # extract predictor and response variable names ----
    # -------------------------------------------------------------------------------------------------- #
    daily_mod_pred_names <- names(calibrated_daily_model[[1]]$frame)[-1]
    physio_mod_pred_names <- names(calibrated_physio_model[["data"]])
    response_var_name <- names(calibrated_daily_model[[1]]$frame)[1]

    # -------------------------------------------------------------------------------------------------- #
    # convert seasonal forecast grid to nested list ----
    # -------------------------------------------------------------------------------------------------- #
    seas_forecast_list <- convert_multi_member_grid_to_list(seasonal_forecast_grid)

    # -------------------------------------------------------------------------------------------------- #
    # rename reanalysis column headers ----
    # -------------------------------------------------------------------------------------------------- #
    colnames(reanalysis_data) <- c("date", "tas_rean", "pr_rean", "petH_rean")

    # -------------------------------------------------------------------------------------------------- #
    # extract longest photoperiod from historic data for later scaling ----
    # -------------------------------------------------------------------------------------------------- #
    max_photoper <- max(counts_and_antecedent_conditions$photoper, na.rm = T)

    # -------------------------------------------------------------------------------------------------- #
    # merge reanalysis and historic counts, bioyear bioydays etc ----
    # -------------------------------------------------------------------------------------------------- #

    if(species_bio_year_name == "eel_year"){
      reanalysis_counts_data_bio_year_na <- merge(reanalysis_data, counts_and_antecedent_conditions,
                                               by = "date", all.x = TRUE)

    }
    if(species_bio_year_name == "salmonid_year"){
      reanalysis_counts_data_bio_year_na <- merge(reanalysis_data, counts_and_antecedent_conditions,
                                               by = "date", all.x = TRUE)

    }

    # remove NA bioyears and select only bioyear,bioyday,count columns (i.e.,
    # not catchment predictors, which will be calculated later in function)
    reanalysis_counts_data_bio_year <-
      reanalysis_counts_data_bio_year_na[!is.na(reanalysis_counts_data_bio_year_na[[species_bio_year_name]]),
                                      c(which(
                                        colnames(reanalysis_counts_data_bio_year_na) %in% c(
                                          "date",
                                          "tas_rean",
                                          "pr_rean",
                                          "petH_rean",
                                          species_bio_year_name,
                                          species_bio_yday_name,
                                          response_var_name
                                        )
                                      ))]

    # reanalysis with NA years for use for discharge warm up 1979- 1981
    reanalysis_data_bio_years <- reanalysis_counts_data_bio_year_na[,
                                                                  c(which(
                                                                    colnames(reanalysis_counts_data_bio_year_na) %in% c(
                                                                      "date",
                                                                      "tas_rean",
                                                                      "pr_rean",
                                                                      "petH_rean",
                                                                      species_bio_year_name,
                                                                      species_bio_yday_name,
                                                                      response_var_name
                                                                    )
                                                                  ))]

    reanalysis_data_bio_years$photoper <- fishcastr::photper_calc(dates = reanalysis_data_bio_years$date,
                                                                  latitude = 53.932458,
                                                                  longitude = -9.575556)

    # fill in missing salmonid years
    if(species_bio_year_name == "salmonid_year"){
    salmonid_years_reanalysis <- fishcastr::bio_year_photoper(dates = reanalysis_data_bio_years$date,
                                                              latitude = 53.932458,
                                                              longitude = -9.575556,
                                                              retain_photoper = FALSE,
                                                              start_previous_year = TRUE,
                                                              shortest_day = TRUE,
                                                              yday_head = "salmonid_yday",
                                                              bio_year_head = "salmonid_year")


    reanalysis_data_bio_years$salmonid_year = ifelse(is.na(reanalysis_data_bio_years$salmonid_year),
                                                     salmonid_years_reanalysis$salmonid_year,
                                                     reanalysis_data_bio_years$salmonid_year)
    }

    # fill in missing eel years
    if(species_bio_year_name == "eel_year"){

      # model water temp
      data_water_temp_eel <- data.frame(
        "date" = reanalysis_data_bio_years$date,
        "water_temp" = fishcastr::air_to_water_model(
          Ta = reanalysis_data_bio_years$tas_rean,
          Yday = lubridate::yday(reanalysis_data_bio_years$date),
          A = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$A,
          ac = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$ac,
          b = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$b,
          B = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$B
        )
      )

      eel_years_reanalysis <- fishcastr::bio_year_therm(mean_temp = data_water_temp_eel$water_temp,
                                                            dates = data_water_temp_eel$date,
                                                            biofix_temp = 11,
                                                            min_no_days_above_biofix = 10,
                                                            increasing_temp = TRUE,
                                                            yday_head = "eel_yday",
                                                            bio_year_head = "eel_year",
                                                            days_since_earliest_biofix_head = "eel_yday_biofix",
                                                            incomplete_first_year = 1978,
                                                            start_previous_calendar_year = FALSE)

      # subset relevant data to same years as eel_years_reanalysis
      reanalysis_data_bio_years <- reanalysis_data_bio_years[reanalysis_data_bio_years$date %in% c(eel_years_reanalysis$date),]

      # substitute NAs in eel_year with computed years
      reanalysis_data_bio_years$eel_year = ifelse(is.na(reanalysis_data_bio_years$eel_year),
                                                       eel_years_reanalysis$eel_year,
                                                       reanalysis_data_bio_years$eel_year)
    }

  # -------------------------------------------------------------------------------------------------- #
  # merge antecedent reanalysis conditions to seasonal forecast members within target forecast year ----
  # -------------------------------------------------------------------------------------------------- #
  merged_list_rean <- lapply(seas_forecast_list, function(x){

    sublist <- lapply(x,function(y){
      # identify forecast year
      f_year <- max(lubridate::year(y[["date"]]))

      sub_result <- merge(y,
                         reanalysis_counts_data_bio_year[reanalysis_counts_data_bio_year[[species_bio_year_name]] == f_year,],
                         by = "date",all = TRUE)

      # remove reanalysis values beyond end of target season
      sub_sub_result <- sub_result[sub_result$date <= as.Date(paste0(f_year,
                                                                 "-",
                                                                 initialisation_month+no_forecast_months,"-",
                                                                 lubridate::days_in_month(initialisation_month+no_forecast_months))),]

      # create new columns of appended antecedent and seasonal forecasts to be
      # used to derive predictors
      sub_sub_result$forecast_tas <- ifelse(is.na(sub_sub_result$tas), sub_sub_result$tas_rean,
                                                        sub_sub_result$tas)

      sub_sub_result$forecast_pr <- ifelse(is.na(sub_sub_result$pr), sub_sub_result$pr_rean,
                                                       sub_sub_result$pr)

      sub_sub_result$forecast_petH <- ifelse(is.na(sub_sub_result$petH), sub_sub_result$petH_rean,
                                             sub_sub_result$petH)

      # fill in NA for bio year and bioyear days for operational year
      if(f_year == operational_year){

        # fill in bioyear NAs
        sub_sub_result[[species_bio_year_name]] <- ifelse(is.na(sub_sub_result[[species_bio_year_name]]),
                                                          f_year,
                                                          sub_sub_result[[species_bio_year_name]])

        if(species_bio_year_name == "salmonid_year"){
        # fill in bio yday NAs
        min_NA <- min(which(is.na(sub_sub_result[[species_bio_yday_name]])))
        max_NA <- max(which(is.na(sub_sub_result[[species_bio_yday_name]])))

        if (!all(is.na((sub_sub_result[[species_bio_yday_name]])))) {
          sub_sub_result[[species_bio_yday_name]][min_NA:max_NA] <-
            seq(
              from = max(sub_sub_result[[species_bio_yday_name]],
                         na.rm = TRUE) + 1,
              by = 1,
              length.out = length(sub_sub_result[[species_bio_yday_name]][min_NA:max_NA])
            )
        }
        }

        # relevant to eels for which forecast year starts after first day of
        # antecedent conditions for (most) eel years where all NAs, because many
        # eel years do not started until after 01 July which is eel_yday_biofix
        # day 53 (or 54 in leap years), so if year is leap year sequence from
        # 54, if year is non leap year sequence from 53
        if(species_bio_year_name == "eel_year"){

          if (any(is.na(sub_sub_result[[species_bio_yday_name]]))) {

            # identify equivalent yday biofix for 1st July
            # earliest biofix day is 5th May (if 2011 Burrishoole year is in historic dataset)
            earliest_day_eel <- rownames(fishcastr::earliest_day(dataset = reanalysis_counts_data_bio_year_na,
                                                                 calendar_date = "date",bio_date_day = "eel_yday",
                                                                 start_previous_calendar_year = FALSE))

            min_NA <- min(which(is.na(sub_sub_result[[species_bio_yday_name]])))
            max_NA <-
              max(which(is.na(sub_sub_result[[species_bio_yday_name]])))

            if (lubridate::leap_year(sub_sub_result$date[[1]]) == FALSE) {
              yday_biofix_seq_start <-
                lubridate::yday(as.Date(paste0(
                  lubridate::year(earliest_day_eel),
                  "-07-01"
                ))) - lubridate::yday(as.Date(earliest_day_eel))
              sub_sub_result[[species_bio_yday_name]][min_NA:max_NA] <-
                seq(
                  from = 1 + yday_biofix_seq_start,
                  by = 1,
                  length.out = length(sub_sub_result[[species_bio_yday_name]][min_NA:max_NA])
                )
            }
            if (lubridate::leap_year(sub_sub_result$date[[1]]) == TRUE) {
              yday_biofix_seq_start <-
                1 + lubridate::yday(as.Date(paste0(
                  lubridate::year(earliest_day_eel),
                  "-07-01"
                ))) - lubridate::yday(as.Date(earliest_day_eel))
              sub_sub_result[[species_bio_yday_name]][min_NA:max_NA] <-
                seq(
                  from = 2 + yday_biofix_seq_start,
                  by = 1,
                  length.out = length(sub_sub_result[[species_bio_yday_name]][min_NA:max_NA])
                )
            }
          }
        }

        # lastly, for operational years remove all counts already observed
        # during operational year as these should be added later for comparison
        # with forecasts (perhaps using an additional function called
        # 'post_season_review')
          sub_sub_result[[response_var_name]] <- NA
        }

      return(sub_sub_result)
    })
    return(sublist)
  })

    # -------------------------------------------------------------------------------------------------- #
    # compute catchment predictors from merged reanalysis and forecast data ----
    # -------------------------------------------------------------------------------------------------- #

  if (parallel == TRUE) {
    seed_no <- parallel_seed_no
    n_cores <- parallel::detectCores() -1
    cl <- parallel::makeCluster(n_cores, type = "SOCK")
    parallel::clusterSetRNGStream(cl, iseed = seed_no)
#    parallel::clusterCall(cl, function() {
#    requireNamespace("caret")
#      library(VGAM)
#    library(caret)
#      library(fishcastr)
#    })

    parallel::clusterExport(
      cl,
      varlist = c(
        "merged_list_rean",
        "fish_reaction_time",
        "species_bio_year_name",
        "calibrated_physio_model",
        "max_photoper",
        "reanalysis_data_bio_years",
        "predictor_scaling_params",
        "calibrated_daily_model",
        "forecast_method",
        "response_var_name",
        "air_to_water_model_params"
      ),
      envir = base::environment()
    )

    doSNOW::registerDoSNOW(cl)
    opts <- list(preschedule = TRUE)
  }

  # rm(test_list) test_list <- llply(merged_list_rean[24:24], function(x){
  test_list <- plyr::llply(.data = merged_list_rean, .fun = function(x) {

#    sub_x <- x[(1:(length(x) - 1))]
    # y = sub_x[[1]][[1]]
    sub_list <- plyr::llply(.data = x, .fun = function(y, q = NULL, z = NULL) {

      # -------------------------------------------------------------------------------------------------- #
      # change in day length ----
      Delta_Photo <- fishcastr::delta_var(y = fishcastr::photper_calc(y$date,
                                                                      latitude = 53.932458,
                                                                      longitude = -9.575556),
                                          react_time = 1)

      # -------------------------------------------------------------------------------------------------- #
      # moonlight exposure ----
      moonlight_exposure <- fishcastr::lunar_light_exposure(date_of_interest = y$date,
                                                            lat = 53.932458,
                                                            lon = -9.575556,
                                                            log_result = FALSE)

      ln_moonlight_exposure <- fishcastr::lunar_light_exposure(date_of_interest = y$date,
                                                               lat = 53.932458,
                                                               lon = -9.575556,
                                                               log_result = TRUE)

      # -------------------------------------------------------------------------------------------------- #
      # calculate variables that potentially require data from period before
      # forecast year (e.g., catchment discharge, which requires a two year
      # warm-up and photoperiod weighted degree days, which are calculated from
      # the winter solstice immediately preceding the forecast target season) ----

      if (species_bio_year_name == "salmonid_year") {
        # LOCATE WINTER SOLSTICE DATES AND END OF SALMON YEAR
        unique_salmon_years <- unique(y[[species_bio_year_name]])
        # find solstice using reanalysis dataset...

        reanalysis_dataset <- reanalysis_data_bio_years

        # find winter solstice two preceding years ago (for hydrologic model
        # warm-up) and one year ago (for photoperiod weighted degree days)
        solstice_year_1 <- unique_salmon_years[[1]] - 1
        solstice_year_2 <- unique_salmon_years[[1]] - 2

        salmon_solstice_mat <- as.data.frame(matrix(ncol = 5, nrow = length(solstice_year_1)))
        colnames(salmon_solstice_mat) <- c("salmonid_year", "date_sols_1", "date_sols_2", "date_start_year",
                                           "date_end_year")
        class(salmon_solstice_mat$date_sols_1) = "Date"
        class(salmon_solstice_mat$date_sols_2) = "Date"
        class(salmon_solstice_mat$date_end_year) = "Date"
        class(salmon_solstice_mat$date_start_year) = "Date"

        salmon_solstice_mat[1, "date_sols_1"] <-
          reanalysis_dataset$date[reanalysis_dataset$salmonid_year ==
                                    solstice_year_1[1]][which.min(reanalysis_dataset$photoper[reanalysis_dataset$salmonid_year == solstice_year_1[1]])]
        salmon_solstice_mat[1, "date_sols_2"] <-
          reanalysis_dataset$date[reanalysis_dataset$salmonid_year ==
                                    solstice_year_2[1]][which.min(reanalysis_dataset$photoper[reanalysis_dataset$salmonid_year == solstice_year_2[1]])]
        salmon_solstice_mat[1, "salmonid_year"] <-
          solstice_year_1[1] + 1
        salmon_solstice_mat[1, "date_end_year"] <-
          y$date[y$salmonid_year == solstice_year_1[1] + 1][which.max(y$salmonid_yday[y$salmonid_year ==
                                                                                        solstice_year_1[1] + 1])]
        salmon_solstice_mat[1, "date_start_year"] <-
          y$date[y$salmonid_year == solstice_year_1[1] + 1][which.min(y$salmonid_yday[y$salmonid_year ==
                                                                                        solstice_year_1[1] + 1])]

        # -------------------------------------------------------------------------------------------------- #
        # subset reanalysis between warm-up start and beginning of forecast year ----
        reanalysis_subset <-
          reanalysis_dataset[data.table::between(
            x = reanalysis_dataset$date,
            lower = salmon_solstice_mat[1, "date_sols_1"],
            upper = salmon_solstice_mat[1, "date_start_year"] - lubridate::days(1),
            incbounds = TRUE,
            NAbounds = FALSE
          ),
          c("date", "tas_rean")]

        names(reanalysis_subset) = c("date", "forecast_tas")

        # append reanalysis_subset to forecast year data
        merged_list_rean_subset <- y[, c("date", "forecast_tas")]

        # merge dfs
        merged_meanT_solstice_end_salmonid_run <- rbind(reanalysis_subset, merged_list_rean_subset)

        # ------------------------------------------------------------------------------------------------- #
        # estimate water temperature ----
        merged_meanT_solstice_end_salmonid_run$water_temp <-
          fishcastr::air_to_water_model(
            Ta = merged_meanT_solstice_end_salmonid_run$forecast_tas,
            Yday = lubridate::yday(merged_meanT_solstice_end_salmonid_run$date),
            A = air_to_water_model_params$A,
            ac = air_to_water_model_params$ac,
            b = air_to_water_model_params$b,
            B = air_to_water_model_params$B
          )

        merged_meanT_solstice_end_salmonid_run$Delta_temp <-
          fishcastr::delta_var(y = merged_meanT_solstice_end_salmonid_run$water_temp,
                               react_time = fish_reaction_time)

        # subset back to solstice immediately prior to run
        merged_meanT_solstice_end_salmonid_run <-
          merged_meanT_solstice_end_salmonid_run[merged_meanT_solstice_end_salmonid_run$date >=
                                                   salmon_solstice_mat$date_start_year,]

        # add photoperiod
        merged_meanT_solstice_end_salmonid_run$photoper <-
          fishcastr::photper_calc(
            dates = merged_meanT_solstice_end_salmonid_run$date,
            latitude = 53.932458,
            longitude = -9.575556
          )

        # calculate weighted meanT for the reanalysis dataset
        merged_meanT_solstice_end_salmonid_run$meanT_weighted <-
          ifelse(
            merged_meanT_solstice_end_salmonid_run$water_temp <=
              0,
            0,
            merged_meanT_solstice_end_salmonid_run$water_temp * (
              merged_meanT_solstice_end_salmonid_run$photoper / max_photoper
            )
          )

        # ------------------------------------------------------------------------------------------------- #
        # calculate cumulative meanT_weighted since solstice
        merged_meanT_solstice_end_salmonid_run$weighted_dds_water <-
          cumsum(merged_meanT_solstice_end_salmonid_run$meanT_weighted)

        # subset weighted dds and merge to df in list for current member
        weighted_dds_df <-
          merged_meanT_solstice_end_salmonid_run[data.table::between(
            x = merged_meanT_solstice_end_salmonid_run$date,
            lower = salmon_solstice_mat[1, "date_start_year"],
            upper = salmon_solstice_mat[1, "date_end_year"],
            incbounds = TRUE,
            NAbounds = FALSE
          ),
          c("date",
            "weighted_dds_water",
            "water_temp",
            "Delta_temp")]

        weighted_dds_water <- weighted_dds_df[["weighted_dds_water"]]

        # forecast water temp
        Tw_mod <- weighted_dds_df[["water_temp"]]

        # delta water temp
        Delta_temp <- weighted_dds_df[["Delta_temp"]]

        # ------------------------------------------------------------------------------------------------- #
        # Catchment discharge ----
        reanalysis_subset_hydro <-
          reanalysis_dataset[data.table::between(
            x = reanalysis_dataset$date,
            lower = salmon_solstice_mat[1, "date_sols_2"],
            upper = salmon_solstice_mat[1, "date_start_year"] - lubridate::days(1),
            incbounds = TRUE,
            NAbounds = FALSE
          ),
          c("date", "pr_rean","petH_rean")]

        names(reanalysis_subset_hydro) = c("date", "forecast_pr","forecast_petH")

        # append reanalysis_subset to forecast year data
        merged_list_rean_subset_hydro <- y[, c("date", "forecast_pr","forecast_petH")]

        # merge dfs
        merged_PEV_pr_solstice_end_salmonid_run <- rbind(reanalysis_subset_hydro, merged_list_rean_subset_hydro)
        names(merged_PEV_pr_solstice_end_salmonid_run) = c("date","pr","petH")

        # GR4J procedure ----
        discharge = fishcastr::hydrologic_model(
          met_data = merged_PEV_pr_solstice_end_salmonid_run,
          warm_up_dates_range = c(
            salmon_solstice_mat[1, "date_sols_2"],
            salmon_solstice_mat[1, "date_start_year"] - lubridate::days(1)
          ),
          run_dates_range = c(salmon_solstice_mat[1, "date_start_year"],
                              salmon_solstice_mat[1, "date_end_year"]),
          GR4J_params = as.numeric(fishcastr::GR4J_Burr_params_ERA5_bcc),
          log_transform = FALSE
        )[,2]

        ln_discharge <- log(discharge)

        # -------------------------------------------------------------------------------------------------- #
        # migration preparedness ----
        migration_prep <- fishcastr::exp_mod_Gaussian_resp(
          x = weighted_dds_water,
          c = calibrated_physio_model[["coefs"]]["c"],
          mu_exmg = calibrated_physio_model[["coefs"]]["mu_exmg"],
          sigma_exmg = calibrated_physio_model[["coefs"]]["sigma_exmg"],
          lamb = calibrated_physio_model[["coefs"]]["lamb"]
        )

        ln_migration_prep <- log(migration_prep)

      }

      if (species_bio_year_name == "eel_year") {
        # LOCATE WINTER SOLSTICE DATES AND END OF EEL YEAR
        unique_eel_years <- unique(y[[species_bio_year_name]])
        # find solstice using reanalysis dataset...

        reanalysis_dataset <- reanalysis_data_bio_years

        # find winter solstice two preceding years ago (for hydrologic model
        # warm-up) and one year ago (for photoperiod weighted degree days)
        solstice_year_1 <- unique_eel_years[[1]] - 1
        solstice_year_2 <- unique_eel_years[[1]] - 2

        eel_solstice_mat <- as.data.frame(matrix(ncol = 5, nrow = length(solstice_year_1)))
        colnames(eel_solstice_mat) <- c("eel_year", "date_sols_1", "date_sols_2", "date_start_year",
                                           "date_end_year")
        class(eel_solstice_mat$date_sols_1) = "Date"
        class(eel_solstice_mat$date_sols_2) = "Date"
        class(eel_solstice_mat$date_end_year) = "Date"
        class(eel_solstice_mat$date_start_year) = "Date"

        eel_solstice_mat[1, "date_sols_1"] <-
          reanalysis_dataset$date[reanalysis_dataset$eel_year ==
                                    solstice_year_1[1]][which.min(reanalysis_dataset$photoper[reanalysis_dataset$eel_year == solstice_year_1[1]])]
        eel_solstice_mat[1, "date_sols_2"] <-
          reanalysis_dataset$date[reanalysis_dataset$eel_year ==
                                    solstice_year_2[1]][which.min(reanalysis_dataset$photoper[reanalysis_dataset$eel_year == solstice_year_2[1]])]
        eel_solstice_mat[1, "eel_year"] <-
          solstice_year_1[1] + 1
        eel_solstice_mat[1, "date_end_year"] <-
          y$date[y$eel_year == solstice_year_1[1] + 1][which.max(y$eel_yday_biofix[y$eel_year ==
                                                                                        solstice_year_1[1] + 1])]
        eel_solstice_mat[1, "date_start_year"] <-
          y$date[y$eel_year == solstice_year_1[1] + 1][which.min(y$eel_yday_biofix[y$eel_year ==
                                                                                        solstice_year_1[1] + 1])]

        # -------------------------------------------------------------------------------------------------- #
        # subset reanalysis between warm-up start and beginning of forecast year ----
        reanalysis_subset <-
          reanalysis_dataset[data.table::between(
            x = reanalysis_dataset$date,
            lower = eel_solstice_mat[1, "date_sols_2"],
            upper = eel_solstice_mat[1, "date_start_year"] - lubridate::days(1),
            incbounds = TRUE,
            NAbounds = FALSE
          ),
          c("date", "tas_rean")]

        names(reanalysis_subset) = c("date", "forecast_tas")

        # append reanalysis_subset to forecast year data
        merged_list_rean_subset <- y[, c("date", "forecast_tas")]

        # merge dfs
        merged_meanT_solstice_end_eel_run <- rbind(reanalysis_subset, merged_list_rean_subset)

        # ------------------------------------------------------------------------------------------------- #
        # estimate water temperature ----
        merged_meanT_solstice_end_eel_run$water_temp <-
          fishcastr::air_to_water_model(
            Ta = merged_meanT_solstice_end_eel_run$forecast_tas,
            Yday = lubridate::yday(merged_meanT_solstice_end_eel_run$date),
            A = air_to_water_model_params$A,
            ac = air_to_water_model_params$ac,
            b = air_to_water_model_params$b,
            B = air_to_water_model_params$B
          )

        merged_meanT_solstice_end_eel_run$Delta_temp <-
          fishcastr::delta_var(y = merged_meanT_solstice_end_eel_run$water_temp,
                               react_time = fish_reaction_time)

        # subset back to solstice immediately prior to run
        merged_meanT_solstice_end_eel_run <-
          merged_meanT_solstice_end_eel_run[merged_meanT_solstice_end_eel_run$date >=
                                                   eel_solstice_mat$date_sols_1,]

        # add photoperiod
        merged_meanT_solstice_end_eel_run$photoper <-
          fishcastr::photper_calc(
            dates = merged_meanT_solstice_end_eel_run$date,
            latitude = 53.932458,
            longitude = -9.575556
          )

        # calculate weighted meanT for the reanalysis dataset
        merged_meanT_solstice_end_eel_run$meanT_weighted <-
          ifelse(
            merged_meanT_solstice_end_eel_run$water_temp <=
              0,
            0,
            merged_meanT_solstice_end_eel_run$water_temp * (
              merged_meanT_solstice_end_eel_run$photoper / max_photoper
            )
          )

        # ------------------------------------------------------------------------------------------------- #
        # calculate cumulative meanT_weighted since solstice
        merged_meanT_solstice_end_eel_run$weighted_dds_water <-
          cumsum(merged_meanT_solstice_end_eel_run$meanT_weighted)

        # subset weighted dds and merge to df in list for current member
        weighted_dds_df <-
          merged_meanT_solstice_end_eel_run[data.table::between(
            x = merged_meanT_solstice_end_eel_run$date,
            lower = eel_solstice_mat[1, "date_start_year"],
            upper = eel_solstice_mat[1, "date_end_year"],
            incbounds = TRUE,
            NAbounds = FALSE
          ),
          c("date",
            "weighted_dds_water",
            "water_temp",
            "Delta_temp")]

        weighted_dds_water <- weighted_dds_df[["weighted_dds_water"]]

        # forecast water temp
        Tw_mod <- weighted_dds_df[["water_temp"]]

        # delta water temp
        Delta_temp <- weighted_dds_df[["Delta_temp"]]

        # ------------------------------------------------------------------------------------------------- #
        # Catchment discharge ----
        reanalysis_subset_hydro <-
          reanalysis_dataset[data.table::between(
            x = reanalysis_dataset$date,
            lower = eel_solstice_mat[1, "date_sols_2"],
            upper = eel_solstice_mat[1, "date_start_year"] - lubridate::days(1),
            incbounds = TRUE,
            NAbounds = FALSE
          ),
          c("date", "pr_rean","petH_rean")]

        names(reanalysis_subset_hydro) = c("date", "forecast_pr","forecast_petH")

        # append reanalysis_subset to forecast year data
        merged_list_rean_subset_hydro <- y[, c("date", "forecast_pr","forecast_petH")]

        # merge dfs
        merged_PEV_pr_solstice_end_eel_run <- rbind(reanalysis_subset_hydro, merged_list_rean_subset_hydro)
        names(merged_PEV_pr_solstice_end_eel_run) = c("date","pr","petH")

        # GR4J procedure ----
        discharge = fishcastr::hydrologic_model(
          met_data = merged_PEV_pr_solstice_end_eel_run,
          warm_up_dates_range = c(
            eel_solstice_mat[1, "date_sols_2"],
            eel_solstice_mat[1, "date_start_year"] - lubridate::days(1)
          ),
          run_dates_range = c(eel_solstice_mat[1, "date_start_year"],
                              eel_solstice_mat[1, "date_end_year"]),
          GR4J_params = as.numeric(fishcastr::GR4J_Burr_params_ERA5_bcc),
          log_transform = FALSE
        )[,2]

        ln_discharge <- log(discharge)

        # -------------------------------------------------------------------------------------------------- #
        # migration preparedness ----
        migration_prep <- fishcastr::exp_mod_Gaussian_resp_inv(
          x = weighted_dds_water,
          c = calibrated_physio_model[["coefs"]]["c"],
          mu_exmg = calibrated_physio_model[["coefs"]]["mu_exmg"],
          sigma_exmg = calibrated_physio_model[["coefs"]]["sigma_exmg"],
          lamb = calibrated_physio_model[["coefs"]]["lamb"]
        )

        ln_migration_prep <- log(migration_prep)

      }

      # -------------------------------------------------------------------------------------------------- #
      # populate data.frame with predictors ----
      df <- data.frame(y,
                       moonlight_exposure,
                       ln_moonlight_exposure,
                       migration_prep,
                       ln_migration_prep,
                       discharge,
                       ln_discharge,
                       Tw_mod,
                       Delta_temp)

      # -------------------------------------------------------------------------------------------------- #
      # subset and scale predictors according to training params ----

      # View(q) q <- predictor_scaling_params note this argument q is included effectively as an argument to the nested
      # llply function, as is z... q <- predictor_scaling_params
      preprocess_names <-  q[[which(names(q) == unique(y[[species_bio_year_name]]))]]$method$scale
      subset_df_list <- df[, c(response_var_name, species_bio_year_name, preprocess_names)]
      scaled_df_list <-  predict(object = q[[which(names(q) == unique(y[[species_bio_year_name]]))]],
                                 newdata = subset_df_list)

      # generate predictions from model ----
      if (forecast_method == "deterministic") {
          # z <- calibrated_daily_model
        forecast_sim_counts <-
          fishcastr::simulate_glm(
            object = z[[which(names(z) == unique(y[[species_bio_year_name]]))]],
            X = scaled_df_list,
            pred_type = "simulate",
            PIs = FALSE,
            lapply_seed = parallel_seed_no,
            parallel = parallel
          )

        new_means <-
          fishcastr::simulate_glm(
            object = z[[which(names(z) == unique(y[[species_bio_year_name]]))]],
            X = scaled_df_list,
            pred_type = "predict",
            lapply_seed = parallel_seed_no,
            parallel = parallel
          )

        forecast_mean_pred_counts <-
          new_means[["fitted_y"]]["mean.count"]

        sim_PIs_train <-
          fishcastr::simulate_glm(
            object = z[[which(names(z) == unique(y[[species_bio_year_name]]))]],
            X = scaled_df_list,
            pred_type = "simulate",
            PIs = TRUE,
            lapply_seed = parallel_seed_no,
            parallel = parallel
          )

        upper_PI <- sim_PIs_train[["upper_PI"]]
        boot_median <- sim_PIs_train[["median"]]
        low_PI <- sim_PIs_train[["low_PI"]]

        df2 <-
          data.frame(df,
                     forecast_sim_counts,
                     forecast_mean_pred_counts,
                     upper_PI,
                     boot_median,
                     low_PI)
      }

      # stochastic simulation
      if (forecast_method == "stochastic") {
        # z = calibrated_daily_model

        # simulated count including uncertainty owing to count process (PIs not extracted, but see simulate_glm function)
        forecast_sim_counts <-
          fishcastr::simulate_glm(
            object = z[[which(names(z) == unique(y[[species_bio_year_name]]))]],
            X = scaled_df_list,
            pred_type = "simulate",
            PIs = FALSE,
            lapply_seed = parallel_seed_no,
            parallel = parallel
          )

        # simulation based mean with PPI based CIs
        forecast_mean_pred_counts <-
          fishcastr::simulate_glm(
            object = z[[which(names(z) == unique(y[[species_bio_year_name]]))]],
            X = scaled_df_list,
            pred_type = "predict",
            lapply_seed = parallel_seed_no,
            parallel = parallel
          )

        # old_means <- forecast_mean_pred_counts[['fitted_y_old']]
        new_means <-
          forecast_mean_pred_counts[["fitted_y"]][["mean.count"]]
        lwr_ci <-
          forecast_mean_pred_counts[["fitted_y"]][["ci.ucount_lwr"]]
        upr_ci <-
          forecast_mean_pred_counts[["fitted_y"]][["ci.ucount_upr"]]
        forecast_sim_mean <-
          forecast_mean_pred_counts[["fitted_y"]][["pred.ucount.psim"]]

        df2 <-
          data.frame(df,
                     new_means,
                     lwr_ci,
                     upr_ci,
                     forecast_sim_mean,
                     forecast_sim_counts)
      }

      return(df2)

    }, q = predictor_scaling_params, z = calibrated_daily_model)

    return(sub_list)

  }, .progress = "text", .parallel = parallel, .paropts = list(.options.snow = opts))  # Parallel switch

  if (parallel) {
    parallel::stopCluster(cl)  # Close all open clusters
  }

  result <- test_list

  return(result)
}
