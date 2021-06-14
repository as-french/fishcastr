# ---------------------------------------------------------------------------------------------------------- #
# A script for fitting an exponentially modified Gaussian response function to
# silver eel count data in relation to photoperiod weighted degree days. The
# exported model parameters facilitate transformation of photoperiod weighted
# degree days into a proxy variable for "migration preparedness" in the
# Burrishoole catchment, Ireland.
# ---------------------------------------------------------------------------------------------------------- #
# PREDICT WATER TEMPERATURE ----
# load model calibrated parameters
air_to_water_Feeagh_params_ERA5_bcc <- fishcastr::air_to_water_Feeagh_params_ERA5_bcc

# load reanalysis (bias corrected) contains precipitation and potential evapotranspiration
# SPRING
grid_ERA5_1979_2019_Jun_bc <- fishcastr::grid_ERA5_1979_2019_Jun_bc

# convert grid reanalysis to two column table: date, variable
data_ERA5_1979_2019_Jun_bc <- fishcastr::convert_grid_to_dataframe(grid_obj = grid_ERA5_1979_2019_Jun_bc)[,-2]
names(data_ERA5_1979_2019_Jun_bc)[which(names(data_ERA5_1979_2019_Jun_bc) == "dates1")] <- "date"

# calculate water temperature
Tw <- fishcastr::air_to_water_model(Ta = data_ERA5_1979_2019_Jun_bc$tas,
                                    Yday = lubridate::yday(data_ERA5_1979_2019_Jun_bc$date),
                                    A = air_to_water_Feeagh_params_ERA5_bcc$A,
                                    ac = air_to_water_Feeagh_params_ERA5_bcc$ac,
                                    b = air_to_water_Feeagh_params_ERA5_bcc$b,
                                    B = air_to_water_Feeagh_params_ERA5_bcc$B)
data_water_temp <- data.frame("date" = data_ERA5_1979_2019_Jun_bc$date,
                              "water_temp" = Tw)

# stratify forecast years
data_seel_forecast_years <- fishcastr::bio_year_therm(mean_temp = data_water_temp$water_temp,
                                          dates = data_water_temp$date,
                                          biofix_temp = 11,
                                          min_no_days_above_biofix = 10,
                                          increasing_temp = TRUE,
                                          yday_head = "eel_yday",
                                          bio_year_head = "eel_year",
                                          days_since_earliest_biofix_head = "eel_yday_biofix",
                                          incomplete_first_year = 1978,
                                          start_previous_calendar_year = FALSE)

# merge with photoperiod
data_photo_period <- data.frame("date" = data_seel_forecast_years$date,
                                "photoper" = fishcastr::photper_calc(dates = data_seel_forecast_years$date,
                                                                     latitude = 53.932458,
                                                                     longitude = -9.575556))

data_seel_forecast_years_water_temp.merge <- merge(data_photo_period,
                                                     data_seel_forecast_years, by = "date")

data_seel_forecast_years <- data_seel_forecast_years_water_temp.merge
names(data_seel_forecast_years)[which(names(data_seel_forecast_years) == "meanT")] <- "water_temp"

  # WEIGHT TEMPERATURES (MULTIPLY TEMPS BY SCALED PHOTOPERIOD...) -------------------------------------
data_seel_forecast_years$water_temp_weighted <- ifelse(data_seel_forecast_years$water_temp <=0, 0, data_seel_forecast_years$water_temp*(data_seel_forecast_years$photoper/max(data_seel_forecast_years$photoper)))
###########
# --------------------------------------------------------------------------------------------------- #
# LOCATE ALL WINTER SOLSTICE DATES AND END OF SUBSEQUENT EEL YEAR ----
unique_eel_years <- unique(data_seel_forecast_years$eel_year)
#eel_solstice_mat <-NULL
eel_solstice_mat <- as.data.frame(matrix(ncol = 3, nrow = length(unique_eel_years)))
colnames(eel_solstice_mat) <- c("forecast_year", "date_sols", "date_end_year")
class(eel_solstice_mat$date_sols) = "Date"
class(eel_solstice_mat$date_end_year) = "Date"

for(i in 1:length(unique_eel_years)){
  eel_solstice_mat[i,"date_sols"] <- data_seel_forecast_years$date[data_seel_forecast_years$eel_year == unique_eel_years[i]][which.min(data_seel_forecast_years$photoper[data_seel_forecast_years$eel_year == unique_eel_years[i]])]
  eel_solstice_mat[i,"forecast_year"] <- unique_eel_years[i+1]
  eel_solstice_mat[i,"date_end_year"] <- data_seel_forecast_years$date[data_seel_forecast_years$eel_year == unique_eel_years[i+1]][which.max(data_seel_forecast_years$eel_yday_biofix[data_seel_forecast_years$eel_year == unique_eel_years[i+1]])]
}

eel_list_data <- eel_solstice_mat[-c(nrow(eel_solstice_mat)),]

# --------------------------------------------------------------------------------------------------- #
# CREATE LIST OF NEW "FORECAST YEARS" ----
unique_eel_years <- unique(eel_list_data[["forecast_year"]])
list_eel_years <- lapply(1:length(unique_eel_years),FUN = function(x){
  result <- as.data.frame(data_seel_forecast_years[data.table::between(data_seel_forecast_years$date,lower = as.Date(eel_list_data[x,"date_sols"]),upper =  as.Date(eel_list_data[x,"date_end_year"])), c("date","photoper","water_temp_weighted","eel_year")])
  return(result)
  })
names(list_eel_years) <- unique_eel_years

# --------------------------------------------------------------------------------------------------- #
# CALCULATE DDS FOR EACH FORECAST YEAR (water from solstice) ----
list_dds <-lapply(list_eel_years, function(x)
  cbind(x, weighted_dds_water = cumsum(x$water_temp_weighted)))

# --------------------------------------------------------------------------------------------------- #
# REMOVE LIST ELEMENT DATE ROWS OUTSIDE FORECAST YEAR AND SELECT ONLY DATE AND DDS SOL ----
list_dds_forecast_year <- lapply(1:length(list_dds),FUN = function(x){
  year_i <- as.numeric(names(list_dds)[x])
  result <- list_dds[[x]][list_dds[[x]]["eel_year"] == year_i, c("date","weighted_dds_water")]
  return(result)
})
names(list_dds_forecast_year) <- names(list_dds)

# --------------------------------------------------------------------------------------------------- #
# COMBINE ALL ----
data_dds <- data.table::rbindlist(l = list_dds_forecast_year, use.names = T,fill = T,idcol = F)

# --------------------------------------------------------------------------------------------------- #
# PLOT DDS vs. COUNTS
fishcastr::download_fish_data()
data_seel <- fishcastr::import_fish_data(species = "seel")

data_seel_dds.merge <-Reduce(f = function(x,y){merge(x = x,
                                                       y = y,
                                                       by = "date")},
                               x = list(data_seel,
                                        data_seel_forecast_years,
                                        data_dds))

# subset to 1981
data_seel_dds.merge_1981 <- data_seel_dds.merge[data_seel_dds.merge$eel_year >= 1981,]

# convert counts to percentages of run
data_seel_dds.merge_1981$seel_perc_run <- NA

for(i in list(unique(data_seel_dds.merge_1981[["eel_year"]]))[[1]]){
  data_seel_dds.merge_1981$seel_perc_run[data_seel_dds.merge_1981[["eel_year"]] == i] <- 100*((data_seel_dds.merge_1981[["seel"]][data_seel_dds.merge_1981[["eel_year"]] == i])/sum(data_seel_dds.merge_1981[["seel"]][data_seel_dds.merge_1981[["eel_year"]] == i]))
}

# PLOT ALL DATA TO CHECK FOR ERRORS
# # note early 2019 counts (June) appear as outliers here.
# for(i in unique(data_seel_dds.merge_1981$eel_year)){
#
#    plot(data_seel_dds.merge_1981$weighted_dds_water[data_seel_dds.merge_1981$seel_perc_run > 0],
#       data_seel_dds.merge_1981$seel_perc_run[data_seel_dds.merge_1981$seel_perc_run > 0], cex = 0.5, col = "black", xlim = c(100,4000), ylim = c(0,35),main = i)
#
#  points(data_seel_dds.merge_1981$weighted_dds_water[data_seel_dds.merge_1981$seel_perc_run > 0 & data_seel_dds.merge_1981$eel_year == i],
#       data_seel_dds.merge_1981$seel_perc_run[data_seel_dds.merge_1981$seel_perc_run > 0 & data_seel_dds.merge_1981$eel_year == i], cex = 0.5, col = "green3")
#  }
# #legend("topright",
# #       legend = unique(data_seel_dds.merge_1981$eel_year),
# #       col = unique(data_seel_dds.merge_1981$eel_year),cex = 0.5, bty = "n",pch = 1)

# identify multiplication factor
mult_factor <- 0.5001/min(data_seel_dds.merge_1981$seel_perc_run[data_seel_dds.merge_1981$seel_perc_run > 0],na.rm = TRUE)
data_seel_dds.merge_1981$seel_perc_run_int <- round(data_seel_dds.merge_1981$seel_perc_run*mult_factor)

# --------------------------------------------------------------------------------------------------- #
# FIT CURVE TO PHOTOPERIOD WEIGHTED DEGREE DAYS ----

data_list_years <- list(c(1981:1983,1985:1988,1990:1992),
                        1993:2002,
                        2003:2012,
                        2013:2018,
                        c(1981:1983,1985:1988,1990:1992))

dataset_list <- lapply(data_list_years,FUN = function(x){
  data_seel_dds.merge_1981[data_seel_dds.merge_1981$eel_year %in% x,
                             c("seel_perc_run_int","weighted_dds_water")]
})

dataset_list_nameslist <- lapply(data_list_years,
                                 FUN = function(x){paste0(range(x)[1],"-",range(x)[2])})
names(dataset_list) <- dataset_list_nameslist

# Create vignette sub directories ----
dirName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/vignette_figures/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

dirName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/vignette_figures/migration_prep_models/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

dirName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/vignette_figures/migration_prep_models/eel/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

dirName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/vignette_figures/migration_prep_models/eel/val_plots/")
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# # try different conditional distributions ----

# physio_negbin <-
#   physio_model_evaluation_exp_mod_Gauss_inv(
#     species_count_lab = "seel_perc_run_int",
#     x_variable_lab = "weighted_dds_water",
#     data_list = dataset_list,
#     error_distribution_name = "negative_binomial",
#     species_name = "European eel",
#     file_path_plot = paste0(getwd(),'/vignette_figures/FigS9/negbin_FigS9c.png'),
#     file_path_resid_fitted_plot =  paste0(getwd(),'/vignette_figures/FigS10/eel/negbin_FigS10b.png'),
#     file_path_dispersion_zeroinf_acf_plot =  paste0(getwd(),'/vignette_figures/FigS10/eel/negbin_FigS10c.png'),
#     file_path_coef_profile_plot =  paste0(getwd(),'/vignette_figures/FigS10/eel/negbin_FigS10d.png'),
#     start_values = list(c = 200, mu_exmg = 2600, sigma_exmg = 240, lamb = 210, sigma_nb = 5),
#     lower_lims = list(c = 1, mu_exmg=2000, sigma_exmg=50, lamb = 50, sigma_nb = 0.01),
#     ylims = c(-400,400),
#     xlims = c(500,4200)
#   )
#
# physio_quasi_Poisson <-
#   physio_model_evaluation_exp_mod_Gauss_inv(
#     species_count_lab = "seel_perc_run_int",
#     x_variable_lab = "weighted_dds_water",
#     data_list = dataset_list,
#     error_distribution_name = "quasi_Poisson",
#     species_name = "European eel",
#     file_path_plot = paste0(getwd(),'/vignette_figures/FigS9/qPoiss_FigS9c.png'),
#     file_path_resid_fitted_plot = paste0(getwd(),'/vignette_figures/FigS10/eel/qPoiss_FigS10b.png'),
#     file_path_dispersion_zeroinf_acf_plot = paste0(getwd(),'/vignette_figures/FigS10/eel/qPoiss_FigS10c.png'),
#     file_path_coef_profile_plot = paste0(getwd(),'/vignette_figures/FigS10/eel/qPoiss_FigS10d.png'),
#     start_values = list(c = 200, mu_exmg = 2600, sigma_exmg = 240, lamb = 210, sigma_nb = 5),
#     lower_lims = list(c = 100, mu_exmg=2000, sigma_exmg=30, lamb = 5, sigma_nb = 0.01),
#     ylims = c(-400,400),
#     xlims = c(500,4200)
#   )

dirName <- paste0(system.file("extdata", package = "fishcastr"),
                  "/vignette_figures/migration_prep_models/eel/")

library(RMKdiscrete)
physio_generalised_Poisson <-
  fishcastr::physio_model_evaluation_exp_mod_Gauss_inv(
    species_count_lab = "seel_perc_run_int",
    x_variable_lab = "weighted_dds_water",
    data_list = dataset_list,
    error_distribution_name = "generalised_Poisson",
    species_name = "European eel",
    file_path_plot = paste0(dirName,'genpois_eel.png'),
    file_path_resid_fitted_plot = paste0(dirName,'val_plots/resid_fitted.png'),
    file_path_dispersion_zeroinf_acf_plot = paste0(dirName,'val_plots/dispersion.png'),
    file_path_coef_profile_plot = paste0(dirName,'val_plots/param_profiles.png'),
    start_values = list(c = 200, mu_exmg = 2600,sigma_exmg = 240, lamb = 210,phi = 5),
    lower_lims = list(c = 1, mu_exmg=2000, sigma_exmg=30, lamb = 10, phi = 0.01),
    ylims = c(-500,500),
    xlims = c(200,4200)
  )

# assign physio model to new object.
model_physio_expmG_inv_eel <- list("data" = physio_generalised_Poisson[["mod_physio_expmG_inv"]]@data,
                                  "coefs" = bbmle::coef(physio_generalised_Poisson[["mod_physio_expmG_inv"]]),
                                  "vcov_mat" = bbmle::vcov(physio_generalised_Poisson[["mod_physio_expmG_inv"]]))

# export fitted model coefficients etc.,
usethis::use_data(model_physio_expmG_inv_eel, overwrite = TRUE)
