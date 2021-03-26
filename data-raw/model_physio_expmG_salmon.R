# ---------------------------------------------------------------------------------------------------------- #
# A script for fitting an exponentially modified Gaussian response function to
# salmon smolt count data in relation to photoperiod weighted degree days. The
# exported model parameters facilitate transformation of photoperiod weighted
# degree days into a proxy variable for "migration preparedness" in the
# Burrishoole catchment, Ireland.
# ---------------------------------------------------------------------------------------------------------- #
# PREDICT WATER TEMPERATURE ----
# load model calibrated parameters
air_to_water_Feeagh_params_ERA5_bcc <- fishcastr::air_to_water_Feeagh_params_ERA5_bcc

# load reanalysis (bias corrected) contains precipitation and potential evapotranspiration
# SPRING
grid_ERA5_1979_2019_Jan_bc <- fishcastr::grid_ERA5_1979_2019_Jan_bc
# convert grid reanalysis to two column table: date, variable
data_ERA5_1979_2019_Jan_bc <- fishcastr::convert_grid_to_dataframe(grid_obj = grid_ERA5_1979_2019_Jan_bc)[,-2]
names(data_ERA5_1979_2019_Jan_bc)[which(names(data_ERA5_1979_2019_Jan_bc) == "dates1")] <- "date"

# calculate water temperature
Tw <- fishcastr::air_to_water_model(Ta = data_ERA5_1979_2019_Jan_bc$tas,
                                    Yday = lubridate::yday(data_ERA5_1979_2019_Jan_bc$date),
                                    A = air_to_water_Feeagh_params_ERA5_bcc$A,
                                    ac = air_to_water_Feeagh_params_ERA5_bcc$ac,
                                    b = air_to_water_Feeagh_params_ERA5_bcc$b,
                                    B = air_to_water_Feeagh_params_ERA5_bcc$B)
data_water_temp <- data.frame("date" = data_ERA5_1979_2019_Jan_bc$date,
                              "water_temp" = Tw)

# stratify forecast years
data_ssmolt_forecast_years <- fishcastr::bio_year_photoper(dates = data_water_temp$date,
                                                           latitude = 53.932458,
                                                           longitude = -9.575556,
                                                           retain_photoper = TRUE,
                                                           start_previous_year = TRUE,
                                                           shortest_day = TRUE,
                                                           yday_head = "salmonid_yday",
                                                           bio_year_head = "salmonid_year")

data_ssmolt_forecast_years_water_temp.merge <- merge(data_water_temp,
                                                     data_ssmolt_forecast_years, by = "date")

data_ssmolt_forecast_years <- data_ssmolt_forecast_years_water_temp.merge

# WEIGHT TEMPERATURES (MULTIPLY TEMPS BY SCALED PHOTOPERIOD...) -------------------------------------
data_ssmolt_forecast_years$water_temp_weighted <- ifelse(data_ssmolt_forecast_years$water_temp <=0, 0, data_ssmolt_forecast_years$water_temp*(data_ssmolt_forecast_years$photoper/max(data_ssmolt_forecast_years$photoper)))

# calculate photoperiod weighted degree days from winter solstice
# LOCATE ALL WINTER SOLSTICE DATES AND END OF SUBSEQUENT FORECAST YEAR ----
unique_forecast_years <- unique(data_ssmolt_forecast_years$salmonid_year)

# define matrix for year delineation by solstice
solstice_mat <- as.data.frame(matrix(ncol = 3, nrow = length(unique_forecast_years)))
colnames(solstice_mat) <- c("forecast_year", "date_sols", "date_end_year")
class(solstice_mat$date_sols) = "Date"
class(solstice_mat$date_end_year) = "Date"

for(i in 1:length(unique_forecast_years)){
  solstice_mat[i,"date_sols"] <- data_ssmolt_forecast_years$date[data_ssmolt_forecast_years$salmonid_year == unique_forecast_years[i]][which.min(data_ssmolt_forecast_years$photoper[data_ssmolt_forecast_years$salmonid_year == unique_forecast_years[i]])]
  solstice_mat[i,"forecast_year"] <- unique_forecast_years[i]
  solstice_mat[i,"date_end_year"] <- data_ssmolt_forecast_years$date[data_ssmolt_forecast_years$salmonid_year == unique_forecast_years[i]][which.max(data_ssmolt_forecast_years$salmonid_yday[data_ssmolt_forecast_years$salmonid_year == unique_forecast_years[i]])]
}

salmonid_list_data <- solstice_mat[-c(1,nrow(solstice_mat)),]

# --------------------------------------------------------------------------------------------------- #
# CREATE LIST OF NEW "FORECAST YEARS" ----
list_salmonid_years <- list()

unique_forecast_years <- unique(salmonid_list_data[["forecast_year"]])
for(i in 1:length(unique_forecast_years)){
  list_salmonid_years[[i]] <- data_ssmolt_forecast_years[data_ssmolt_forecast_years$date >= as.Date(salmonid_list_data[i,"date_sols"]) & data_ssmolt_forecast_years$date <= as.Date(salmonid_list_data[i,"date_end_year"]),
                                                         c("photoper","date","water_temp_weighted","salmonid_year")]
  names(list_salmonid_years)[i] <- unique_forecast_years[i]
}

# --------------------------------------------------------------------------------------------------- #
# CALCULATE DDS FOR EACH FORECAST YEAR (water from solstice) ----
list_dds <-lapply(list_salmonid_years, function(x)
  cbind(x, weighted_dds_water = cumsum(x$water_temp_weighted)))

# --------------------------------------------------------------------------------------------------- #
# REMOVE LIST ELEMENT DATE ROWS OUTSIDE FORECAST YEAR AND SELECT ONLY DATE AND DDS SOL ----
list_dds_forecast_year <- list()
for(i in 1:length(list_dds)){
  year_i <- as.numeric(names(list_dds)[i])
  list_dds_forecast_year[[i]] <- subset(list_dds[[i]],
                                        salmonid_year == year_i,
                                        select = c(date,weighted_dds_water))
  names(list_dds_forecast_year)[i] <- year_i
}

# --------------------------------------------------------------------------------------------------- #
# COMBINE ALL ----
data_dds <- data.table::rbindlist(l = list_dds_forecast_year, use.names = T,fill = T,idcol = F)

# --------------------------------------------------------------------------------------------------- #
# PLOT DDS vs. COUNTS
data_ssmolt <- fishcastr::data_ssmolt

data_ssmolt_dds.merge <-Reduce(f = function(x,y){merge(x = x,
                                                       y = y,
                                                       by = "date")},
                               x = list(data_ssmolt,
                                        data_ssmolt_forecast_years,
                                        data_dds))

# subset to 1981
data_ssmolt_dds.merge_1981 <- data_ssmolt_dds.merge[data_ssmolt_dds.merge$salmonid_year >= 1981,]

# convert counts to percentages of run
data_ssmolt_dds.merge_1981$ssmolt_perc_run <- NA

for(i in list(unique(data_ssmolt_dds.merge_1981[["salmonid_year"]]))[[1]]){
  data_ssmolt_dds.merge_1981$ssmolt_perc_run[data_ssmolt_dds.merge_1981[["salmonid_year"]] == i] <- 100*((data_ssmolt_dds.merge_1981[["ssmolt"]][data_ssmolt_dds.merge_1981[["salmonid_year"]] == i])/sum(data_ssmolt_dds.merge_1981[["ssmolt"]][data_ssmolt_dds.merge_1981[["salmonid_year"]] == i]))
}
# for(i in unique(data_ssmolt_dds.merge_1981$salmonid_year)){
#
#   plot(data_ssmolt_dds.merge_1981$weighted_dds_water[data_ssmolt_dds.merge_1981$ssmolt_perc_run > 0],
#      data_ssmolt_dds.merge_1981$ssmolt_perc_run[data_ssmolt_dds.merge_1981$ssmolt_perc_run > 0], cex = 0.5, col = "black", xlim = c(100,2500), ylim = c(0,35),main = i)
#
# points(data_ssmolt_dds.merge_1981$weighted_dds_water[data_ssmolt_dds.merge_1981$ssmolt_perc_run > 0 & data_ssmolt_dds.merge_1981$salmonid_year == i],
#      data_ssmolt_dds.merge_1981$ssmolt_perc_run[data_ssmolt_dds.merge_1981$ssmolt_perc_run > 0 & data_ssmolt_dds.merge_1981$salmonid_year == i], cex = 0.5, col = "green3")
# }
#legend("topright",
#       legend = unique(data_ssmolt_dds.merge_1981$salmonid_year),
#       col = unique(data_ssmolt_dds.merge_1981$salmonid_year),cex = 0.5, bty = "n",pch = 1)

# identify multiplication factor
mult_factor <- 0.5001/min(data_ssmolt_dds.merge_1981$ssmolt_perc_run[data_ssmolt_dds.merge_1981$ssmolt_perc_run > 0],na.rm = TRUE)
data_ssmolt_dds.merge_1981$ssmolt_perc_run_int <- round(data_ssmolt_dds.merge_1981$ssmolt_perc_run*mult_factor)

# --------------------------------------------------------------------------------------------------- #
# FIT CURVE TO PHOTOPERIOD WEIGHTED DEGREE DAYS ----

data_list_years <- list(1981:1988,
                        1989:1998,
                        1999:2008,
                        2009:2018,
                        1981:1992)

dataset_list <- lapply(data_list_years,FUN = function(x){
  data_ssmolt_dds.merge_1981[data_ssmolt_dds.merge_1981$salmonid_year %in% x,
                             c("ssmolt_perc_run_int","weighted_dds_water")]
})

dataset_list_nameslist <- lapply(data_list_years,
                                 FUN = function(x){paste0(range(x)[1],"-",range(x)[2])})
names(dataset_list) <- dataset_list_nameslist

# Create vignette sub directories ----
dirName <- paste0(getwd(),"/vignettes/vignette_figures", "/FigS10", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = TRUE, mode = "0777")

dirName <- paste0(getwd(),"/vignettes/vignette_figures", "/FigS10/salmon", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = TRUE, mode = "0777")

dirName <- paste0(getwd(),"/vignettes/vignette_figures", "/FigS9", "/", sep = "",
                  collapse = NULL)
dir.create(dirName, showWarnings = TRUE, mode = "0777")

# # try different conditional distributions ----
# physio_negbin <-
#   physio_model_evaluation_exp_mod_Gauss(
#     species_count_lab = "ssmolt_perc_run_int",
#     x_variable_lab = "weighted_dds_water",
#     data_list = dataset_list,
#     error_distribution_name = "negative_binomial",
#     species_name = "Atlantic salmon",
#     file_path_plot = paste0(getwd(),'/vignette_figures/FigS9/negbin_FigS9a.png'),
#     file_path_resid_fitted_plot =  paste0(getwd(),'/vignette_figures/FigS10/salmon/negbin_FigS10b.png'),
#     file_path_dispersion_zeroinf_acf_plot =  paste0(getwd(),'/vignette_figures/FigS10/salmon/negbin_FigS10c.png'),
#     file_path_coef_profile_plot =  paste0(getwd(),'/vignette_figures/FigS10/salmon/negbin_FigS10d.png'),
#     start_values = list(c = 200, mu_exmg = 500, sigma_exmg = 50, lamb = 150, sigma_nb = 5),
#     lower_lims = list(c = 1, mu_exmg=300, sigma_exmg=30, lamb = 10, sigma_nb = 0.01),
#     ylims = c(-1200,1200),
#     xlims = c(250,1600)
#   )
#
# physio_quasi_Poisson <-
#   physio_model_evaluation_exp_mod_Gauss(
#     species_count_lab = "ssmolt_perc_run_int",
#     x_variable_lab = "weighted_dds_water",
#     data_list = dataset_list,
#     error_distribution_name = "quasi_Poisson",
#     species_name = "Atlantic salmon",
#     file_path_plot = paste0(getwd(),'/vignette_figures/FigS9/qPoiss_FigS9a.png'),
#     file_path_resid_fitted_plot = paste0(getwd(),'/vignette_figures/FigS10/salmon/qPoiss_FigS10b.png'),
#     file_path_dispersion_zeroinf_acf_plot = paste0(getwd(),'/vignette_figures/FigS10/salmon/qPoiss_FigS10c.png'),
#     file_path_coef_profile_plot = paste0(getwd(),'/vignette_figures/FigS10/salmon/qPoiss_FigS10d.png'),
#     start_values = list(c = 500, mu_exmg = 600, sigma_exmg = 50, lamb = 150, sigma_nb = 20),
#     lower_lims = list(c = 100, mu_exmg=300, sigma_exmg=30, lamb = 5, sigma_nb = 2),
#     ylims = c(-1200,1200),
#     xlims = c(250,1600)
#   )
#
physio_generalised_Poisson <-
  fishcastr::physio_model_evaluation_exp_mod_Gauss(
    species_count_lab = "ssmolt_perc_run_int",
    x_variable_lab = "weighted_dds_water",
    data_list = dataset_list,
    error_distribution_name = "generalised_Poisson",
    species_name = "Atlantic salmon",
    file_path_plot = paste0(getwd(),'/vignettes/vignette_figures/FigS9/genpois_FigS9a.png'),
    file_path_resid_fitted_plot = paste0(getwd(),'/vignettes/vignette_figures/FigS10/salmon/genpois_FigS10b.png'),
    file_path_dispersion_zeroinf_acf_plot = paste0(getwd(),'/vignettes/vignette_figures/FigS10/salmon/genpois_FigS10c.png'),
    file_path_coef_profile_plot = paste0(getwd(),'/vignettes/vignette_figures/FigS10/salmon/genpois_FigS10d.png'),
    start_values = list(c = 200, mu_exmg = 500,sigma_exmg = 50, lamb = 150,phi = 5),
    lower_lims = list(c = 1, mu_exmg=300, sigma_exmg=30, lamb = 10, phi = 0.01),
    ylims = c(-1800,1800),
    xlims = c(250,1600)
  )

# assign physio model to new object.
model_physio_expmG_salmon <- list("data" = physio_generalised_Poisson[["mod_physio_expmG"]]@data,
                                  "coefs" = bbmle::coef(physio_generalised_Poisson[["mod_physio_expmG"]]),
                                  "vcov_mat" = bbmle::vcov(physio_generalised_Poisson[["mod_physio_expmG"]]))

# export fitted model coefficients etc.,
usethis::use_data(model_physio_expmG_salmon, overwrite = TRUE)
