#' A function to subset stratified fish census data into lists for warm-up of
#' hydrologic or water temperature models
#'
#' @description This function takes an input data.frame with at least date and
#'   forecast year columns. This function is applicable to cases where count
#'   data (e.g., migration runs) are stratified into separate years, but
#'   antecedent environmental conditions may extend backwards into the previous
#'   forecast year. For example, hydrologic conditions in spring 1981 are
#'   appropriately modelled using a warm-up period that extends backwards into
#'   1980 or even 1979. Likewise, in relation to European eels, migration runs
#'   are stratified in years that run from ~May to April, but antecedent thermal
#'   conditions (e.g., degree days) might influence the onset and rate of
#'   silvering transition (e.g., a run starting in 1981 may be influenced by
#'   thermal conditions experienced since winter solstice 1980). The current
#'   default here is to start warm-ups from winter solstice; see examples for
#'   years that start on winter solstice (i.e., salmonids in Burrishoole) and
#'   forecast years that start post winter solstice (i.e, eels in Burrishoole).
#'
#' @param water_temp A numeric vector
#' @param photoper A numeric vector.
#' @param forecast_years A numeric vector.
#' @param forecast_yday A numeric vector.
#' @param dates A date vector.
#' @param no_years_warmup An integer value.
#' @param warmup_from_solstice Default TRUE (no other options at this time)
#' @param delete_op_year Remove operational year data. Default is TRUE as the
#'   first use of this function in forecasting workflow is to calibrate a model
#'   using historic data.
#' @return A two column data.frame with headers: "date" and "pwdds".
#' @examples
#' \dontrun{
#' # LOAD MET DATA ----
#' met_data <- fishcastr::grid_ERA5_1979_2019_Jan_bc
#' met <-
#'   fishcastr::convert_grid_to_dataframe(grid_obj = met_data)[, -2]
#' names(met)[which(names(met) == "dates1")] <-
#'   "date"
#'
#' # RUN AIR TO WATER TEMP MODEL -----
#' data_water_temp <- data.frame(
#'    "date" = met$date,
#'    "Tw_mod" = fishcastr::air_to_water_model(
#'     Ta = met$tas,
#'     Yday = lubridate::yday(met$date),
#'     A = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$A,
#'     ac = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$ac,
#'     b = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$b,
#'     B = fishcastr::air_to_water_Feeagh_params_ERA5_bcc$B
#'   )
#' )
#'
#' # STRATIFY DATA BY FORECAST YEAR (RETAIN PHOTOPERIOD) ----
#' data_ssmolt_forecast_years <-
#'   fishcastr::bio_year_photoper(
#'     dates = data_water_temp$date,
#'     latitude = 53.932458,
#'     longitude = -9.575556,
#'     retain_photoper = TRUE,
#'     start_previous_year = TRUE,
#'     shortest_day = TRUE,
#'     yday_head = "salmonid_yday",
#'     bio_year_head = "salmonid_year"
#'   )
#'
#' # CALCULATE PHOTOPERIOD WEIGHTED DEGREE DAYS ----
#'   convert_stratified_census_data_to_model_warmup_list(
#'     water_temp = data_water_temp$Tw_mod,
#'     photoper = data_ssmolt_forecast_years$photoper,
#'     forecast_years = data_ssmolt_forecast_years$salmonid_year,
#'     forecast_yday = data_ssmolt_forecast_years$salmonid_yday,
#'     dates = data_ssmolt_forecast_years$date,
#'     no_years_warmup = 0
#'   )
#' }
#' @export
convert_stratified_census_data_to_model_warmup_list <- function(water_temp,
                                                                photoper,
                                                                forecast_years,
                                                                forecast_yday,
                                                                dates,
                                                                no_years_warmup,
                                                                warmup_from_solstice = TRUE,
                                                                delete_op_year = TRUE){

  # WEIGHT TEMPERATURES (MULTIPLY TEMPS BY SCALED PHOTOPERIOD...) -------------------------------------
water_temp_weighted <- ifelse(water_temp <=0, 0, water_temp*(photoper/max(photoper)))

# calculate photoperiod weighted degree days from winter solstice
# LOCATE ALL WINTER SOLSTICE DATES AND END OF SUBSEQUENT FORECAST YEAR ----
unique_forecast_years <- unique(forecast_years)

# define matrix for year delineation by solstice
solstice_mat <- as.data.frame(matrix(ncol = 3, nrow = length(unique_forecast_years)))
colnames(solstice_mat) <- c("forecast_year", "date_sols", "date_end_year")
class(solstice_mat$date_sols) = "Date"
class(solstice_mat$date_end_year) = "Date"

for(i in 1:length(unique_forecast_years)){
  solstice_mat[i,"date_sols"] <- dates[forecast_years == unique_forecast_years[i]][which.min(photoper[forecast_years == unique_forecast_years[i]])]
  solstice_mat[i,"forecast_year"] <- unique_forecast_years[i+no_years_warmup]
  solstice_mat[i,"date_end_year"] <- dates[forecast_years == unique_forecast_years[i+no_years_warmup]][which.max(forecast_yday[forecast_years == unique_forecast_years[i+no_years_warmup]])]
}

# remove first year and operational year (operational years are dealt with separately)
if(delete_op_year == TRUE){
forecast_list_data <- solstice_mat[-c(1,nrow(solstice_mat)),]
}
if(delete_op_year == FALSE){
  forecast_list_data <- solstice_mat[-c(1),]
}
# --------------------------------------------------------------------------------------------------- #
# CREATE LIST OF NEW "FORECAST YEARS" ----
unique_forecast_years <- unique(forecast_list_data[["forecast_year"]])
df <- data.frame("date" = dates,
                 "forecast_year" = forecast_years,
                 "water_temp_weighted"= water_temp_weighted)

list_forecast_years <- lapply(1:length(unique_forecast_years),FUN = function(x){
  result <- df[dates >= as.Date(forecast_list_data[x,"date_sols"]) & dates <= as.Date(forecast_list_data[x,"date_end_year"]),c("date","forecast_year","water_temp_weighted")]
  return(result)
})
names(list_forecast_years) <- unique_forecast_years

# --------------------------------------------------------------------------------------------------- #
# CALCULATE DDS FOR EACH FORECAST YEAR (water from solstice) ----
list_dds <-lapply(list_forecast_years, function(x)
  cbind(x, weighted_dds_water = cumsum(x$water_temp_weighted)))

# --------------------------------------------------------------------------------------------------- #
# REMOVE LIST ELEMENT DATE ROWS OUTSIDE FORECAST YEAR AND SELECT ONLY DATE AND PWDDS ----
list_dds_forecast_year <- lapply(1:length(list_dds),FUN = function(x){
  year_i <- as.numeric(names(list_dds)[x])
  result <- list_dds[[x]][list_dds[[x]][["forecast_year"]] == year_i, c(1,4)]
  return(result)
})
names(list_dds_forecast_year) <- as.numeric(names(list_dds))

# --------------------------------------------------------------------------------------------------- #
# COMBINE ALL ----
data_dds <- data.table::rbindlist(l = list_dds_forecast_year, use.names = T,fill = T,idcol = F)

return(data_dds)
}
