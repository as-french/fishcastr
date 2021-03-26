## code to prepare `data_met_manual_station_1959_2019` dataset goes here
data_met_manual_station_1959_2019 <- readr::read_csv(paste0(getwd(), "/inst/extdata/data_met_manual_station_1959_2019.csv"), col_types = readr::cols(date = readr::col_date(format = "%d/%m/%Y")))
names(data_met_manual_station_1959_2019) <- c("date","tas_manual","pr_manual","tas_min_manual","tas_max_manual")
usethis::use_data(data_met_manual_station_1959_2019, overwrite = TRUE)
