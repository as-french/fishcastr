## code to prepare `data_met_auto_station_2005_2019` dataset goes here
data_met_auto_station_2005_2019 <- readr::read_csv(paste0(getwd(), "/inst/extdata/data_met_auto_station_2005_2019.csv"), col_types = readr::cols(date = readr::col_date(format = "%d/%m/%Y")))
names(data_met_auto_station_2005_2019) <- c("date","tas_auto","pr_auto","tas_min_auto","tas_max_auto")
usethis::use_data(data_met_auto_station_2005_2019, overwrite = TRUE)
