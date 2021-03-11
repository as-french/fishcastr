## code to prepare `data_Feeagh2m_temp` dataset
data_Feeagh2m_temp <- read.csv(file = paste0(getwd(),"/inst/extdata/daily_Feeagh_2m_temp_2004_2019.csv"),
                               header = TRUE,
                               colClasses = c("date" = "Date"),
                               stringsAsFactors = TRUE)
usethis::use_data(data_Feeagh2m_temp, overwrite = TRUE)
