## code to prepare `data_seel` dataset goes here
data_seel <- read.csv("inst/extdata/daily_seel_1970_2019.csv",
                         header = TRUE,
                         colClasses=c("date"="Date"))
usethis::use_data(data_seel, overwrite = TRUE)
