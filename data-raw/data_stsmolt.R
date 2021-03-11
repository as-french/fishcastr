## code to prepare `data_stsmolt` dataset goes here
data_stsmolt <- read.csv("inst/extdata/daily_stsmolt_1970_2019.csv",
                        header = TRUE,
                        colClasses=c("date"="Date"))
usethis::use_data(data_stsmolt, overwrite = TRUE)
