## code to prepare `data_ssmolt` dataset goes here
data_ssmolt <- read.csv("inst/extdata/daily_ssmolt_1970_2019.csv",
                            header = TRUE,
                            colClasses=c("date"="Date"))
usethis::use_data(data_ssmolt, overwrite = TRUE)
