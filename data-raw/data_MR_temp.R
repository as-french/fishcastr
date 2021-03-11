## code to prepare `data_MR_temp` dataset
data_MR_temp <- read.csv("inst/extdata/daily_MR_temperature_1970_2019.csv",
                            header = TRUE,
                            colClasses=c("date"="Date"),
                         stringsAsFactors = TRUE) # note that 01/10/2010 temperature was missing, so imputed 14 degrees Celsius to match nearby days...
usethis::use_data(data_MR_temp, overwrite = TRUE)
