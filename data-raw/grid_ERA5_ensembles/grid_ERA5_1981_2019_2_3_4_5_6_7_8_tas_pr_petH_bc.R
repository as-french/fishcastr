# ----------------------------------------------------------------------------------------------- #
# A script to build an "ERA5 pseudo-ensemble". Each member of the ensemble is
# identical within years, but members differ among years. Each member for each
# year is built by substituting ERA5 reanalysis data into an existing SEAS5 grid
# (downloaded from Copernicus CDS or using University of Cantabria's climate4R
# bundle).
# ----------------------------------------------------------------------------------------------- #

# ----------------------------------------------------------------------------------------------- #
# LOAD BC ERA5 RDATA ) (needs to be to end of August 2019; i.e., the end of the target season)
#ERA5_data <- fishcastr::grid_ERA5_1979_2020_Jan_bc
ERA5_data <- readRDS(file = paste0(
          system.file("extdata", package = "fishcastr"),
          "/grid_ERA5_1979_2019_bc_CDS.rds"
        ))

# ----------------------------------------------------------------------------------------------- #
# LOAD SEAS5 RDATA

# SPRING
#SEAS5_data_spring_op <- fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_raw
SEAS5_data_spring_op <- readRDS(file = paste0(
  system.file("extdata", package = "fishcastr"),
  "/grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_raw_CDS.rds"
))


# subset to 2019
SEAS5_data_spring_tas <- transformeR::subsetGrid(grid = SEAS5_data_spring_op$tas,years = 1993:2019)
SEAS5_data_spring_pr <- transformeR::subsetGrid(grid = SEAS5_data_spring_op$pr,years = 1993:2019)
SEAS5_data_spring_petH <- transformeR::subsetGrid(grid = SEAS5_data_spring_op$petH,years = 1993:2019)

SEAS5_data_spring_93_19 <- list("tas" = SEAS5_data_spring_tas,
                          "pr" = SEAS5_data_spring_pr,
                          "petH" = SEAS5_data_spring_petH)

# format the dates
dates_93_19_start <- as.character(format(as.POSIXct(SEAS5_data_spring_93_19$tas$Dates$start, tz = "UTC"), format = "%Y-%m-%d", usetz = TRUE))

dates_93_19_end <- as.character(format(as.POSIXct(SEAS5_data_spring_93_19$tas$Dates$end, tz = "UTC"), format = "%Y-%m-%d", usetz = TRUE))

SEAS5_data_spring_93_19$tas$Dates$start <- dates_93_19_start
SEAS5_data_spring_93_19$tas$Dates$end <- dates_93_19_end
SEAS5_data_spring_93_19$pr$Dates$start <- dates_93_19_start
SEAS5_data_spring_93_19$pr$Dates$end <- dates_93_19_end
SEAS5_data_spring_93_19$petH$Dates$start <- dates_93_19_start
SEAS5_data_spring_93_19$petH$Dates$end <- dates_93_19_end

# ----------------------------------------------------------------------------------------------- #
# CREATE 1981 - 1992 SUBSET OF SEAS5 FOR REPLACEMENT WITH ERA5 (12 years; e.g., 1993 to 2004)
SEAS5_data_spring_tas_81_92 <- transformeR::subsetGrid(grid = SEAS5_data_spring_op$tas,years = 1993:2004)
SEAS5_data_spring_pr_81_92 <- transformeR::subsetGrid(grid = SEAS5_data_spring_op$pr,years = 1993:2004)
SEAS5_data_spring_petH_81_92 <- transformeR::subsetGrid(grid = SEAS5_data_spring_op$petH,years = 1993:2004)

SEAS5_data_spring_81_92 <- list("tas" = SEAS5_data_spring_tas_81_92,
                                "pr" = SEAS5_data_spring_pr_81_92,
                                "petH" = SEAS5_data_spring_petH_81_92)

# change dates to 81 to 92
dates_81_92_start <- as.character(format(as.POSIXct(SEAS5_data_spring_81_92$tas$Dates$start, tz = "UTC") - lubridate::years(12), format = "%Y-%m-%d", usetz = TRUE))

dates_81_92_end <- as.character(format(as.POSIXct(SEAS5_data_spring_81_92$tas$Dates$end, tz = "UTC") - lubridate::years(12), format = "%Y-%m-%d", usetz = TRUE))

SEAS5_data_spring_81_92$tas$Dates$start <- dates_81_92_start
SEAS5_data_spring_81_92$tas$Dates$end <- dates_81_92_end
SEAS5_data_spring_81_92$pr$Dates$start <- dates_81_92_start
SEAS5_data_spring_81_92$pr$Dates$end <- dates_81_92_end
SEAS5_data_spring_81_92$petH$Dates$start <- dates_81_92_start
SEAS5_data_spring_81_92$petH$Dates$end <- dates_81_92_end

# sort out initialization dates...
init_dates_81_92 <- as.character(format(as.POSIXct(SEAS5_data_spring_81_92[["tas"]][["InitializationDates"]][["Member_1"]], tz = "UTC") - lubridate::years(12), format = "%Y-%m-%d", usetz = TRUE))

InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- c(init_dates_81_92)})
names(InitializationDates) <- SEAS5_data_spring_81_92$tas$Members
SEAS5_data_spring_81_92$tas$InitializationDates <- InitializationDates
SEAS5_data_spring_81_92$pr$InitializationDates <- InitializationDates
SEAS5_data_spring_81_92$petH$InitializationDates <- InitializationDates

# append 1981 - 1992 to 1993 - 2019

# bind grids -----
SEAS5_data_spring_81_19.tas.list <- list("tas_81_92" = SEAS5_data_spring_81_92$tas,
                         "tas_93_19" = SEAS5_data_spring_93_19$tas)

SEAS5_data_spring_81_19.pr.list <- list("pr_81_92" = SEAS5_data_spring_81_92$pr,
                        "pr_93_19" = SEAS5_data_spring_93_19$pr)

SEAS5_data_spring_81_19.petH.list <- list("petH_81_92" = SEAS5_data_spring_81_92$petH,
                          "petH_93_19" = SEAS5_data_spring_93_19$petH)

SEAS5_data_spring_81_19.tas.list.bind <- do.call(transformeR::bindGrid, c(SEAS5_data_spring_81_19.tas.list, dimension = "time"))
SEAS5_data_spring_81_19.pr.list.bind <- do.call(transformeR::bindGrid, c(SEAS5_data_spring_81_19.pr.list, dimension = "time"))
SEAS5_data_spring_81_19.petH.list.bind <- do.call(transformeR::bindGrid, c(SEAS5_data_spring_81_19.petH.list, dimension = "time"))

SEAS5_data_spring_81_19 <- list("tas" = SEAS5_data_spring_81_19.tas.list.bind,
                     "pr" = SEAS5_data_spring_81_19.pr.list.bind,
                     "petH" = SEAS5_data_spring_81_19.petH.list.bind)

# again sort out initialisation dates
init_dates_81_19 <- c(SEAS5_data_spring_81_92$tas$InitializationDates$Member_1,SEAS5_data_spring_93_19$tas$InitializationDates$Member_1)

InitializationDates <- list()
InitializationDates <- lapply(1:25, function(x) {InitializationDates[[x]] <- init_dates_81_19})
names(InitializationDates) <- SEAS5_data_spring_81_19$tas$Members
SEAS5_data_spring_81_19$tas$InitializationDates <- InitializationDates
SEAS5_data_spring_81_19$pr$InitializationDates <- InitializationDates
SEAS5_data_spring_81_19$petH$InitializationDates <- InitializationDates

# ----------------------------------------------------------------------------------------------- #
# SUBSET ERA5 TO SAME DATES AS SEAS 5
obs.data <- ERA5_data
data <- SEAS5_data_spring_81_19
# Subset all datasets to the same Dates as the hindcast precipitation. Note that we compute daily accumulated
# precipitation, for this reason this variable has no value for the first day of every season.
if (sum(names(data)=="pr")>0){
  data <- lapply(1:length(data), function(x)  {transformeR::intersectGrid(data[[x]], data[[which(names(data)=="pr")]], type = "temporal", which.return = 1)})
  names(data) <- sapply(data, function(x) transformeR::getVarNames(x))
  obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data[[x]], type = "temporal", which.return = 1)})
  names(obs.data) <- sapply(obs.data, function(x) transformeR::getVarNames(x))
} else{
  obs.data <- lapply(1:length(obs.data), function(x)  {transformeR::intersectGrid(obs.data[[x]], data[[x]], type = "temporal", which.return = 1)})
  names(obs.data) <- sapply(obs.data, function(x) transformeR::getVarNames(x))
}

# ----------------------------------------------------------------------------------------------- #
# SUBSTITUTE ERA5 IN FOR SEAS5 FOR ALL MEMBERS (make sure to conserve class; e.g., numeric vector)
#View(unlist(obs.data$tas$Data))
#View(data$tas$Data)
for(i in 1:nrow(data$tas$Data)){
  data$tas$Data[i,] <- as.vector(unlist(obs.data$tas$Data))
}
for(i in 1:nrow(data$pr$Data)){
  data$pr$Data[i,] <- as.vector(unlist(obs.data$pr$Data))
}
for(i in 1:nrow(data$petH$Data)){
  data$petH$Data[i,] <- as.vector(unlist(obs.data$petH$Data))
}

# round
#data$tas$Data <- round(data$tas$Data,2)
#data$pr$Data <- round(data$pr$Data,2)
#data$petH$Data <- round(data$petH$Data,2)

# export BC ERA5 data
grid_ERA5_1981_2019_2_3_4_5_6_7_8_tas_pr_petH_bc <- data

usethis::use_data(grid_ERA5_1981_2019_2_3_4_5_6_7_8_tas_pr_petH_bc, overwrite = TRUE)

saveRDS(grid_ERA5_1981_2019_2_3_4_5_6_7_8_tas_pr_petH_bc,
        file = paste0(
          system.file("extdata", package = "fishcastr"),
          "/grid_ERA5_1981_2019_2_3_4_5_6_7_8_tas_pr_petH_bc_CDS.rds"
        ))
