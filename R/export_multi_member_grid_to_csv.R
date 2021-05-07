#' Export a climate multi-member grid to a folder of csvs
#'
#' @description This function takes a multi-member grid object (e.g., as
#'   seasonal climate forecast) and converts it to a set of folders containing
#'   multivariable forecasts (one per member).
#'
#' @param multi_member_grid A Date.
#' @param name_study Character string to be used as folder name.
#' @param dataset_name Character string used to label sub folders (e.g., SEAS5).
#' @param dir.data Directory path for exported data.
#' @return A nested list of years and members within years.
#' @examples
#' \dontrun{
#' mm_grid <- fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc
#' export_multi_member_grid_to_csv(
#'   multi_member_grid = mm_grid,
#'   dataset_name = SEAS5,
#'   name_study = "Burrishoole",
#'   dir.data = system.file("vignettes", package = "fishcastr"),
#'   members = 1:25
#' )
#' }
#' @export
export_multi_member_grid_to_csv <- function(multi_member_grid,
                                            name_study,
                                            dataset_name,
                                            dir.data = NULL){

# how many members (assuming all grids have same number of members)
no_members <- lapply(multi_member_grid, function(x){length(x[["Members"]])})[[1]]

# Select the object to export (can be 'data.bc', 'data.bc.cross' or 'data')
datatoexport <- multi_member_grid

# Collect some common metadata (e.g. from variable uas)
dates <- datatoexport[[1]]$Dates
xycoords <- transformeR::getCoordinates(datatoexport[[1]])

# Give format to dates
yymmdd <- as.Date(dates$start)
hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")

# Define metadata to generate the file name
ClimateModelName <- dataset_name
ExperimentName <- "seasonal"
freq <- "day"

# Save a single file for each member
result <- lapply(1:no_members, FUN = function(i){
  # Build data.frame for a single member
  single.member <- lapply(datatoexport, function(x) transformeR::subsetGrid(x, members = i))
  single.member <- lapply(single.member, function(x) x$Data)
  # Remove unwanted variables
  single.member["rsds"] <- NULL
  single.member["rlds"] <- NULL
  # data.frame creation
  df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), single.member)
  if (i < 10) {
    member <- paste0("member0", i, sep = "", collapse = NULL)
  } else {
    member <- paste0("member", i, sep = "", collapse = NULL)
  }
  startTime <- format(as.POSIXlt(yymmdd[1]), format = "%Y%m%d")
  endTime <- format(tail(as.POSIXlt(yymmdd), n = 1), format = "%Y%m%d")
  dirName <- paste0(dir.data, name_study, "/CLIMATE/", name_study, "_", ClimateModelName, "_", ExperimentName, "_", member, "_", freq,"_", startTime, "-", endTime, "/", sep = "", collapse = NULL)
  dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  write.table(df, paste0(dirName, "meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
  return(df)
})
return(result)

}
