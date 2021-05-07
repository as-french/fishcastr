#' Convert a grid object to a data.frame
#'
#' This function takes a list of grid objects (e.g., multiple meteorological
#' variables downloaded and interpolated from climate reanalysis or multi-member
#' seasonal climate forecast) and extracts variable information and concurrent
#' dates. Optionally export output to a .dat file
#'
#' @param grid_obj A list of grids (can be single grid).
#' @param output_dir A file path
#' @param output_folder_name Output parent folder name (to be created within
#'   specified path)
#' @param ... additional arguments (e.g., header == FALSE to pass to
#'   write.table)
#' @return A data.frame (if single member). A list of data.frames if
#'   multi-member grid.
#' @examples
#' \dontrun{
#' # single member reanalysis
#' grid_sub <- lapply(fishcastr::grid_ERA5_1979_2019_Jan_bc,
#'                    function(x){
#'                    transformeR::subsetGrid(x,
#'                                              years = 1996:1998)})
#' convert_grid_to_dataframe(grid_obj = grid_sub,
#'                           output_dir = paste0(getwd(),
#'                                               "/inst/extdata/ERA5_archive_1979_2019"))
#'
#' # multi-member seasonal forecast
#' grid_sub_mm <- lapply(fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc,
#'                    function(x){
#'                      transformeR::subsetGrid(x,
#'                                              years = 1996:1998)})
#' convert_grid_to_dataframe(grid_obj = grid_sub_mm,
#'                           output_dir = paste0(getwd(),
#'                                               "/inst/extdata/SEAS5_archive_1993_2019"))
#' # single member reanalysis
#' grid_sub <- lapply(fishcastr::grid_ERA5_1979_2019_Jan_bc,
#'                    function(x){
#'                    transformeR::subsetGrid(x,
#'                                              years = 1996:1998)})
#' convert_grid_to_dataframe(grid_obj = grid_sub)
#'
#' # multi-member seasonal forecast
#' grid_sub_mm <- lapply(fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc,
#'                    function(x){
#'                      transformeR::subsetGrid(x,
#'                                              years = 1996:1998)})
#' convert_grid_to_dataframe(grid_obj = grid_sub_mm)
#' }
#' @export
convert_grid_to_dataframe <- function(grid_obj,
                                      output_dir = NULL,
                                      output_folder_name = "output",
                                      ...){
  data_inp <- grid_obj
  # extract the data arrays of all variables from the list
  data <- lapply(data_inp, function(x) x[["Data"]])

# check data dimensions
dim_size <- attributes(grid_obj[[1]][["Data"]])$dim

# SINGLE MEMBER
if(length(dim_size) == 1){
  # Build data frame
  dates <- grid_obj[[1]]$Dates
  xycoords <- transformeR::getCoordinates(grid_obj[[1]])
  yymmdd <- as.Date(dates$start)
  hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")
  df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), data)

  if(!is.null(output_dir)){
    # Create directory and save df as .dat file
    dir.data <- paste0(output_dir,"/",output_folder_name,"/", sep = "", collapse = NULL)
    dir.create(dir.data, showWarnings = TRUE, recursive = TRUE, mode = "0777")
    utils::write.table(df, paste0(dir.data,"meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
  }
  return(df)
}

# MULTI MEMBER
if(length(dim_size) != 1){

  # bind attribute information
  dim_att <- do.call(rbind,attributes(grid_obj[[1]][["Data"]]))
  colnames(dim_att) <- attributes(grid_obj[[1]][["Data"]])$dimensions

  # number of iterations (i.e., columns in output dataframe)
  no_iter <- as.numeric(dim_att["dim","member"])

  # Collect some common metadata (e.g., from variable uas)
  dates <- grid_obj[[1]]$Dates

  # Give format to dates
  yymmdd <- as.Date(dates$start)
  hhmmss <- format(as.POSIXlt(dates$start), format = "%H:%M:%S")

  # Define metadata to generate the file name
  dir.data <- paste0(output_dir,"/",output_folder_name,"/", sep = "", collapse = NULL)
  dir.create(dir.data, showWarnings = TRUE, recursive = TRUE, mode = "0777")

  # Save a single file for each member
 mm_df <- lapply(1:no_iter, function(i){
    # Build data.frame for a single member
    single.member <- lapply(grid_obj, function(x) transformeR::subsetGrid(x, members = i))
    single.member <- lapply(single.member, function(x) x$Data)
    # Remove unwanted variables
    # data.frame creation
    df <- data.frame(c(list("dates1" = yymmdd, "dates2" = hhmmss)), single.member)
    if (i < 10) {
      member <- paste0("member0", i, sep = "", collapse = NULL)
    } else {
      member <- paste0("member", i, sep = "", collapse = NULL)
    }
    startTime <- format(as.POSIXlt(yymmdd[1]), format = "%Y%m%d")
    endTime <- format(utils::tail(as.POSIXlt(yymmdd), n = 1), format = "%Y%m%d")
    dirName <- paste0(dir.data, "/", member, "/", sep = "", collapse = NULL)
    if(!is.null(output_dir)){
    dir.create(dirName, showWarnings = TRUE, recursive = TRUE, mode = "0777")
      utils::write.table(df, paste0(dirName, "meteo_file.dat", sep = "", collapse = NULL), sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
    }
    return(df)
 })

  return(mm_df)
}


}

