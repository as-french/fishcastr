#' Convert a climate multi-member grid to a nested list
#'
#' @description This function takes a multi-member grid object (e.g., as
#'   seasonal climate forecast grid) and converts it to a nested list, whereby
#'   each primary list element refers to a year and secondary list elements
#'   relate to members within years. Each member is a multivariable data.frame.
#'
#' @param multi_member_grid A multi-member grid.
#' @return A nested list of years and multi-variable data.frame members within
#'   years.
#' @examples
#' \dontrun{
#'  mm_grid <- fishcastr::grid_SEAS5_1993_2019_2_3_4_5_6_7_8_tas_pr_petH_bc
#'  mm_grid_sub <- lapply(mm_grid,FUN = function(x){
#'    transformeR::subsetGrid(grid = x,years = 1996:1998)})
#'  test <- convert_multi_member_grid_to_list(
#'    multi_member_grid = mm_grid_sub
#'  )
#'  }
#' @export
convert_multi_member_grid_to_list <- function(multi_member_grid){

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

    # organise list into individual years with nested lapply
    mm_sublist <- lapply(unique(lubridate::year(yymmdd)),FUN = function(j){
      sublist <- lapply(1:no_members, FUN = function(i){
        # Build data.frame for a single member
        single.member <- lapply(datatoexport, function(x) transformeR::subsetGrid(x, members = i,
                                                                                  years = j))
        dates <- single.member[[1]]$Dates
        yyyymmdd <- as.Date(dates$start)

        single.member <- lapply(single.member, function(x) x$Data)
        # data.frame creation
        df <- data.frame(c(list("date" = yyyymmdd)), single.member)
        return(df)
      })
      names(sublist) <- paste0("member_",1:25)
      return(sublist)
    })
    names(mm_sublist) <- unique(lubridate::year(yymmdd))
    return(mm_sublist)

}
