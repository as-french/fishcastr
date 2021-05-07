#' Read seasonal climate forecast netcdf data downloaded from Copernicus CDS
#' into a grid format
#'
#' @description This function takes a folder of .nc files, extracts data arrays
#'   from each file using the ncdf4 package and populates a template (empty)
#'   grid provided with fishcastr. The output grid is compatible with the
#'   climate4R bundle of R packages for processing climate data (e.g., for
#'   calculating potential evapotranspiration using drought4R or aggregating
#'   variables (e.g., daily means, max, min temperatures). This function
#'   provides an alternative to using a netcdf editor to manipulate the raw
#'   downloaded data from the CDS into a grid format that is readable using the
#'   climate4R bundle in the first instance (e.g.,
#'   https://www.unidata.ucar.edu/downloads/netcdf-java/).
#'
#' @param nc_folder_path A character string.
#' @param forecast_months An integer.
#' @param lat Numeric length 2.
#' @param lon Numeric length 2.
#' @param lead_month Numeric.
#' @param members Numeric.
#' @return A grid.
#' @examples
#' \dontrun{
#'nc_path <- paste0(
#'  system.file("inst", package = "fishcastr"),
#'  "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip"
#')
#'nc_path <- paste0(
#'  system.file("inst", package = "fishcastr"),
#'  "/extdata/ECMWF_SEAS5/download_salmon_tp/download_salmon_tp_unzip"
#')
#'out <- read_nc_to_grid(nc_folder_path = nc_path,
#'                forecast_months = 2:8,
#'                lat = c(53,54),
#'                lon = c(-10,-9),
#'                lead_month = 0,
#'                members = 1:25)
#' }
#' @export
read_nc_to_grid <- function(nc_folder_path,
                            forecast_months,
                            lat,
                            lon,
                            lead_month,
                            members){

  # common data for all exported nc files
  lonLim <- lon
  latLim <- lat

  # members
  mem <- members
  max_mem <- max(members)

  # lead month
  lead.month <- lead_month

  # forecast months
  forecast.months <- forecast_months

  # how many nc files in folder
  dataset_list <- as.list(list.files(path = nc_folder_path,
                             pattern = "\\.nc$",
                             all.files = FALSE,
                             full.names = TRUE,
                             recursive = TRUE,
                             ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE))

  # extract data from nc files in dataset_list one by one and put into a new list in grid format
  imported_data_grid <- lapply(dataset_list,FUN = function(x){

    # read in nc file
    netcdf_x <- ncdf4::nc_open(x, write=TRUE)

    # var name
    var_nam <- attr(netcdf_x$var, "names")
    var_nam <- ifelse(var_nam == "t2m","tas",
                      ifelse(var_nam == "tp","pr",NULL))

    # extract variable data array
    if(var_nam == "tas"){
      nc_var <- ncdf4::ncvar_get(netcdf_x,varid = "t2m")
      nc_var <- nc_var - 273.15 # change units
    }
    if(var_nam == "pr"){
      nc_var <- ncdf4::ncvar_get(netcdf_x,varid = "tp")
      # change units to mm
      nc_var <- nc_var*1000
      time_dim_pr <- attributes(nc_var)[[1]][[4]]
      ###################################################################################
      # convert precipitation units values to mm per day (as currently in m accumulated)
      # de-accumulate all members and each dimension

      # note that the first value of accumulated fields (e.g., precipitation) is
      # always zero in cases of lead.month = 0,
      # (https://rdrr.io/github/SantanderMetGroup/loadeR/man/loadSeasonalForecast.html)

      #1,1
      deacumulated_pr11 <- lapply(1:max_mem, function(x){
        res <- c(nc_var[1,1,x,1], diff(nc_var[1,1,x,1:time_dim_pr]))
        return(res)
      })
      for(i in 1:max_mem){
        nc_var[1,1,i,1:time_dim_pr] <- deacumulated_pr11[[i]]
      }
      #1,2
      deacumulated_pr12 <- lapply(1:max_mem, function(x){
        res <- c(nc_var[1,2,x,1], diff(nc_var[1,2,x,1:time_dim_pr]))
        return(res)
      })
      for(i in 1:max_mem){
        nc_var[1,2,i,1:time_dim_pr] <- deacumulated_pr12[[i]]
      }
      #2,1
      deacumulated_pr21 <- lapply(1:max_mem, function(x){
        res <- c(nc_var[2,1,x,1], diff(nc_var[2,1,x,1:time_dim_pr]))
        return(res)
      })
      for(i in 1:max_mem){
        nc_var[2,1,i,1:time_dim_pr] <- deacumulated_pr21[[i]]
      }
      # 2,2
      deacumulated_pr22 <- lapply(1:max_mem, function(x){
        res <- c(nc_var[2,2,x,1], diff(nc_var[2,2,x,1:time_dim_pr]))
        return(res)
      })
      for(i in 1:max_mem){
        nc_var[2,2,i,1:time_dim_pr] <- deacumulated_pr22[[i]]
      }
      ###################################################################################

    }

    # leap year or not
    base_time <- strsplit(netcdf_x[["dim"]][["time"]]$units,split = c(" "))
    base_time[[1]][4] <- strsplit(base_time[[1]][4],split = c("[.]"))[[1]][1]

    import_time_stamps <- as.POSIXct(paste(base_time[[1]][3],
                                           base_time[[1]][4],
                                           sep = " "), tz = "GMT") + lubridate::hours(netcdf_x[["dim"]][["time"]]$vals)

    # close original nc file
    ncdf4::nc_close(nc = netcdf_x)

    #   format(as.POSIXct(paste(base_time[[1]][3], base_time[[1]][4], sep = " "), tz = "GMT"),
    #          format = "%Y-%m-%d %H:%M:%S %Z")
    year_x <- lubridate::year(min(import_time_stamps))
    is_leap_year <- lubridate::leap_year(year_x)

    # select template based on forecast months and leap year status
    if(is_leap_year == TRUE & min(forecast_months) == 2){
      template_grid = fishcastr::grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_leap_year
    }
    if(is_leap_year == FALSE & min(forecast_months) == 2){
      template_grid = fishcastr::grid_SEAS5_template_2_3_4_5_6_7_8_tas_pr_tasmin_tasmax_petH_non_leap_year
    }
    if(is_leap_year == TRUE & min(forecast_months) == 7){
      template_grid = fishcastr::grid_SEAS5_template_7_8_9_10_11_12_tas_pr_tasmin_tasmax_petH_leap_year
    }
    if(is_leap_year == FALSE & min(forecast_months) == 7){
      template_grid = fishcastr::grid_SEAS5_template_7_8_9_10_11_12_tas_pr_tasmin_tasmax_petH_non_leap_year
    }

    # extract Data attributes from the template array
    #   data_atts_template <- attributes(template_grid[[var_nam]][["Data"]]) # (member time lat long)

    # extract Data attributes from imported nc array (could maybe use
    # ncdf4::nc.conform.data here instead of assuming the order of dimensions
    # from CDS and ncdf4::ncvarget, but we'll keep for now)
    # data_atts_import <- attributes(nc_var) # (lon lat member time)
    # permute array to match climate4R dimension convention (member time lat long)

    nc_var_perm <- aperm(nc_var, c(3,4,2,1))
    attr(nc_var_perm,'dimensions') <- c("member","time", "lat","lon")

    # can now substitute array into template and update time stamps, initialisation dates etc.,
    # note difference between time stamps for tas and pr. pr is an accumulated
    # variable, so time start and end are different, whereas for tas time start
    # and end are identical as values are instantaneous.

    if(var_nam == "tas"){
      template_grid[[var_nam]][["Data"]] <- nc_var_perm
      template_grid[[var_nam]][["Dates"]][["start"]] <- as.character(import_time_stamps,
                                                                     format = "%Y-%m-%d %H:%M:%S %Z")
      template_grid[[var_nam]][["Dates"]][["end"]] <- as.character(import_time_stamps,
                                                                   format = "%Y-%m-%d %H:%M:%S %Z")
      # transformeR::getGridUnits(template_grid[[var_nam]])
      attr(template_grid[[var_nam]][["Variable"]], "units") <- "celsius"
    }

    if(var_nam == "pr"){
      template_grid[[var_nam]][["Data"]] <- nc_var_perm
      template_grid[[var_nam]][["Dates"]][["start"]] <- as.character(import_time_stamps - lubridate::days(1),
                                                                     format = "%Y-%m-%d %H:%M:%S %Z")
      template_grid[[var_nam]][["Dates"]][["end"]] <- as.character(import_time_stamps,
                                                                   format = "%Y-%m-%d %H:%M:%S %Z")
      attr(template_grid[[var_nam]][["Variable"]], "units") <- "millimetres"
    }

    # set initialisation dates
    initialization_dates <- as.character(lubridate::date(min(template_grid[[var_nam]][["Dates"]][["start"]])),
                                         format = "%Y-%m-%d %Z")
    initialization_dates_list <- lapply(1:max_mem,FUN = function(x){initialization_dates})
    names(initialization_dates_list) <- paste0("Member_",1:max_mem)
    template_grid[[var_nam]][["InitializationDates"]] <- initialization_dates_list

    # subset and extract newly substituted grid to forecast months
    template_grid_sub <- transformeR::subsetGrid(grid = template_grid[[var_nam]],
                                                 season = forecast_months,
                                                 members = 1:max_mem)
    return(template_grid_sub)

  })

  # bind grids by time
  imported_data_grid.bind <- do.call(transformeR::bindGrid,
                                     c(imported_data_grid, dimension = "time"))

  # bind initialisation dates (there will be a more efficient way to do this)
  init_dates <- lapply(imported_data_grid,function(y){
    y[["InitializationDates"]]
  })
  init_dates.cbind <- do.call(cbind,init_dates)
  init_dates.cbind_df <- as.data.frame(init_dates.cbind)
  init_dates_list <- lapply(1:nrow(init_dates.cbind_df),function(z){
    init_dates.cbind_df[z,]
  })
  init_dates_list_mem <- lapply(init_dates_list,function(i){
    unlist(do.call(cbind,i))
  })
  names(init_dates_list_mem) <- paste0("Member_",members)

  imported_data_grid.bind$InitializationDates <- init_dates_list_mem

  return(imported_data_grid.bind)
}
