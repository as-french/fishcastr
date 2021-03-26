#' ECMWF's ERA5 climate reanalysis at Burrishoole, Co Mayo, Ireland
#'
#' A grid containing daily mean air temperature at 2m, daily total precipitation
#' and potential evapotranspiration (calculated using the Hargreaves-Samani
#' equation). Data were obtained from the Copernicus Climate Data Store gridded
#' reanalysis dataset. Dates 1979-01-01 to 2019-06-30. These grid data are raw
#' (but rounded to 2 decimal places) and have not yet undergone posterior bias
#' correction using the *climate4R* bundle of packages.
#'
#' @format A list of three grids: \describe{
#'   \item{tas}{mean air temperature, in degrees Celsius}
#'   \item{pr}{total precipitation, in millimetres}
#'   \item{petH}{potential evapotranspiration, in millimetres}... }
#' @source
#' \url{https://doi.org/10.24381/cds.adbb2d47}
#'
"grid_ERA5_1979_2019_Jun_raw"
