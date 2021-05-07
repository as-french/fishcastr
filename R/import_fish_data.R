#' Import fish count data
#'
#' @description A function to import daily counts of downstream migrating
#'   diadromous fishes recorded at the Mill Race and Salmon Leap traps in
#'   Burrishoole, Co. Mayo, Ireland since 1970-01-01. The dataset contains
#'   counts of Atlantic salmon smolts, anadromous brown trout smolts (Sea trout
#'   smolts) and European silver eels. IMPORTANT - this function can only be run
#'   after fishcastr::download_fish_data() has been run.
#'
#' @param species A character string choice of salmon smolt "ssmolt", sea trout
#'   smolt "stsmolt" and silver eel "seel".
#' @return A two column data.frame "date" and chosen species label.
#' @source
#' \url{https://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.4343}
#' @examples
#' \dontrun{
#' data_ssmolt <- import_fish_data(species = "ssmolt")
#' }
#' @export
import_fish_data <- function(species){

  if(species == "ssmolt"){
    data_import <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),"/extdata/data_ssmolt.rds"))
    }

  if(species == "stsmolt"){
    data_import <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),"/extdata/data_stsmolt.rds"))
    }

  if(species == "seel"){
    data_import <- readRDS(file = paste0(system.file("inst", package = "fishcastr"),"/extdata/data_seel.rds"))
    }

  return(data_import)
}
