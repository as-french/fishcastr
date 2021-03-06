% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download_Feeagh_water_temp.R
\name{download_Feeagh_water_temp}
\alias{download_Feeagh_water_temp}
\title{Download surface water temperature for Lough Feeagh, Burrishoole, Ireland}
\source{
\url{https://data.marine.ie/geonetwork/srv/eng/catalog.search#/metadata/ie.marine.data:dataset.3757}
}
\usage{
download_Feeagh_water_temp()
}
\value{
A message indicating successful or unsuccessful download.
}
\description{
A function to download and pre-process water temperature
recordings at 2m depth in Lough Feeagh, Burrishoole, Co. Mayo, Ireland. The
resulting .rds dataset is stored in the extdata folder in the package
directory and contains daily mean water temperature (at 2m depth) that has
been aggregated from 2 minutely records from the Automatic Water Quality
monitoring station (AWQMS) from 2004-01-01 to 2019-12-31.
}
\examples{
\dontrun{
download_Feeagh_water_temp()
}
}
