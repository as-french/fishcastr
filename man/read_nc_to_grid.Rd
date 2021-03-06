% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_nc_to_grid.R
\name{read_nc_to_grid}
\alias{read_nc_to_grid}
\title{Read seasonal climate forecast netcdf data downloaded from Copernicus CDS
into a grid format}
\usage{
read_nc_to_grid(nc_folder_path, forecast_months, lat, lon, lead_month, members)
}
\arguments{
\item{nc_folder_path}{A character string.}

\item{forecast_months}{An integer.}

\item{lat}{Numeric length 2.}

\item{lon}{Numeric length 2.}

\item{lead_month}{Numeric.}

\item{members}{Numeric.}
}
\value{
A grid.
}
\description{
This function takes a folder of .nc files, extracts data arrays
from each file using the ncdf4 package and populates a template (empty)
grid provided with fishcastr. The output grid is compatible with the
climate4R bundle of R packages for processing climate data (e.g., for
calculating potential evapotranspiration using drought4R or aggregating
variables (e.g., daily means, max, min temperatures). This function
provides an alternative to using a netcdf editor to manipulate the raw
downloaded data from the CDS into a grid format that is readable using the
climate4R bundle in the first instance (e.g.,
https://www.unidata.ucar.edu/downloads/netcdf-java/).
}
\examples{
\dontrun{
nc_path <- paste0(
 system.file("inst", package = "fishcastr"),
 "/extdata/ECMWF_SEAS5/download_salmon_t2m/download_salmon_t2m_unzip"
)
nc_path <- paste0(
 system.file("inst", package = "fishcastr"),
 "/extdata/ECMWF_SEAS5/download_salmon_tp/download_salmon_tp_unzip"
)
out <- read_nc_to_grid(nc_folder_path = nc_path,
               forecast_months = 2:8,
               lat = c(53,54),
               lon = c(-10,-9),
               lead_month = 0,
               members = 1:25)
}
}
