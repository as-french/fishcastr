% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc.R
\docType{data}
\name{grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc}
\alias{grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc}
\title{Bias adjusted ECMWF's SEAS seasonal climate forecast at Burrishoole, Co Mayo,
Ireland}
\format{
A list of three 25 member grids: \describe{
\item{tas}{mean air temperature, in degrees Celsius}
\item{pr}{total precipitation, in millimetres}
\item{petH}{potential evapotranspiration, in millimetres}... }
}
\source{
\url{http://meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_Seasonal_25Members_SFC.ncml}
\url{http://data.meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_forecast_Seasonal_51Members_SFC.ncml}
}
\usage{
grid_SEAS5_1993_2019_7_8_9_10_11_12_tas_pr_petH_bc
}
\description{
A 25 member grid containing bias adjusted predictions for daily mean air
temperature at 2m, daily total precipitation and potential evapotranspiration
(calculated using the Hargreaves-Samani equation). Data were obtained from
the University of Cantabria's archived 25 member SEAS5 re-forecasts
(1993-2016) and 51 member operational forecasts (2017 - 2019). The first 25
members of the archived operational forecasts were selected. Dates: July
to December between the period 1993-07-01 to 2019-12-31. These grid data are
posterior bias adjusted and rounded to 2 decimal places using leave-one-out
empirical quantile mapping bias adjustment method implemented in
downscaleR::biasCorrection.
}
\keyword{datasets}
