% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/grid_ERA5_1981_2019_7_8_9_10_11_12_tas_pr_petH_bc.R
\docType{data}
\name{grid_ERA5_1981_2019_7_8_9_10_11_12_tas_pr_petH_bc}
\alias{grid_ERA5_1981_2019_7_8_9_10_11_12_tas_pr_petH_bc}
\title{A "pseudo-ensemble" of bias adjusted ECMWF's ERA5 reanalysis at Burrishoole,
Co Mayo, Ireland}
\format{
A list of three 25 member grids: \describe{
\item{tas}{mean air temperature, in degrees Celsius}
\item{pr}{total precipitation, in millimetres}
\item{petH}{potential evapotranspiration, in millimetres}... }
}
\source{
\url{http://meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_Seasonal_25Members_SFC.ncml}
\url{http://data.meteo.unican.es/tds5/dodsC/Copernicus/SYSTEM5_ecmwf_forecast_Seasonal_51Members_SFC.ncml}
\url{https://doi.org/10.24381/cds.adbb2d47}
}
\usage{
grid_ERA5_1981_2019_7_8_9_10_11_12_tas_pr_petH_bc
}
\description{
A 25 member grid containing bias adjusted reanalysis data for
daily mean air temperature at 2m, daily total precipitation and potential
evapotranspiration (calculated using the Hargreaves-Samani equation). Here,
all members of the ensemble within years are identical, whereas member data
differs among years. As such, this "pseudo-ensemble" represents the a
"perfect" forecast. These data are intended for use as forcing data for
model chains for which chain outputs contain a stochastic element (e.g.,
random draws from a conditional distribution, such as those derived from
simulations of glmmTMB objects); in so doing, probabilistic predictions
under known conditions can be compared for consistency with observations.
The grid was build by substituting ERA5 data obtained from the Copernicus
Climate Data Store (CDS) with ECMWF SEAS5 data obtained from the the
University of Cantabria's archived 25 member SEAS5 re-forecasts (1993-2016)
and 51 member operational forecasts (2017 - 2019; Dates: July to December
between the period 1993-07-01 to 2019-12-31). Data are posterior bias
adjusted and rounded to 2 decimal places using leave-one-out empirical
quantile mapping bias adjustment method implemented in
downscaleR::biasCorrection. See
data-raw/grid_ERA5_ensembles/System5_seasonal_forecast_spring_ideal_81_19.R
}
\keyword{datasets}
