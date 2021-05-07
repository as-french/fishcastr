#' Model parameters for GR4J lumped rainfall run-off model calibrated using
#' downscaled ERA5.
#'
#' @description A dataset containing four parameters for running the GR4J model
#'   for the Burrishole catchment, Ireland. Parameters are: (i) production store
#'   capacity (mm), (ii) inter-catchment exchange coefficient (mm/d), (iii)
#'   routing store capacity (mm), (iv) unit hydrograph time constant (d). A
#'   model for Burrishoole was calibrated using the airGR package, Irish EPA
#'   Lough Feeagh lake level data, a ratings curve fitted (using bbmle::mle2)
#'   using flow measurements taken by SK at the Marine Institute, Furnace,
#'   Ireland, statistically downscaled (bias corrected) ECMWF ERA5 reanalysis
#'   precipitation data, and estimated daily potential evapotranspiration using
#'   the Hargreaves-Samani equation in the drought4R package. Further details on
#'   calibration can be found within the EcoCountForecastR package in
#'   /data-raw/data_est_catchment_discharge.R.
#'
#' @format A data frame with 1 row and four columns: \describe{
#'   \item{prod.stor.cap_mm}{production store capacity (mm), a double}
#'   \item{inter.exch.coeff_mm.d}{inter-catchment exchange coefficient (mm/d), a
#'   double} \item{rout.stor.cap_mm}{routing store capacity (mm), a double}
#'   \item{unit.hyd.time.cons_d}{unit hydrograph time constant (d), a double}
#'   ... }
#' @source \url{https://www.epa.ie/hydronet/#32070}
#'
"GR4J_Burr_params_ERA5_bcc"
