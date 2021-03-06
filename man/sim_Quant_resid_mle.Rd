% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_Quant_resid_mle.R
\name{sim_Quant_resid_mle}
\alias{sim_Quant_resid_mle}
\title{Produce simulated residuals compatible with DHARMa package}
\usage{
sim_Quant_resid_mle(
  fittedModel,
  n,
  count_lab,
  sim_seed,
  fittedModel_name = NULL,
  newdata = NULL,
  fittedModel_curve_name,
  ...
)
}
\arguments{
\item{fittedModel}{A bbmle::mle2 object.}

\item{n}{Number of simulations of mle2 object.}

\item{count_lab}{Name of response variable.}

\item{sim_seed}{A random seed.}

\item{fittedModel_name}{Default NULL. Experimental.}

\item{newdata}{Default NULL. Experimental.}

\item{fittedModel_curve_name}{Character string currently "Cauchy",
"exp_mod_Gauss", or "exp_mod_Gauss_inv".}

\item{...}{Other arguments to nested functions.}
}
\value{
A createDHARMa simulated residual object.
}
\description{
This function produces simulated residuals from a fitted
bbmle::mle2 (Bolker & R Core Team 2017) model object compatible with DHARMa
(Hartig 2019) functions (e.g., testDispersion).
}
\examples{
\dontrun{
sim_Quant_resid_mle(fittedModel = mle2_mod,
                    n = 250,
                    count_lab = "ssmolt",
                    sim_seed = 123,
                    fittedModel_name = NULL,
                    newdata = NULL)
}

}
\references{
Hartig, F. (2019). DHARMa: Residual Diagnostics for Hierarchical (Multi-Level
/ Mixed) Regression Models (R package version 0.2.2).
https://CRAN.R-project.org/package=DHARMa
Bolker, B. & R Core Team. (2017). bbmle: Tools for General Maximum Likelihood
Estimation (R package version 1.0.20).
https://CRAN.R-project.org/package=bbmle
}
