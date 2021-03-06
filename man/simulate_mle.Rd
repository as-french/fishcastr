% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_mle.R
\name{simulate_mle}
\alias{simulate_mle}
\title{Generate simulated values from bbmle::mle2 model}
\usage{
simulate_mle(
  fittedModel,
  n,
  count_lab,
  sim_seed,
  newdata = NULL,
  fittedModel_name = NULL,
  fittedModel_curve_name,
  ...
)
}
\arguments{
\item{fittedModel}{A bbmle::mle2 object.}

\item{n}{Number of simulations of mle2 object.}

\item{count_lab}{Name of response variable.}

\item{sim_seed}{A random seed.}

\item{newdata}{Default NULL. Experimental.}

\item{fittedModel_name}{Default NULL. Experimental.}

\item{fittedModel_curve_name}{Character string currently "Cauchy",
"exp_mod_Gauss" or exp_mod_Gauss_inv.}

\item{...}{Other arguments to nested functions.}
}
\value{
A matrix of simulations.
}
\description{
This function generates simulations from a fitted bbmle::mle2
(Bolker & R Core Team 2017) model object. Simulations are returned as a
matrix. The stochastic element of the simulations comes from randoms draws
from the conditional distribution of the response (e.g., generalised
Poisson distribution). However, mean predictions are generated by
simulation from a multivariate normal distribution, and 95\% confidence
intervals of this mean (i.e., population prediction intervals) are
generated within this function, but not currently returned.
}
\examples{
\dontrun{
simulate_mle(fittedModel = mle2_mod,
             n = 250,
             count_lab = "ssmolt",
             sim_seed = 123,
             fittedModel_name = NULL,
             newdata = NULL)
}
}
\references{
Bolker, B. & R Core Team. (2017). bbmle: Tools for General Maximum Likelihood
Estimation (R package version 1.0.20).
https://CRAN.R-project.org/package=bbmle
}
