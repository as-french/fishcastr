% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exp_mod_Gaussian_resp_inv.R
\name{exp_mod_Gaussian_resp_inv}
\alias{exp_mod_Gaussian_resp_inv}
\title{An (inverted) exponentially modified Gaussian curve.}
\usage{
exp_mod_Gaussian_resp_inv(x, c, mu_exmg, sigma_exmg, lamb)
}
\arguments{
\item{x}{A numeric vector.}

\item{c}{A scaling constant.}

\item{mu_exmg}{A constant that defines the position of the curve peak.}

\item{sigma_exmg}{A constant that defines the kurtosis of the curve.}

\item{lamb}{A constant that defines the rate of decline of the left tail of
the curve.}
}
\value{
y A numeric vector.
}
\description{
This function describes a unimodal curve defined by y values as a function of
x and four constants. For example, y might define a proxy measure for
physiological preparedness for marine endurance of diadromous fish migrating
into sea water following physiological transformation (e.g., silvering). The
(inverted) exponentially modified Gaussian is left skewed and is suitable for
response variables for which outliers occur at small values of x.
}
