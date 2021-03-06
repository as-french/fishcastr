% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rgenpoisson.R
\name{rgenpoisson}
\alias{rgenpoisson}
\title{Generate random deviates from a generalised Poisson distribution (Legrangian
Poisson)}
\usage{
rgenpoisson(mu, disp_param)
}
\arguments{
\item{mu}{A vector.}

\item{disp_param}{A numeric, dispersion parameter.}
}
\value{
A vector of random deviates.
}
\description{
This function generates random deviates when supplied with a
vector of conditional means and dispersion parameter. It is important to
note that the dispersion parameter returned by different models (e.g.,
those implemented using glmmTMB), may have different definitions. For
example, extracting dispersion parameter from genpois family models will
yield "phi-squared" dispersion parameter, which must be square rooted
before input into rgenpoisson (see simulate_glm function for example use
and https://rdrr.io/cran/glmmTMB/man/sigma.glmmTMB.html for details).
}
\examples{
set.seed(123)
plot(rgenpoisson(mu = fishcastr::exp_mod_Gaussian_resp(x = 1:365,
                                                  c = 60,
                                                  mu_exmg = 100,
                                                  sigma_exmg = 15,
                                                  lamb = 25),
            disp_param = 10))
}
