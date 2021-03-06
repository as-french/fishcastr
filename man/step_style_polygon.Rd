% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_style_polygon.R
\name{step_style_polygon}
\alias{step_style_polygon}
\title{Plot a polygon in base R consistent with line style "s"}
\usage{
step_style_polygon(xvals, y_vals_low, y_vals_up, col_alpha_rgb, ...)
}
\arguments{
\item{xvals}{A vector (e.g., ydays).}

\item{y_vals_low}{A vector (e.g., fish counts 0.975 quantile).}

\item{y_vals_up}{A vector (e.g., fish counts 0.025 quantile).}

\item{col_alpha_rgb}{An rgb colour}

\item{...}{Additional arguments to nested functions (e.g., polygon()).}
}
\value{
Plots a polygon on an existing plot.
}
\description{
This function can be used for overlaying fish counts vs. time
on top of long term averages.
}
