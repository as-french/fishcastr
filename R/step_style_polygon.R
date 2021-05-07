#' Plot a polygon in base R consistent with line style "s"
#'
#' @description This function can be used for overlaying fish counts vs. time
#'   on top of long term averages.
#'
#' @param xvals A vector (e.g., ydays).
#' @param y_vals_low A vector (e.g., fish counts 0.975 quantile).
#' @param y_vals_up A vector (e.g., fish counts 0.025 quantile).
#' @param col_alpha_rgb An rgb colour
#' @param ... Additional arguments to nested functions (e.g., polygon()).
#' @return Plots a polygon on an existing plot.
#' @export
step_style_polygon <- function(xvals,
                               y_vals_low,
                               y_vals_up,
                               col_alpha_rgb,
                               ...){

  x_vals = rep(xvals, each = 2)

  if(length(y_vals_up) == length(xvals)){
    y_vals_up_new <- y_vals_up
    yvals_stagger_up = c(y_vals_up_new[1], rep(y_vals_up_new, each = 2))
    yvals_up = yvals_stagger_up[-length(yvals_stagger_up)]
  }

  if(length(y_vals_up) == 1){
    y_vals_up_new <- rep(y_vals_up, times = length(xvals))
    yvals_stagger_up = c(y_vals_up_new[1], rep(y_vals_up_new, each = 2))
    yvals_up = yvals_stagger_up[-length(yvals_stagger_up)]
  }

  if(length(y_vals_low) == length(xvals)){
    y_vals_low_new <- y_vals_low
    yvals_stagger_low = c(y_vals_low_new[1], rep(y_vals_low_new, each = 2))
    yvals_low = yvals_stagger_low[-length(yvals_stagger_low)]
  }

  if(length(y_vals_low) == 1){
    y_vals_low_new = rep(y_vals_low, times = length(xvals))
    yvals_stagger_low = c(y_vals_low_new[1], rep(y_vals_low_new, each = 2))
    yvals_low = yvals_stagger_low[-length(yvals_stagger_low)]
  }
# , border = col_alpha_rgb

  # polygon (step style)
  polygon(
    x = c(x_vals, rev(x_vals)),
    y = c(yvals_up, rev(yvals_low)),
    col = col_alpha_rgb, ...)
}
