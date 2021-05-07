#' Define colour ramp for tercile plot
#'
#' @description This function takes a list of three colours (e.g., red, yellow
#'   and blue) and creates three colour ramps from white to the specified
#'   colours required for a tercile (image and image.plot based) plot.
#'
#' @param ncolors Number of colours.
#' @param custom_cols A list of three colours in tercile order (low, middle,
#'   high); e.g., c("blue","grey","red").
#' @return A colour ramp.
#' @export
tercileColorRampCustom <- function(ncolors, custom_cols){
  #  ncolors <- ncolors-2
  #  tcols <- tercileColor()
  #  tmp <- RColorBrewer::brewer.pal(ncolors, tcols[1])
  #  rval <- data.frame(low=c(tmp[1],tmp[1],tmp), stringsAsFactors=F)
  #  tmp <- RColorBrewer::brewer.pal(ncolors, tcols[2])
  #  rval$middle <- c(tmp[1],tmp[1],tmp)
  #  tmp <- RColorBrewer::brewer.pal(ncolors, tcols[3])
  #  rval$high <- c(tmp[1],tmp[1],tmp)

  ncolors <- ncolors-2

  tmp_long <- colorRampPalette(c("white",custom_cols[1]))(ncolors)
  rval <- data.frame(low=c(tmp_long[1],tmp_long[1],tmp_long), stringsAsFactors=F)

  tmp_long <- colorRampPalette(c("white",custom_cols[2]))(ncolors)
  rval$middle <- c(tmp_long[1],tmp_long[1],tmp_long)

  tmp_long <- colorRampPalette(c("white",custom_cols[3]))(ncolors)
  rval$high <- c(tmp_long[1],tmp_long[1],tmp_long)

  return(rval)
}
