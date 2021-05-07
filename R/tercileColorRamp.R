#' Define colour ramp for tercile plot definition plot.
#'
#' @description This function takes a list of colours from RColorBrewer pallette
#'   (e.g., Reds, Greys and Blues) and creates a colour ramp required for a
#'   tercile defintion (image and image.plot based) plot.
#'
#' @param ncolors Number of colours.
#' @return A colour ramp.
#' @export
tercileColorRamp <- function(ncolors){
  ncolors <- ncolors-2
  tcols <- tercileColors()
  ##
  #tmp <- RColorBrewer::brewer.pal(8, tcols[1])
  #tmp_long <- colorRampPalette(tmp)(ncolors)

  tmp_long <- colorRampPalette(c("white",tcols[1]))(ncolors)
  rval <- data.frame(low=c(tmp_long[1],tmp_long[1],tmp_long), stringsAsFactors=F)

  #tmp <- RColorBrewer::brewer.pal(8, tcols[2])
  #tmp_long <- colorRampPalette(tmp)(ncolors)
  tmp_long <- colorRampPalette(c("white",tcols[2]))(ncolors)
  rval$middle <- c(tmp_long[1],tmp_long[1],tmp_long)

  #tmp <- RColorBrewer::brewer.pal(8, tcols[3])
  #tmp_long <- colorRampPalette(tmp)(ncolors)
  tmp_long <- colorRampPalette(c("white",tcols[3]))(ncolors)
  rval$high <- c(tmp_long[1],tmp_long[1],tmp_long)

  return(rval)
}
