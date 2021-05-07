#' Define colour ramp for tercile plot
#'
#' @description This function takes a list of colours from RColorBrewer pallette
#'   (e.g., Reds, Greys and Blues) and creates a colour ramp required for a
#'   tercile (image and image.plot based) plot.
#'
#' @param ncolors Number of colours.
#' @return A colour ramp.
#' @export
tercileBrewerColorRamp <- function(ncolors){
#  suppressMessages(require(RColorBrewer))
#  ncolors <- ncolors-2
#  tcols <- tercileColor()
#  tmp <- RColorBrewer::brewer.pal(ncolors, tcols[1])
#  rval <- data.frame(low=c(tmp[1],tmp[1],tmp), stringsAsFactors=F)
#  tmp <- RColorBrewer::brewer.pal(ncolors, tcols[2])
#  rval$middle <- c(tmp[1],tmp[1],tmp)
#  tmp <- RColorBrewer::brewer.pal(ncolors, tcols[3])
#  rval$high <- c(tmp[1],tmp[1],tmp)

  ncolors <- ncolors-2
  tcols <- tercileColor()

  tmp_long <- colorRampPalette(c("white",tcols[1]))(ncolors)
  rval <- data.frame(low=c(tmp_long[1],tmp_long[1],tmp_long), stringsAsFactors=F)

  tmp_long <- colorRampPalette(c("white",tcols[2]))(ncolors)
  rval$middle <- c(tmp_long[1],tmp_long[1],tmp_long)

  tmp_long <- colorRampPalette(c("white",tcols[3]))(ncolors)
  rval$high <- c(tmp_long[1],tmp_long[1],tmp_long)

  return(rval)
}
