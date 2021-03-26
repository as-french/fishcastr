#' Measure length of longest stretch of NAs within a vector
#'
#' This function takes vectors and finds the length of the longest stretch of
#' NAs
#'
#' @param x A vector.
#' @return An integer.
#' @examples
#' x <- rnorm(n = 100, mean = 20, sd = 5)
#' x[c(10:20,40:50,70:95)] <- NA
#' longestNAstrech(x) # 100
#' @export
longestNAstrech <- function(x) {
  dat <- rle(is.na(x)) # identifies consecutive nas (TRUE) and non nas (FALSE)
  res <- max(dat$lengths[dat$values]) # finds the maximum length of just the nas (TRUEs)
  return(res)
}
