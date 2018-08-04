#' monobin
#'
#' This function performs supervised binning especially for building credit risk scorecards.
#' @param  R object
#' @return The mode of \code{x}  generates and object containing the necessary info and utilities for binning.
#' @keywords binning
#' @export
#' @examples
#' myNumbers <- c(7,6,6,10,6,6,7,3,7,14,20,7)
#' myMode(myNumbers)
#======================================== Monobin function=================================
# Inspired by statcompute
# https://statcompute.wordpress.com/2017/01/22/monotonic-binning-with-smbinning-package/

monoBin <- function(data, y, x, s = 3) {
  d1 <- data[c(y, x)]
  n <- min(20, nrow(unique(d1[x])))
  repeat {
    d1$bin <- Hmisc::cut2(d1[, x], g = n)
    d2 <- aggregate(d1[-s], d1[s], mean)
    c <- cor(d2[-1], method = "spearman")

    if(is.na(abs(c[1, 2]))) break
    else {
      if(abs(c[1, 2]) == 1 | n == 2) break
    }
    n <- n - 1

  }
  d3 <- aggregate(d1[-s], d1[s], max)
  #print(d3)
  cuts <- d3[-length(d3[, 3]), 3]
  #print(cuts)
  return(smbinning::smbinning.custom(d1, y, x, cuts))
}

