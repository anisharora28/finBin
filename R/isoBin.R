#' isobin
#'
#' This function performs supervised binning especially for building credit risk scorecards.
#' @param  R object
#' @return The mode of \code{x}  generates and object containing the necessary info and utilities for binning.
#' @keywords binning
#' @export
#' @examples
#' myNumbers <- c(7,6,6,10,6,6,7,3,7,14,20,7)
#' myMode(myNumbers)
#======================================== Isobin function=================================

isoBin <- function(data, y, x) {
  d1 <- data[c(y, x)]
  d2 <- d1[!is.na(d1[x]), ]
  #c <- cor(d2[, 2], d2[, 1], method = "spearman", use = "everything")
  cforsm <- cor(d2[, 2], d2[, 1], method = "spearman", use = "complete.obs")
  #print(c)
  if(!is.na(cforsm)){
    reg <- isoreg(d2[, 2], cforsm / abs(cforsm) * d2[, 1])
    k <- knots(as.stepfun(reg))
    sminitial <-smbinning.custom(d1, y, x, k)
    #print(sm1)
    if(is.list(sminitial)){
      cfirst <- subset(sminitial$ivtable, subset = CntGood * CntBad > 0, select = Cutpoint)
      #print(c1)
      csecond <- suppressWarnings(as.numeric(unlist(strsplit(cfirst$Cutpoint, " "))))
      cthird <- csecond[!is.na(csecond)]
    }
    return(smbinning.custom(d1, y, x, cthird[-length(cthird)]))
  }

}





