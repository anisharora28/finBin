#' coarseBins
#'
#' This function performs supervised binning especially for building credit risk scorecards.
#' @param  R object
#' @return The mode of \code{x}  generates and object containing the necessary info and utilities for binning.
#' @keywords binning
#' @export
#' @examples
#' myNumbers <- c(7,6,6,10,6,6,7,3,7,14,20,7)
#' myMode(myNumbers)
#'
#==========================================Coarse binning====================================================
coarseBins<-function(x,woeMargin=0.01) {



  #neliels workaroaud jo pirmas kolonas nosaukums var mainities; japarraksta uz kollonu indeksiem, ta lai si nav vajadzigs
  temp_n<-names(x)
  temp_n[1]<-"var_bins"
  names(x)<-temp_n

  if(sum(is.na(x))>0) { warning("NAs detected!")}

  i<-2

  repeat{
    k<-i-1

    x$woeDiff<-append(Inf,abs(diff(x$WOE)))

    dTest<-sum(x$woeDiff<woeMargin)

    if(dTest==0) break()

    if(x$woeDiff[i]<woeMargin) {
      x$bad[k]<-sum(x$bad[k:i])
      x$good[k]<-sum(x$good[k:i])
      x$total[k]<-sum(x$total[k:i])

      x$var_bins[k]<-paste(x$var_bins[k],x$var_bins[i], sep=";")

      x<-x[-i,]

      x<-tableCalc(x)

      # i<-k

    } else {i<-i+1}
  }

  x$woeDiff<-NULL

  temp_n<-names(x)
  temp_n[1]<-"bins"
  names(x)<-temp_n

  return(x)

}
