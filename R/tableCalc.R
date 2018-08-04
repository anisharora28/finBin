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
#===================================Calculateds binning table values WOE, IV etc.===========================
tableCalc<-function(x, repl_na_woe=FALSE) {


  temp<-names(x)
  temp[1]<-"var_bins"
  names(x)<-temp

  x %>% mutate(badP=round(bad/sum(bad)*100,2),
               goodP=round(good/sum(good)*100,2),
               totalP=round(total/sum(total)*100,2),
               badRate=round(bad/total*100,2),
               WOE=round(log(goodP)-log(badP),3),
               WOE=ifelse(is.finite(WOE),WOE,0),
               WOE=ifelse(is.na(var_bins) & repl_na_woe==TRUE, 0, WOE),
               IV_i=(goodP-badP)/100*WOE )
}
