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
#'
#==========================================Fine binning Categorical====================================================
fineBinsCat<-function(x,target,var, varName=NA,nBins=50, minBadP=0.01, mingoodP=0.005, breaks=NA, repl_na_woe=FALSE) {

  nBins<-ifelse(nBins>500,500,nBins)

  x<-x[append(target, var)]

  names(x)<-c("target","var_bins")

  if(length(unique(x$var_bins))<2) {
    warning("Too few unique values! Returning NA ...")
    xs <- NA} else {

      min_b<-round(sum(x$target)*minBadP,0)
      min_b<-ifelse(min_b<=0,1,min_b)

      min_g<-round((length(x$target)-sum(x$target))*mingoodP,0)
      min_g<-ifelse(min_g<=0,1,min_g)

      x<-data.frame(x %>% group_by (var_bins) %>%
                      summarise(bad=sum(target),
                                good=length(target)-bad,
                                total=length(target),
                                drop_index=ifelse(bad<min_b | good < min_g,1,0)))

      #Createing "other" category if there are atleast 2 small groups.
      #Even if there is no bads or goods in the small group it will be left as it is.

      if(sum(x$drop_index)>1 ) {

        y<- x %>%
          filter(drop_index==1) %>%
          summarise_each(funs(sum), -var_bins)

        y<-cbind("Other",y)
        names(y)<-c("var_bins","bad","good","total","drop_index")

        x<-x %>% filter(drop_index==0)

        x<-rbind(x,y)
        rm(y)

      }

      x$drop_index<-NULL

      x<-x %>% arrange(bad/total)

      x<-tableCalc(x, repl_na_woe)
    }

  x$type<-"CAT"
  n<-names(x)
  n[1]<-"bins"
  names(x)<-n

  x$variable<-var
  if(is.na(varName)) {varName<-var}
  x$varName<-varName

  return (x)

}
