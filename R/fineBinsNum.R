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

#==========================================Fine binning numeric====================================================
fineBinsNum<-function(x,target,var, varName=NA,nBins=500, minBadP=0.0001, mingoodP=0.0001, breaks=NA,repl_na_woe=FALSE) {

  nBins<-ifelse(nBins>500,500,nBins)

  x<-x[append(target, var)]

  names(x)<-c("target","var")

  if(length(unique(x$var))<2) {
    warning("Too few unique values! Returning NA ...")
    return(NA)}

  min_b<-round(sum(x$target)*minBadP,0)
  min_b<-ifelse(min_b<=0,1,min_b)

  min_g<-round((length(x$target)-sum(x$target))*mingoodP,0)
  min_g<-ifelse(min_g<=0,1,min_g)

  if(sum(is.na(breaks))==0) {

    breaks<-sort(breaks, decreasing = FALSE)

    if(min(breaks)>min(x[,2], na.rm = TRUE)) {breaks<-c(min(x[,2], na.rm = TRUE),breaks)}
    if(max(breaks)<max(x[,2], na.rm = TRUE)) {breaks<-c(breaks,max(x[,2], na.rm = TRUE))}

    breaks<-breaks[-which(breaks<=min(x[,2], na.rm = TRUE))]
    breaks<-breaks[-which(breaks>=max(x[,2], na.rm = TRUE))]

    breaks<-c(min(x[,2], na.rm = TRUE),breaks,max(x[,2], na.rm = TRUE))

    x$var_bins<-cut(x[,2],breaks,dig.lab=10,include.lowest = TRUE,right = TRUE,ordered_result=TRUE)
    x$var_bins<-addNA(x$var_bins)
    x$var_bins<-droplevels(x$var_bins)

  } else {

    breaks<-unique(x[,2])

    breaks<-breaks[!is.na(breaks)]

    #If group count is too large teh data is split using quantile function
    if(length(breaks)>nBins) {
      breaks<-quantile(x[,2],(0:nBins)/nBins,na.rm=TRUE) }

    breaks<-round(breaks,2)

    if(min(breaks)>min(x[,2],na.rm=TRUE)) {breaks<-c(min(x[,2],na.rm=TRUE),breaks)}
    if(max(breaks)<max(x[,2],na.rm=TRUE)) {breaks<-c(breaks,max(x[,2],na.rm=TRUE))}

    breaks<-unique(breaks)

    #If there has to be atleast two numerical values to define an interval
    if (length(breaks) < 2) {
      x$var_bins<-as.factor(x[,2])
      x$var_bins<-addNA(x$var_bins)
      x$var_bins<-droplevels(x$var_bins)
    } else {
      x$var_bins<-cut(x[,2],breaks,dig.lab=10,include.lowest = TRUE)
      x$var_bins<-addNA(x$var_bins)
      x$var_bins<-droplevels(x$var_bins) }
  }


  #print(test)

  x<-data.frame(x %>% group_by (var_bins) %>%
                  summarise(bad=sum(target),
                            good=length(target)-bad,
                            total=length(target)))

  x$var_bins <- as.character(x$var_bins)

  NaRow<-x[is.na(x$var_bins),]
  xt<-x[!is.na(x$var_bins),]

  xt$index<-seq(1,length(xt[,1]),by=1)

  xt$var_bins <- as.character(xt$var_bins)

  i<-1


  repeat {
    j<-i+1
    k<-i-1
    if(xt$index[i]<max(xt$index)) {
      if(xt$bad[i]<min_b | xt$good[i]<min_g) {
        xt$bad[j]<-sum(xt$bad[i:j])
        xt$good[j]<-sum(xt$good[i:j])
        xt$total[j]<-sum(xt$total[i:j])

        half1<-unlist(strsplit(xt$var_bins[i], ","))[1]
        half2<-unlist(strsplit(xt$var_bins[j], ","))[2]
        xt$var_bins[j]<-paste(half1,half2,sep=",")
        xt<-xt[-i,]
        i=i-1
      }
      i=i+1
    }else{

      if(xt$bad[i]<min_b | xt$good[i]<min_g) {
        xt$bad[k]<-sum(xt$bad[k:i])
        xt$good[k]<-sum(xt$good[k:i])
        xt$total[k]<-sum(xt$total[k:i])

        half1<-unlist(strsplit(xt$var_bins[k], ","))[1]
        half2<-unlist(strsplit(xt$var_bins[i], ","))[2]
        xt$var_bins[k]<-paste(half1,half2,sep=",")
        if(i>1) {xt<-xt[-i,]} #Ja paliek tikai viena rinda to vajag atstat.
      }
      xt$index<-NULL
      break()
    }
  }

  if(length(NaRow)>0) {
    xt<-rbind(xt,NaRow)
  }

  x<-tableCalc(xt, repl_na_woe)



  x$type<-"NUM"
  n<-names(x)
  n[1]<-"bins"
  names(x)<-n

  x$variable<-var
  if(is.na(varName)) {varName<-var}
  x$varName<-varName

  return (x)

}
