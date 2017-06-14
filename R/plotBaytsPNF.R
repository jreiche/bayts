#' @title Plot non-forest probabillty (PNF) time series and indicate changes (from "bayts" time series frame)
#' 
#' @description Plot non-forest probabillty (PNF) time series and indicate changes (from "bayts" time series object)
#' T.flagged and T.confirmed are plotted

#' @author Johannes Reiche (Wageningen University)

#' @param bayts bayts time series data frame
#' @param lab y-axis lables 
#' @param ylim y-axis limits. The default value is NULL, indicating that the ylim are set to the min and max value of the ts
#' @param col ts colour
#' @param start abline for e.g. showing the start of the monitoring period

#' @export 


plotBaytsPNF <- function(bayts,lab="PNF",ylim=NULL,col="black",start=NULL){
  
  #get output variables
  change.flagged = index(bayts[min(which(bayts$Flag=="Change"))])
  #change.flagged.PChange = bayts[min(which(bayts$Flag=="Flag"))]$PChange
  change.confirmed = index(bayts[max(which(bayts$Flag=="Change"))])
  #change.confirmed.PChange = bayts[max(which(bayts$Flag=="Change"))]$PChange
  flag = index(bayts[min(which(bayts$Flag=="Flag"))])
  oldflag = index(bayts[which(bayts$Flag=="oldFlag")])
  vchange = na.omit(bayts$Flag[which(bayts$Flag=="Change")])
  vflag = na.omit(bayts$Flag[which(bayts$Flag=="Flag")])
  
  # create bfastts
  index(bayts)[which(as.character(duplicated(as.character(time2date(index(bayts$ts1)))))==TRUE)]<-index(bayts)[which(as.character(duplicated(as.character(time2date(index(bayts$ts1)))))==TRUE)]+0.001 #increase by 1 day in case of duplicated dates
  PNF <- bfastts(as.double(bayts$PNF),time2date(index(bayts$PNF)),type="irregular")
  
  # plot PNF time series
  plotts(tsL=list(PNF),labL=list(lab),ylimL=list(ylim),colL=list(col))
  
  # plot ablines and title
  if(!is.na(flag)==TRUE){
    if(!is.null(start)){abline(v=start,col='black',add=TRUE)}
    abline(v=flag,col='red',add=TRUE,lty='dashed')
    abline(v=oldflag,col='black',add=TRUE,lty='dashed') 
    title(paste(time2date(max(index(bayts)),format = "%Y.%j"), " (Tflagged=",time2date(min(index(vflag)),format = "%Y.%j"), ")  [DOY]", sep = ""), cex.main = 1.05)
  } else if(!is.na(change.confirmed)==TRUE){
    if(!is.null(start)){abline(v=start,col='black',add=TRUE)}
    abline(v=change.flagged,col='red',lty='dashed')
    abline(v=change.confirmed,col='red')
    abline(v=oldflag,col='black',add=TRUE,lty='dashed')
    title(paste(time2date(max(index(bayts)),format = "%Y.%j"), " (T=",time2date(change.confirmed,format = "%Y.%j"), " | Tflagged=", time2date(min(index(vchange)),format = "%Y.%j"), ")  [DOY]", sep = ""), cex.main = 1.05)
    } else {
    if(!is.null(start)){abline(v=start,col='black',add=TRUE)}
    abline(v=oldflag,col='black',add=TRUE,lty='dashed')
    title(paste(time2date(max(index(bayts))),format = "%Y.%j"),cex.main=1.05)
  }
}



