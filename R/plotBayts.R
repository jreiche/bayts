#' @title Plot original time series including indicated and detected changes (2 time series)

#' @description Plot original time series including indicated and detected changes. T.flagged and T.confirmed are plotted

#' @author Johannes Reiche (Wageningen University)

#' @param bayts bayts time series data frame
#' @param labL list of y-axis lables 
#' @param ylimL list of the y-axis limits (x1, x2) for time series 2. The default value is NULL, indicating that the ylim are set to the min and max value of time series 2
#' @param colL list of colour for time series
#' @param xlim range of x-axis value; default is the range of the time series
#' @param start abline for e.g. showing the start of the monitoring period

#' @export 

plotBayts <- function(bayts, labL=list(),ylimL=list(), colL=list(), xlim=NULL, start=NULL){

  # get output variables
  change.flagged = index(bayts[min(which(bayts$Flag=="Change"))])
  change.confirmed = index(bayts[max(which(bayts$Flag=="Change"))])
  flag = index(bayts[min(which(bayts$Flag=="Flag"))])
  oldflag = index(bayts[which(bayts$Flag=="oldFlag")])
  vchange = na.omit(bayts$Flag[which(bayts$Flag=="Change")])
  vflag = na.omit(bayts$Flag[which(bayts$Flag=="Flag")])
  
    
  # create bfastts for individual ts and create list of bfastts for a maximum of 10 ts
  index(bayts)[which(as.character(duplicated(as.character(time2date(index(bayts$ts1)))))==TRUE)]<-index(bayts)[which(as.character(duplicated(as.character(time2date(index(bayts$ts1)))))==TRUE)]+0.001   # increase by 1 day in case of duplicated dates
  ts1 <- bfastts(as.double(bayts$ts1),time2date(index(bayts$ts1)),type="irregular")
  tsL <- list(ts1)
  if (ncol(bayts)>4){tsL <- list(tsL[[1]],bfastts(as.double(bayts$ts2),time2date(index(bayts$ts2)),type="irregular"))} 
  if (ncol(bayts)>5){tsL <- list(tsL[[1]],tsL[[2]],bfastts(as.double(bayts$ts3),time2date(index(bayts$ts3)),type="irregular"))} 
  if (ncol(bayts)>6){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],bfastts(as.double(bayts$ts4),time2date(index(bayts$ts4)),type="irregular"))} 
  if (ncol(bayts)>7){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],tsL[[4]],bfastts(as.double(bayts$ts5),time2date(index(bayts$ts5)),type="irregular"))} 
  if (ncol(bayts)>8){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],tsL[[4]],tsL[[5]],bfastts(as.double(bayts$ts6),time2date(index(bayts$ts6)),type="irregular"))} 
  if (ncol(bayts)>9){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],tsL[[4]],tsL[[5]],tsL[[6]],bfastts(as.double(bayts$ts7),time2date(index(bayts$ts7)),type="irregular"))} 
  if (ncol(bayts)>10){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],tsL[[4]],tsL[[5]],tsL[[6]],tsL[[7]],bfastts(as.double(bayts$ts8),time2date(index(bayts$ts8)),type="irregular"))} 
  if (ncol(bayts)>11){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],tsL[[4]],tsL[[5]],tsL[[6]],tsL[[7]],tsL[[8]],bfastts(as.double(bayts$ts9),time2date(index(bayts$ts9)),type="irregular"))} 
  if (ncol(bayts)>12){tsL <- list(tsL[[1]],tsL[[2]],tsL[[3]],tsL[[4]],tsL[[5]],tsL[[6]],tsL[[7]],tsL[[8]],tsL[[9]],bfastts(as.double(bayts$ts10),time2date(index(bayts$ts10)),type="irregular"))} 
  
  # plot multiple ts
  plotts(tsL=tsL,labL=labL,ylimL=ylimL,colL=colL,xlim=xlim)
  
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