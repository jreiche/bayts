#' @title Plot multiple time series with different data ranges 

#' @description Plot multiple time series with different data ranges.

#' @author Johannes Reiche (Wageningen University)

#' @param tsL list of object(s) of class \code{\link{ts}}
#' @param ylimL list of objects(s) of y limits (x1, x2) for tsL. The default value is NULL, indicating that the ylim are set to the min and max value of ts1
#' @param labL list of objects(s) of title for tsL
#' @param colL list of objects(s) of colour for tsL
#' @param xlim range of x-axis value; default is the range of the time series
#' @param points add points (default = TRUE)
#' @param title plot title

#' @examples  
#'
#' # load example ts data (Sentinel-1 and Landsat NDVI time series; 09/2014 - 05/2016; Deforestation event in early 2016)
#' data(s1vv_lndvi_pixel)
#'
#' # create ts using bfastts (bfast package)
#' tsS <- bfastts(s1vv_obs,as.Date(s1vv_date),type=c("irregular"))
#' tsL <- bfastts(lndvi_obs,as.Date(lndvi_date),type=c("irregular"))
#'
#' # plot ts
#' plotts(tsL=list(tsL,tsS),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"))
#' plotts(tsL=list(tsL,tsS),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"),ylimL=list(c(0,1),c(-13,-6)))
#' 
#' @export 


plotts <- function(tsL=list(),ylimL=list(),labL=list(),colL=list(), xlim=NULL, points=TRUE, title="") {
  # number of ts
  l <- length(tsL)
  
  # set standard colours for 10 ts
  colset <- c("black","blue","darkgreen","red","darkorange","deeppink","darkred","yellow","lightgreen","darkgrey")
  labset <- c("ts1","ts2","ts3","ts4","ts5","ts6","ts7","ts8","ts9","ts10")
  
  # set par for the plot
  if(l==1){
    par(mar=c(5, 4, 4, 2))
  } else {
    par(mar=c(5, 4, 4, (2+(l-1)*4)))
  }

  for(i in 1:l){
    # get ts
    zts <- na.omit(as.zoo(tsL[[i]]))
    # check for empty ts
    if (length(zts[!is.na(zts)])>0) {
      # set y-data range 
      if (!is.null(ylimL)){
        if (length(ylimL)>=i){
          # a - use defined range  
          ylim <- ylimL[[i]]
        } else {
          ylim = c(range(zts))
        } 
      } else {
        # b - derive from data range (in case of an undefined range)
        ylim = c(range(zts))
      } 
      
      # define colours
      if (!is.null(colL)){
        if (length(colL)>=i){
          col <- colL[[i]]
        } else {
          col <- colset[i]
        } 
      } else {
        col <- colset[i]
      } 
      
      # define y axis lables
      if (!is.null(labL)){
        if (length(labL)>=i){
          lab <- labL[[i]]
        } else {
          lab <- labset[i]
        } 
      } else {
        lab <- labset[i]
      } 
      
      # define x axis range
      if (i>1) {par(new=T)}
      if (is.null(xlim)) {
        xlim = time(tsL[[1]])
      }
      
      # plot ts
      p <- plot(zts,xlim=range(xlim),axes=F, ylim=ylim, xlab="", ylab="",type="l",col=col,bty="n")
      box(lwd=0.5) 
      if(points==TRUE) {p <- p + points(zts,xlim=range(xlim),ylim=ylim, pch = 19,cex=0.5,col=col)}
      if (i==1){
        p <- axis(2, ylim=ylim,col=col,lwd=1,col.axis=col,line=0.5)
        mtext(2,text=lab,line=2.5)
        axis(1,pretty(range(xlim),10))
        mtext(3,text=paste(title,sep=""),line=1)
      } else {
        p <- axis(4, ylim=ylim,col=col,lwd=1,col.axis=col,line=(0.5+((i-2)*4)))
        mtext(4,text=lab,line=(2.5+((i-2)*4)),col=col)
      }
    }
  }
}
