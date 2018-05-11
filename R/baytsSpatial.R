#' @title Function to run bayts on mulitple raster bricks
#' 
#' @description Implements bayts function on multiple time series rasterBrick object(s). 
#' Time information is provided as an extra object and the time series can be regular or irregular. Information describing F and NF distributions is provided for each time series. See (\code{\link{bayts}}) for more details. 

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @author Johannes Reiche (Wageningen University)

#' @param bL list of raster bricks. Raster bricks need to have the same extent and spatial resolution.
#' @param datesL list of time vector of the format: "2014-10-07". 
#' @param pdfL list of "pdf" object(s) describing F and NF distributions (see \code{\link{calcPNF}}). 
#' "pdf" object: pdf[1] = pdf type F, pdf[2] = pdf type NF, pdf[3] and pdf[4] = pdf parameter describing F, pdf[5] and pdf[6] = pdf parameter describing NF. pdf types supported: Gaussian or Weibull.
#' @param modL list of modL - modulation of the time series observations. default=NULL
#' @param mask (raster) mask at which method is applied; default = NULL (method is applied to all pixel)
#' @param bwf block weighting function to truncate the NF probability; Default=c(0.1,0.9); (c(0,1) = no truncation) 
#' @param chi threshold of Pchange at which the change is confirmed; Default=0.5
#' @param PNFmin threshold of pNF above which the first observation is flagged; Default=0.5
#' @param start start date of monitoring period. Default=NULL (start of input time series).
#' @param end end date of monitoring period. Default=NULL (end of input time series)
#' @param pptype character. Type of preprocessing to be applied to individual time series vectors. The two options are 'irregular' and '16-days'. See \code{\link{bfastts}} for more details.
#' @param outfile output file
#' @param mc.cores numeric. number of cores to be used for the job. See \code{\link{mc.calc}} for more details (default = 1)

#' @return A rasterBrick with 5 layers: 
#' (1) flag: time at which unconfirmed change got flagged; 
#' (2) change.flagged: time at which confirmed change got flagged; 
#' (3) change.confirmed: time at which change is confirmed; 
#' (4) Pflag: Probabilty of change for unconfirmed flagged changes; 
#' (5) Pchange.confirmed: Probabilty of change for confirmed changes.

#' @examples 
#' #TBD

#' @export 


baytsSpatial <- function(bL = list(NULL,...), datesL=list(NULL,...), pdfL=list(NULL,...), modL=list(), mask=NULL, bwf=c(0.1, 0.9), chi=0.9, PNFmin=0.5, start=NULL, end=NULL, pptype='irregular', out_file=NULL, mc.cores=1)
{
  
  is.integer0 <- function(x)
  {
    is.integer(x) && length(x) == 0L
  }
  
  fun <- function(v) {
    if(!all(is.na(v))){
      if(is.null(mask)){
        l <- 0
        run <- TRUE
      } else if(!(is.na(v[1]))){
        run <- TRUE
        l <- 1
      } else {run <- FALSE}
    } else {run <- FALSE}
    
    if(run==TRUE){
      for (i in 1:length(datesL)) {
        d <- datesL[[i]]
        b <- v[l + 1:(l + length(d))]
        if(length(modL)>0){
          m <- modL[[i]]
          b <- b+m
        } 
        ts <- bfastts(b, d, type = "irregular")
        if (length(na.omit(as.zoo(ts)) > 0)) {
          if (exists("tsL2")) {
            tsL2 <- c(tsL2, list(ts))
          }
          else {
            tsL2 <- list(ts)
          }
        }
        l <- l + length(d)
      }
      
      bts <- try(baytsDD(tsL=tsL2,distNFL=distNFL,msdL=msdL,start=start,end=end,formula=formula,order=order,chi=chi, PNFmin=PNFmin, bwf = bwf))
      
      if(class(bts) == 'try-error') {
        res <- cbind(NA,NA,NA,NA,NA)
      } else {
        mts <- bts$bayts
        
        if (is.integer0(which(mts$Flag == "Change")) == FALSE) {
          change = index(mts[min(which(mts$Flag == "Change"))])
          change.confirmed = index(mts[max(which(mts$Flag == 
                                                   "Change"))])
          if (length(as.double(na.omit(mts$PChange[max(which(mts$Flag == "Change"))]))) > 0) {
            pchange = as.double(na.omit(mts$PChange[max(which(mts$Flag == "Change"))]))
          }
          else {
            pchange = NA
          }
        }
        else {
          change = NA
          change.confirmed = NA
          pchange = NA
        }
        if (is.integer0(which(mts$Flag == "Flag")) == FALSE) {
          flag = index(mts[min(which(mts$Flag == "Flag"))])
          if (length(as.double(na.omit(mts$PChange[max(which(mts$Flag == "Flag"))]))) > 0) {
            pflag = as.double(na.omit(mts$PChange[max(which(mts$Flag == "Flag"))]))
          }
          else {
            pflag = NA
          }
        }
        else {
          flag = NA
          pflag = NA
        }
        res <- cbind(flag,change,change.confirmed,pflag,pchange)
      }
    } else {res <- cbind(NA,NA,NA,NA,NA)}
    
    return(res) 
  }
  
  b <- bL[[1]]
  if(length(bL)>1){
    for(i in 2:length(bL)){
      b <- addLayer(b,bL[[i]])  
    }
  }
  
  x <- mc.calc(b, fun=fun, mc.cores=mc.cores)
  names(x) <- c("flag","change.flagged","change.confirmed","Pflag","Pchange.confirmed")
  
  if (!is.null(out_file)) {writeRaster(x,filename=out_file,overwrite=TRUE)}
  
  return(x)
}