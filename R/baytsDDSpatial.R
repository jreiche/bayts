#' @title Function to run baytsDD on (mulitple) raster bricks
#' 
#' @description Implements baytsDD function on (multiple) time series rasterBrick object(s). 
#' @description (i) deseasonalising time series using harmonic model fitting , (ii) deriving F and NF pdfs in data driven way and (iii) run bayts to detect forest chang 
#' @description Method as described in Reiche et al., 2018 (Remote Sensing)

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @author Johannes Reiche (Wageningen University)

#' @param bL list of raster bricks. Raster bricks need to have the same extent and spatial resolution.
#' @param datesL list of time vector of the format: "2014-10-07". 
#' @param pdfL list of "pdf" object(s) describing F and NF distributions (see \code{\link{calcPNF}}). 
#' "pdf" object: pdf[1] = pdf type F, pdf[2] = pdf type NF, pdf[3] and pdf[4] = pdf parameter describing F, pdf[5] and pdf[6] = pdf parameter describing NF. pdf types supported: Gaussian or Weibull.
#' @param msdL list of msdl object(s) describing the modulation of the sd of F and NF sd(F),sd(NF),mean(NF) (e.g. 2,2,-4)
#' @param distL list of "distNF" object(s) describing the mean and sd of the NF distribution in case no data driven way to derive the NF distribution is wanted; default=NULL
#' @param modL list of modL - modulation of the time series observations. default=NULL
#' @param formulaL list of formula for the regression model. The default is response ~ trend + harmon, i.e., a linear trend and a harmonic season component. Other specifications are possible using all terms set up by bfastpp, i.e., season (seasonal pattern with dummy variables), lag (autoregressive terms), slag (seasonal autoregressive terms), or xreg (further covariates). See bfastpp for details.
#' @param orderL list of numeric. Order of the harmonic term, defaulting to 3.
#' @param mask (raster) mask at which method is applied; default = NULL (method is applied to all pixel)
#' @param start_history Start date of history period used to model the seasonality and derive F and NF PDFs. Default=NULL (start of input time series)
#' @param end_history End date of history period used to model the seasonality and derive F and NF PDFs. Default=NULL (Start of the monitoring period is used)
#' @param start start date of monitoring period. Default=NULL (start of input time series).
#' @param end end date of monitoring period. Default=NULL (end of input time series)
#' @param bwf block weighting function to truncate the NF probability; Default=c(0.1,0.9); (c(0,1) = no truncation) 
#' @param chi threshold of Pchange at which the change is confirmed; Default=0.5
#' @param PNFmin threshold of pNF above which the first observation is flagged; Default=0.5
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

baytsDDSpatial <- function(bL = list(NULL, ...), datesL = list(NULL, ...), modL = list(), msdL=list(), distNFL=list(), formulaL=list(), orderL=list(), mask=NULL, start_history = NULL, end_history = NULL, start, end = NULL, chi=0.9, PNFmin=0.5, bwf = c(0.1, 0.9),mc.cores=1, out_file = NULL){
  
  is.integer0 <- function(x) {is.integer(x) && length(x) == 0L}
  
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
      
      bts <- try(baytsDD(tsL=tsL2,distNFL=distNFL,msdL=msdL,start_history=start_history,end_history=end_history,start=start,end=end,formulaL=formulaL,orderL=orderL,chi=chi, PNFmin=PNFmin, bwf = bwf))
      
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
  if (length(bL) > 1) {
    for (i in 2:length(bL)) {
      b <- addLayer(b, bL[[i]])
    }
  }
  
  if(!is.null(mask)){b <- addLayer(mask,b)}
  
  x <- mc.calc(b, fun = fun, mc.cores = mc.cores)
  names(x) <- c("flag","change.flagged","change.confirmed","Pflag","Pchange.confirmed")
  
  if (!is.null(out_file)) {writeRaster(x,filename=out_file,overwrite=TRUE)}
  
  return(x)
}
