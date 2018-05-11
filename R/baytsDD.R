#' @title baytsDD 
#' 
#' @description (i) deseasonalising time series using harmonic model fitting , (ii) deriving F and NF pdfs in data driven way and (iii) run bayts to detect forest chang 
#' @description Method as described in Reiche et al., 2018 (Remote Sensing)

#' @author Johannes Reiche (Wageningen University)

#' @param tsL list of object(s) of class \code{\link{ts}}.
#' @param pdfL list of "pdf" object(s) describing F and NF distributions (see \code{\link{calcPNF}}). 
#' @param msdL list of msdl object(s) describing the modulation of the sd of F and NF sd(F),sd(NF),mean(NF) (e.g. 2,2,-4)
#' @param distL list of "distNF" object(s) describing the mean and sd of the NF distribution in case no data driven way to derive the NF distribution is wanted; default=NULL

#' @param formula formula for the regression model. The default is response ~ trend + harmon, i.e., a linear trend and a harmonic season component. Other specifications are possible using all terms set up by bfastpp, i.e., season (seasonal pattern with dummy variables), lag (autoregressive terms), slag (seasonal autoregressive terms), or xreg (further covariates). See bfastpp for details.
#' @param order numeric. Order of the harmonic term, defaulting to 3.
#' @param start Start date of monitoring period. Default=NULL (start of input time series). Period before is used for deseasonalisation
#' @param residuals TRUE = output are time series of deseasonalised "residuals"; FALSE = time series of deseasonalised values

#' @return List of 7 output paramter. 
#' (1) bayts: "bayts" time series data frame 
#' (2) flag: time at which unconfirmed change got flagged; 
#' (3) change.flagged: time at which confirmed change got flagged; 
#' (4) change.confirmed: time at which change is confirmed; 
#' (5) oldflag: time of earlier flagged but not confirmed changes; 
#' (6) vchange: vector of time steps from time at which change got flagged until confirmation; 
#' (7) vflag: vector of time steps at which unconfirmed change is flaged

#' @export 

baytsDD <- function (tsL=list(NULL, ...), msdL=list(), distNFL=list(), start, end = NULL, formula = response ~ trend + harmon, order = 1, chi=0.9, PNFmin = 0.5, bwf = c(0.1, 0.9),  residuals=FALSE){
  
  ## deseasonalise time series and derive pdfs
  for (i in 1:length(tsL)){
    ### STEP 1: MODEL HISTORY PERIOD USING SEASONAL AND TREND MODEL [taken from the bfast package]
    ## create data frame
    data_tspp <- bfastpp(tsL[[i]], order = order)
    ## select hisotry period
    ## potentially be expanded to select stable history period only -> see bfastmonitor function
    history_tspp <- subset(data_tspp, time < start)
    ## model history period using seasonal and trend model
    test_lm <- lm(formula, data = history_tspp)
    
    ## Calculate residuals (deseasonalised observations)
    data_tspp$prediction <- predict(test_lm, newdata = data_tspp)
    data_tspp$residuals <- data_tspp$response - data_tspp$prediction
    
    ### STEP 2: DERIVE PDFS FOR STABLE CLASS (F) AND CHANGE CLASS (NF) FROM DESEASONALISED OBSERVATIONS
    ###         IN THE HISTORY PERIOD
    ## get standard deviation of residuals from history period
    sd_history <-  sd(data_tspp$response[data_tspp$time < start])
    median_history <- median(data_tspp$response[data_tspp$time < start])
    
    ## set NF distribution by (i) defined mean and sd (distNFL)
    ## (ii) mean and sd from history period with modifications of sd (msdL) [data driven]
    if(residuals==TRUE){
      if(length(distNFL)>0){distNF<-distNFL[[i]]} else {distNF <- c(sd_history*msdL[[i]][3],sd_history*msdL[[i]][2])}
      pdf <- c(c("gaussian","gaussian"),c(0,as.double(sd_history*msdL[[i]][1])),distNF)
    } 
    if(residuals==FALSE){
      if(length(distNFL)>0){distNF<-distNFL[[i]]} else {distNF <- c(median_history+(sd_history*msdL[[i]][3]),sd_history*msdL[[i]][2])}
      pdf <- c(c("gaussian","gaussian"),c(median_history,as.double(sd_history*msdL[[i]][1])),distNF)
    }
    
    ### STEP 3: create lists of bfastts objects and pdfs for Bayts function 
    if (exists("pdfL")) {
      if(residuals==TRUE){rtsL <- c(rtsL, list(bfastts(data_tspp$residuals,time2date(data_tspp$time),type="irregular")))}
      if(residuals==FALSE){rtsL <- c(rtsL, list(bfastts(median_history+data_tspp$residuals,time2date(data_tspp$time),type="irregular")))}
      pdfL <- c(pdfL, list(pdf))
    } else {
      if(residuals==TRUE){rtsL <- list(bfastts(data_tspp$residuals,time2date(data_tspp$time),type="irregular"))}
      if(residuals==FALSE){rtsL <- list(bfastts(median_history+data_tspp$residuals,time2date(data_tspp$time),type="irregular"))}
      pdfL <- list(pdf)
    }
  }
  
  ## run bayts
  bayts <- createBayts(tsL=des$rtsL,pdfL=des$pdfL,bwf=bwf)
  bayts <- detectBayts(bayts,chi=chi,PNFmin=PNFmin,start=start,end=end)
  
  rval <- list(
    data_tspp = des$data_tspp,
    bayts = bayts,
    start = start,
    chi = chi,
    pdf = des$pdfL,
    flag = index(bayts[min(which(bayts$Flag=="Flag"))]),
    change.flagged = index(bayts[min(which(bayts$Flag=="Change"))]),
    change.confirmed = index(bayts[max(which(bayts$Flag=="Change"))]),
    oldflag = index(bayts[which(bayts$Flag=="oldFlag")]),
    vchange = na.omit(bayts$PChange[which(bayts$Flag=="Change")]),
    vflag = na.omit(bayts$PChange[which(bayts$Flag=="Flag")])
  )
  
  return(rval)
}
