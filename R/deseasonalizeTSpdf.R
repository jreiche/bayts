#' @title Deseasonlize a time series using harmonic model fitting and deriving pdfs for forest and non-forest
#' 
#' @description Method as described in Reiche et al., 2018 (Remote Sensing)

#' @author Johannes Reiche (Wageningen University)

#' @param tsL list of object(s) of class \code{\link{ts}}.
#' @param pdfL list of "pdf" object(s) describing F and NF distributions (see \code{\link{calcPNF}}). 
#' @param msdL list of msdl object(s) describing the modulation of the sd of F and NF sd(F),sd(NF),mean(NF) (e.g. 2,2,-4)
#' @param distL list of "distNF" object(s) describing the mean and sd of the NF distribution in case no data driven way to derive the NF distribution is wanted; default=NULL

#' @param formula formula for the regression model. The default is response ~ trend + harmon, i.e., a linear trend and a harmonic season component. Other specifications are possible using all terms set up by bfastpp, i.e., season (seasonal pattern with dummy variables), lag (autoregressive terms), slag (seasonal autoregressive terms), or xreg (further covariates). See bfastpp for details.
#' @param order numeric. Order of the harmonic term, defaulting to 3.
#' @param start Start date of monitoring period. Default=NULL (start of input time series).
#' @param residuals TRUE = output are time series of deseasonalised "residuals"; FALSE = time series of deseasonalised values

#' @return deseasonlized time series (residuals or real values) and Gaussian pdfs of defining F and NF distribution

#' @export 

deseasonalizeTSpdf <- function (tsL=list(NULL, ...), distNFL=list(), msdL=list(), start, formula = response ~ trend + harmon, order = 1, residuals=FALSE) {
  
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
  return(list(rtsL=rtsL,pdfL=pdfL,data_tspp=data_tspp))
}
