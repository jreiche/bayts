#' @title Creates "bayts" time series data frame for multiple input time series 
#' 
#' @description Creates "bayts" time series data frame from multiple input time series and calculates time series of normalised conditional non-forest probabilities (PNF). 
#' @details Short method description: PNF is estimated for each individual time series observation, using the corresponding sensor specific probability density functions (pdfs) for forest (F) and non-forest (NF).
#' In case of multiple observation at the same date, PNF is interatively updated using Bayesian probability updating (\code{\link{calcPosterior}}). 
#' A detailed description is provided in  \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. 2015} (Chapter 2.1.2 and 2.1.3).

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @author Johannes Reiche (Wageningen University)

#' @param tsL list of object(s) of class \code{\link{ts}}
#' @param pdfL list of "pdf" object(s) describing F and NF distributions (see \code{\link{calcPNF}}). 
#' "pdf" object: pdf[1] = pdf type F, pdf[2] = pdf type NF, pdf[3] and pdf[4] = pdf parameter describing F, pdf[5] and pdf[6] = pdf parameter describing NF. pdf types supported: Gaussian or Weibull.
#' @param bwf block weighting function to truncate the NF probability; Default=c(0.1,0.9); (c(0,1) = no truncation) 

#' @return "bayts" time seris data frame including: time series observation (ts1, ts2 ...), conditional non-forest probabilties (PNF), empty Flag (Flag) and empty Change probability (PChange). 

#' @examples 
#' #TBD


#' @export 

createBayts <- function (tsL=list(NULL,...), pdfL=list(NULL,...), bwf = c(0.1, 0.9)) {

  #######################################
  # step 1: add time series to data frame
  
  #number of ts
  l <- length(tsL)
  #get first ts as zoo object
  ts1 <- merge.zoo(na.omit(as.zoo(tsL[[1]])))
  
  #add remaining ts as zoo objects
  if (l>1){
    for(i in 2:l){
      #if ts is empty add empty ts
      if (length(na.omit(as.zoo(tsL[[i]])))==0){
        ts2 <- as.double(merge.zoo(na.omit(as.zoo(tsL[[1]]))))
        ts2[!is.na(ts2)] <- NA
        ts1 <- merge.zoo(ts1, ts2)
        names(ts1)[i] <- paste("ts", i, sep = "")
      #otherwise add ts
      } else {
        ts1 <- merge.zoo(ts1, na.omit(as.zoo(tsL[[i]])))
        names(ts1)[i] <- paste("ts", i, sep = "")
      }
    }
  }
  
  ##################################################################
  # step 2: calc conditional non-forest probability (PNF) 
  #         update PNF using Bayesian updating in case of multiple 
  #         observations at same date
  
  #calc PNF for the first ts and add
  PNF <- calcPNF(na.omit(as.zoo(tsL[[1]])), pdfL[[1]], bwf)
  ts1 <- merge.zoo(ts1,PNF)
  
  #calc PNF for remaining ts &
  #update PNF in case of multiple observations at the same date
  if (l>1){
    for(i in 2:l){
      ts2 <- merge.zoo(ts1, calcPNF(ts1[,i], pdfL[[i]], bwf))
      names(ts2)[l+2] <- paste("PNF2")
      #updating PNF using Bayesioan updating
      ts1$PNF[which(!is.na(ts2$PNF)==!is.na(ts2$PNF2))] <- calcPosterior(ts2$PNF[which(!is.na(ts2$PNF)==!is.na(ts2$PNF2))],ts2$PNF2[which(!is.na(ts2$PNF)==!is.na(ts2$PNF2))])
      ts1$PNF[is.na(ts1$PNF)] <- ts2$PNF2[is.na(ts1$PNF)]
      remove(ts2)
    }
  }
  
  ######################################################
  # step 3: add empty flag and Pchange row to data frame

  Flag <- ts1[, 1]
  Flag[!is.na(Flag)] <- NA
  PChange <- ts1[, 1]
  PChange[!is.na(PChange)] <- NA
  ts1 <- merge(ts1, PChange, Flag)
  
  return(ts1)
}