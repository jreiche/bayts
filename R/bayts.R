#' @title Bayesian approach to combine multiple time series for near real-time forest change detection
#' 
#' @description (i) Creates "bayts" time series data frame from multiple input time series and calculates time series of normalised conditional non-forest probabilities (PNF). 
#' (ii) Iterative Bayesian updating of the conditional probability of change (PChange) based on PNF and detection of change
#' @details Short method description: First, the conditional probability for non-forest (PNF) is estimated for each individual time series observation, using the corresponding sensor specific probability density functions (pdfs) for forest (F) and non-forest (NF).
#' In case of multiple observation at the same date, PNF is interatively updated using Bayesian probability updating (\code{\link{calcPosterior}}). 
#' Observations at time t (current observation) are flagged to be potentially changed in the case that the conditional NF probability (PNF) is larger than 0.5. 
#' For a flagged observation, the conditional probability of change (PChange) is computed by iterative Bayesian updating (\code{\link{calcPosterior}}), using the previous observation (t − 1), 
#' the current observation (t), as well as i upcoming observations (t + i) to confirm or reject a change event at t. A change is confirmed in case PChange exceeds a given threshold "chi".
#' A detailed description is provided in  \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. 2015} (Chapter 2.1.2 and 2.1.3).


#' @param tsL list of object(s) of class \code{\link{ts}}.
#' @param pdfL list of "pdf" object(s) describing F and NF distributions (see \code{\link{calcPNF}}). 
#' "pdf" object: pdf[1] = pdf type F, pdf[2] = pdf type NF, pdf[3] and pdf[4] = pdf parameter describing F, pdf[5] and pdf[6] = pdf parameter describing NF. pdf types supported: Gaussian or Weibull.
#' @param bwf block weighting function to truncate the NF probability; Default=c(0.1,0.9); (c(0,1) = no truncation) 
#' @param chi Threshold of Pchange at which the change is confirmed; Default=0.5
#' @param PNFmin threshold of pNF above which the first observation is flagged; Default=0.5
#' @param start Start date of monitoring period. Default=NULL (start of input time series).
#' @param end End date of monitoring period. Default=NULL (end of input time series)

#' @author Johannes Reiche (Wageningen University)

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @return List of 7 output paramter. 
#' (1) bayts: "bayts" time series data frame 
#' (2) flag: time at which unconfirmed change got flagged; 
#' (3) change.flagged: time at which confirmed change got flagged; 
#' (4) change.confirmed: time at which change is confirmed; 
#' (5) oldflag: time of earlier flagged but not confirmed changes; 
#' (6) vchange: vector of time steps from time at which change got flagged until confirmation; 
#' (7) vflag: vector of time steps at which unconfirmed change is flaged


#' @export 


bayts <- function(tsL=list(NULL,...), pdfL=list(NULL,...), bwf=c(0.1, 0.9), chi=0.9, PNFmin=0.5, start=NULL, end=NULL){
  
  bayts <- createBayts(tsL=tsL, pdfL=pdfL, bwf=bwf)

  bayts <- detectBayts(bayts, chi=chi, PNFmin=PNFmin, start=start, end=end)
  
  #create output list
  baytsL <- list(
    bayts = bayts,
    flag = index(bayts[min(which(bayts$Flag=="Flag"))]),
    change.flagged = index(bayts[min(which(bayts$Flag=="Change"))]),
    change.confirmed = index(bayts[max(which(bayts$Flag=="Change"))]),
    oldflag = index(bayts[which(bayts$Flag=="oldFlag")]),
    vchange = na.omit(bayts$PChange[which(bayts$Flag=="Change")]),
    vflag = na.omit(bayts$PChange[which(bayts$Flag=="Flag")])
 )
  return(baytsL)
}

