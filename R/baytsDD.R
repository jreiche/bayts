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
  des <- deseasonalizeTSpdf(tsL=tsL, msdL=msdL, distNFL=distNFL, start=start, formula = formula, order = order,  residuals=residuals)
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
