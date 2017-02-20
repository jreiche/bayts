#' @title Sentinel-1 VV and Landsat NDVI time series example
#' 
#' @description Sentinel-1 VV and Landsat NDVI time series; 09/2014 - 05/2016; Deforestation event in early 2016.  Two univariate time series objects of the class \link{ts}. 
#'
#' @name s1_landsat_pixel.RData
#' 
#' @usage data(s1_landsat_pixel.RData)
#' 
#' @author Johannes Reiche
#' 
#' @examples
#' ## load test data
#' load("s1_landsat_pixel.RData")
#' 
#'
#' # create ts using bfastts (bfast package)
#' tsS <- bfastts(s1,as.Date(s1_date),type=c("irregular"))
#' tsL <- bfastts(landsat,as.Date(landsat_date),type=c("irregular"))
#'
#' # plot ts
#' plotts(tsL=list(tsL,tsS),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"))
#' plotts(tsL=list(tsL,tsS),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"),ylimL=list(c(0,1),c(-13,-6)))

NULL

#' @title Sentinel-1 VV and Landsat NDVI raster brick
#' 
#' @description Sentinel-1 VV and Landsat NDVI time series raster brick; 09/2014 - 05/2016; Deforestation event in early 2016.  Two univariate time series objects of the class \link{ts}. 
#' 
#' @name s1_landsat_raster.RData
#' 
#' @usage data(s1_landsat_raster.RData)
#' 
#' @author Johannes Reiche
#' 
#' @examples
#' ## load test data
#' load("s1_landsat_raster.RData")
#' 
#' plot(s1,1)
#' 

NULL