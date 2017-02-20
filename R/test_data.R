#' @title Sentinel-1 VV and Landsat NDVI time series example
#' 
#' @description Sentinel-1 VV and Landsat NDVI time series; 09/2014 - 05/2016; Deforestation event in early 2016.  Two univariate time series objects of the class \link{ts}. 
#'
#' @name s1vv_lndvi_pixel.rda
#' 
#' @usage data(s1vv_lndvi_pixel)
#' 
#' @author Johannes Reiche
#' 
#' @examples
#' # load test data
#' data(s1vv_lndvi_pixel)
#' 
#' # create ts using bfastts (bfast package)
#' tsS <- bfastts(s1vv_obs,as.Date(s1vv_date),type=c("irregular"))
#' tsL <- bfastts(lndvi_obs,as.Date(lndvi_date),type=c("irregular"))
#'
#' # plot ts
#' plotts(tsL=list(tsL,tsS),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"))
#' plotts(tsL=list(tsL,tsS),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"),ylimL=list(c(0,1),c(-13,-6)))

NULL

#' @title Sentinel-1 VV and Landsat NDVI raster brick
#' 
#' @description Sentinel-1 VV and Landsat NDVI time series raster brick; 09/2014 - 05/2016. 
#' Data has been taken from an area in Santa Cruz, Bolivia. Please contact Johannes Reiche (johannes.reiche@wur) for more information. 
#' Forest mask is applied.
#' 
#' @name s1vv_landsat_raster.rda
#' 
#' @usage data(s1vv_lndvi_raster)
#' 
#' @author Johannes Reiche
#' 
#' @examples
#' ## load test data
#' data(s1vv_lndvi_raster)
#' 
#' plot(s1vv,1)
#' 

NULL