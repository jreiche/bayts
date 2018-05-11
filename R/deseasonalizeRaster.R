#' @title Deseasonlize a raster using spatial normalisation 
#' 
#' @description Deseasonlize a single raster layer or each individual raster layer in a brick. "Deseasonlized pixel value" = "original pixel value" - "percentile of the distribution of the raster"
#' @description Method is based on Hamunyela et al., 2015 (RSE) and is described in detail in Reiche et al., 2018 (RSE)

#' @author Johannes Reiche (Wageningen University)

#' @param b raster or raster brick
#' @param percentile percentile of the distribution used to deseasonalize the raster. Values between 0 - 1.
#' @param min minimum value allowed. Values below the minimum are set to NA. 
#' @param max maximum value allowed. Values above the maximum are set to NA. 

#' @return deseasonlized raster layer or raster brick 

#' @examples 
#' data(s1vv_lndvi_raster)
#' 
#' plot(s1vv,3)
#' 
#' s1vvd <- deseasonalizeRaster(s1vv,p=0.95)
#' 
#' plot(s1vvd,3)
#' @export 


deseasonalizeRaster <- function(b, percentile=0.95, min=NULL, max=NULL){
  
  # function to calculate percentile
  calcPercentileRaster <- function(b, percentile=0.95, min=NULL, max=NULL){
    
    # create empty vector
    perc <- rep(NA, nlayers(b))
    
    # loop through raster brick
    for(i in 1:nlayers(b)){
      if(nlayers(b)==1){
        # single raster layer
        ras <- b
      } else {
        # get layer from raster brick
        ras <- raster(b,i)
      }
      # set values > max and < min to NA
      if (!is.null(max)){ras[ras>max]<-NA}
      if (!is.null(min)){ras[ras<min]<-NA}
      
      # calculate percentile
      perc[i] <- as.numeric(quantile(ras, c(percentile), na.rm =T))
    }
    return(perc)
  }
  
  # function to deseasonalize
  deseasonalize <- function(v,perc){
    # set values > max and < min to NA
    if (!is.null(max)){v[v>max]<-NA}
    if (!is.null(min)){v[v<min]<-NA}
    # deseasonalize
    vd <- v-perc
    return(vd)
  }
  
  # single raster layer
  if(nlayers(b)==1){
    r <- deseasonalize(b,calcPercentileRaster(b, percentile=percentile, min=min, max=max))
  } 
  # brick
  if(nlayers(b)>1){
    print(paste(1,"/",nlayers(b)))
    r <- deseasonalize(raster(b,1),calcPercentileRaster(raster(b,1), percentile=percentile, min=min, max=max))
    for(i in 2: nlayers(b)){
      print(paste(i,"/",nlayers(b)))
      r2 <- deseasonalize(raster(b,i),calcPercentileRaster(raster(b,i), percentile=percentile, min=min, max=max))
      r <- addLayer(r,r2)
    }
    r <- brick(r)
  }
  
  names(r) <- names(b)
  setZ(r,getZ(b))
  return(r)
}

