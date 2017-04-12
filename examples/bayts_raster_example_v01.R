require(bayts)

##############################################
######### load data, create & plot time series

# load raster bricks; Sentinel-1 VV (s1vv, 2014-10-07 - 2016-05-17) and Landsat NDVI (lndvi, 2005-01-03 - 2016-05-25) 
data(s1vv_lndvi_raster)

# get observation dates from raster brick
lndvi_date <- as.Date(substr(names(lndvi),10,16), format="%Y%j")
lndvi_date
s1vv_date <- as.Date(substr(names(s1vv),2,11), format="%Y.%m.%d")
s1vv_date

# plot raster
plot(s1vv,3)    # all areas are covered with forest
plot(s1vv,85)   # deforestation in the top right part is visible
plot(lndvi,290) # deforestation in the top right part is visible

###################################################################################
######### Spatial normalisation to reduce dry forest seasonality in the time series

# "Deseasonalised pixel value" = "original pixel value" - "95% Percentile of the distribution of the raster"
s1vvD <- deseasonalizeRaster(s1vv,p=0.95)
lndviD <- deseasonalizeRaster(lndvi,p=0.95)

######################################################
######### plot original and deseasonalised time series

plot(s1vv,85)
cell <- click(s1vv, n=1, cell=TRUE)[,1]
#cell <- 4264

# create time series using bfastts (bfast package)
tlndvi <- bfastts(as.vector(lndvi[cell]),lndvi_date,type=c("irregular"))   # original Landsat NDVI
tlndviD <- bfastts(as.vector(lndviD[cell]),lndvi_date,type=c("irregular")) # deseasonalised Landsat NDVI
ts1vv <- bfastts(as.vector(s1vv[cell]),s1vv_date,type=c("irregular"))      # original Sentinel-1 VV
ts1vvD <- bfastts(as.vector(s1vvD[cell]),s1vv_date,type=c("irregular"))    # deseasonalised Sentinel-1 VV

# plot time series
# strong dry forest seasonality visible in the Landsat NDVI time series
plotts(tsL=list(tlndvi,tlndviD),labL=list("LNDVI","LNDVI_deseasonalised"))
# weaker dry forest seasonality in the Sentinel-1 VV time series
plotts(list(ts1vv,ts1vvD),labL = list("S1VV [dB]","S1VV_deseasonalised [dB]"))


#############################################
######### apply baytsSpatial and plot results

# (0) subset Landsat NDVI time series to length of Sentinel-1 VV time series (start in 2015)
lndviD <- subset(lndviD, 251:291, drop=FALSE)
lndvi_date <- lndvi_date[251:291]

# (1) Define parameters 
# (1a) Sensor specific pdfs of forest (F) and non-foerst (NF). Used to calculate the conditional NF probability of each observation. Gaussian distribution of F and NF distribution. Distributions are described using mean and sd.
s1vvD_pdf <- c(c("gaussian","gaussian"),c(-1,0.75),c(-4,1))  
lndviD_pdf <- c(c("gaussian","gaussian"),c(0,0.1),c(-0.5,0.125))

# (1b) Theshold of deforestation probability at which flagged change is confirmed (chi)
chi = 0.9
# (1c) Start date of monitoring
start = 2016

# (2) Apply baytsSpatial (it takes a long time; try parallel computing at next step)
out <- baytsSpatial(list(s1vvD,lndviD),list(s1vv_date,lndvi_date),list(s1vvD_pdf,lndviD_pdf),chi=chi,start=start)
# plot confirmed changes
plot(out,3)

# (3) Apply baytsSpatial using parallel computing; using mc.calc function from bfastSpatial package 
require(bfastSpatial)
out2 <- baytsSpatial(list(s1vvD,lndviD),list(s1vv_date,lndvi_date),list(s1vvD_pdf,lndviD_pdf),chi=chi,start=start,mc.cores = 10)
# plot confirmed changes
plot(out2,3)



