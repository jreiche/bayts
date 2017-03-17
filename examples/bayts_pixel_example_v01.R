require(bayts)

##############################################
######### load data, create & plot time series

# load example data
# single pixel Sentinel-1 VV and Landsat NDVI time series (09/2014 - 05/2016); deforestation event in early 2016
data(s1vv_lndvi_pixel)

# create time series using bfastts (bfast package)
ts1vv <- bfastts(s1vv_obs,as.Date(s1vv_date),type=c("irregular"))
tlndvi <- bfastts(lndvi_obs,as.Date(lndvi_date),type=c("irregular"))

# plot time series
plotts(tsL=list(tlndvi,ts1vv),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"))
plotts(tsL=list(tlndvi,ts1vv),labL=list("Landsat NDVI","Sentinel-1 VV [dB]"),ylimL=list(c(0,1),c(-13,-6)))

######################################
######### apply bayts and plot results

# (1) Define parameters 
# (1a) Sensor specific pdfs of forest (F) and non-foerst (NF). Used to calculate the conditional NF probability of each observation. Gaussian distribution of F and NF distribution. Distributions are described using mean and sd.
s1vv_pdf <- c(c("gaussian","gaussian"),c(-7,0.75),c(-11.5,1))    
lndvi_pdf <- c(c("gaussian","gaussian"),c(0.85,0.075),c(0.4,0.125))

# (1b) Theshold of deforestation probability at which flagged change is confirmed (chi)
chi = 0.9
# (1c) Start date of monitoring
start = 2015

# (2) apply bayts (combine original time series into a time series of NF probabilities and detect deforestation)
# (2a) apply bayts
bts <- bayts(tsL=list(tlndvi,ts1vv),pdfL=list(lndvi_pdf,s1vv_pdf),chi=chi,start=start)

# (2b) plot original time series; including flagged and detected changes
plotBayts(bts$bayts,labL=list("Landsat NDVI","Sentinel-1 VV [dB]"),ylimL=list(c(0,1),c(-13,-6)),start=start)

# (2c) plot time series of NF probabilities, including flagged and detected changes
plotBaytsPNF(bts$bayts,start=start)

# (2d) get time of change
bts$change.flagged    # time at which change is flagged
bts$change.confirmed  # time at which change is confirmed



