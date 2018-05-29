###################################################################################################
# Example applying method presented in Reiche et al., 2018 (Remote Sensing)                       #                           #
#                                                                                                 #
# Data:   Sentinel-1 raster time series data from Riau, Sumatra                                   #
#                                                                                                 #
# Step 1: Use season-trend fitting to model and remove forest seasonality                         #                     #
#                                                                                                 #
# Step 2: Data-driven way to derive F and NF distribution (pdfs) to paramterise bayts             #
#                                                                                                 #
# Step 3: Probablistic approach (bayts, baytsSpatial) used to combine optical                     #
#         and SAR time series and to detect change in near real-time                              #                                         #
#         Probabalistic approach (bayts) published in Reiche et al., 2015, (Remote Sensing;       #
#         https://doi.org/10.3390/rs70504973)                                                     #                                                              #
#                                                                                                 #
# Reiche et al., 2018 (RSE): "Improving near-real time deforestation monitoring in tropical dry   #
# forests by combining dense Sentinel-1 time series with Landsat and ALOS-2 PALSAR-2"             #
# http://dx.doi.org/10.1016/j.rse.2017.10.034)                                                    # 
###################################################################################################

require(bayts)

##############################################
######### load data, create & plot time series

# load raster bricks; Sentinel-1 VV (s1vv, 2014-10-07 - 2016-05-17) and Landsat NDVI (lndvi, 2005-01-03 - 2016-05-25) 
data(s1vh_riau_raster)

# plot raster
plot(s1vh,3)
plot(s1vh,250) 
plot(fmask,add=T) #forest mask

## Single pixel example
plot(s1vh,250) 
cell <- click(s1vh, n=1, cell=TRUE)[,1]
ts1vh <- bfastts(as.vector(s1vh[cell]),s1vh_date,type=c("irregular"))      # original Sentinel-1 VV
plotts(list(ts1vh),labL = list("S1VH [dB]"))


## apply baytsDD to single pixel
# start of monitoring period
start = 2016
# define model
formula = response ~ harmon #simple harmonic model
# define order of model
order = 1 # first order model choosen
# definition of F and NF pdf standard deviation modulations
pdfsd <- c(2,-4,2) #F = N(median; 2sd) NF = N(median - 4sd,2sd)
# Theshold of deforestation probability at which flagged change is confirmed (chi)
chi = 0.85
# Minum probability of NF at which potential changes are indicates (default is 0.5)
PNFmin = 0.75 

# (1) apply
anb <- baytsDD(tsL=list(ts1vh),formulaL=list(formula),pdfsdL=list(pdfsd),orderL=list(order),start=start,PNFmin=PNFmin,chi=chi)
# (2) plot
plotBayts(anb$bayts,ylimL=list(c(-20,-10),c(-10,5)),labL=list("VH [dB]"),plotflag = TRUE)


## Apply baytsSpatialDD and using forest mask to only apply to forest pixels
# (1) Apply baytsSpatialDD (it may takes up to 5 minutes; try parallel computing at next step)
out <- baytsDDSpatial(bL=list(s1vh), datesL=list(s1vh_date), pdfsdL=list(pdfsd), mask=fmask, start=start,formulaL=list(formula),orderL=list(order),PNFmin=PNFmin,chi=chi)
# plot confirmed changes
plot(out,3)

# (2) Apply baytsSpatialDD using parallel computing; using mc.calc function from bfastSpatial package 
require(bfastSpatial)
out2 <- baytsDDSpatial(bL=list(s1vh), datesL=list(s1vh_date), pdfsdL=list(pdfsd), mask=fmask, start=start,formulaL=list(formula),orderL=list(order),PNFmin=PNFmin,chi=chi,mc.cores=10)
# plot confirmed changes
plot(out2,2)




