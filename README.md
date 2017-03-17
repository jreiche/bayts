# bayts

Propabalistic approach (Reiche et al., 2015, 2017) to combine multiple optical and Radar satellite time series and to detect deforestation. The package includes functions to apply the approach to single pixel time series and to raster time series. Examples for both are provided below.

## Probablistic approach 
The basic version of the probablistic approach has been published in Reiche et al., (2015). An improved version was published in Reiche et al. (under review). In Reiche et al., 2017, the probabalistic approach was used for the multi-sensor combination of Radar and optical data from Sentinel-1 and PALSAR-2 HV, together with Landsat for near-real time forest change detection in tropical dry forests in Bolivia. A brief description of the approach is provided below:

Figure 1 gives an schematic overview the probabilistic approach. We considered a near real-time scenario with past (t-1), current (t) and future observations (t+1), with multiple observations possible at the same observation date. First, once a new observation of either of the input time series was available (t = current) it was converted to the conditional NF probability (s<sup>NF</sup>) using the sensor specific forest (F) and non-forest (NF) probability density functions (pdf) (The sensor specific F and NF pdfs were derived using training data).
The derived conditional NF probability was added to the combined time series of conditional NF probabilities derived from the previous LNDVIn, S1VVn and P2HVn time series observations (t–i). Second, we flagged a potential deforestation event in the case that the conditional NF probability was larger than 0.5. We calculated the probability of deforestation using iterative Bayesian updating. Future observation (t+i) were used to update the probability of deforestation in order to confirm or reject the flagged deforestation event.

![fig](method_overview.jpg)
Figure 1. Probabilistic approach used to combine time series of Landsat NDVI (LNDVIn), Sentinel-1 VV (S1VVn) and ALOS-2 PALSAR-2 HV (P2HVn) observations and to detect deforestation in near real-time. (Reiche et al., under review) 


## Install

The package can be installed directly from github using devtools
```r
library(devtools)
install_github('jreiche/bayts')
```
## Examples 

Two examples are provided ...

### Example data

Example data, dry forest Bolivia, Reiche et al. (under review)
Figure

### Example 1: Single-pixel example (Deforestation)

Include pictures

### Example 2: Area example (Deforestation over dry forest)

Include pictures

## References
Reiche, J., de Bruin, S., Hoekman, D. H., Verbesselt, J. & Herold, M. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing, 7, 4973-4996. DOI:10.3390/rs70504973. (http://www.mdpi.com/2072-4292/7/5/4973)

Reiche, J., Hamunyela, E., Verbesselt, J., Hoekman, D. & Herold, M. (under review): Near-real time deforestation detection in tropical dry forest combining Landsat, Sentinel-1 and ALOS-2 PALSAR-2 time series. Remote Sensing of Environment. 

## License
