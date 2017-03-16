# bayts

Package for combining multiple univariate time series (e.g. optical and SAR) for deforestation detection, using a probalistic approach. The probablistic approach has been published in XXX.

## Probablistic approach for combining time series and near real-time deforstation detection
, where it was used to fuse univariate Landsat NDVI, Sentinel-1 VV and ALSO-PALSAR HV time series for detecting deforestation in near real-time over a tropical dry forest site in Santa Cruz, Bolivia.

A detailed description of theapproach is provided in [Reiche et al. 2015,17]. A brief description to fuse two univariate remote sensing time series (e.g. SAR and optical time series) is given below:


First, a weighted time series correlation is performed. To maximise the statistical significance of the correlation and to take exceptional cases into account, correlation weight optimization is done before the relationship of the two time series is modelled through a weighted regression analysis. 
The optimized regressionmodel is utilized in a second step for regression-based prediction of time series observation to fuse the two time series.

![fig2_20052014](https://cloud.githubusercontent.com/assets/6399980/7251311/77775dc4-e82a-11e4-8b6b-083cc9051fb8.jpg)
Figure 1. Schematic overview of MulTiFuse to fuse a univariate optical and SAR time series [Reiche et al. 2015](http://www.sciencedirect.com/science/article/pii/S0034425714003885).
