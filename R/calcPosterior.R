#' @title Calculating posterior (Bayes theorem) 
#' 
#' @description Calculating posterior according to Bayes theorem.
#' @description posterior = (prior * likelihood)/((prior * likelihood) + ((1 - prior) * (1 - likelihood)))

#' @author Johannes Reiche (Wageningen University)

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @param prior single value, vector or time series object of class "ts"

#' @param likelihood single value, vector or time series object of class "ts"

#' @return single value, vector or time series object of class "ts"

#' @export 

calcPosterior <- function(prior, likelihood){
  return((prior * likelihood)/((prior * likelihood) + ((1 - prior) * (1 - likelihood))))
}