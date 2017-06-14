#' @title Convert time to date 
#' 

#' @author Johannes Reiche (Wageningen University)

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @param time time

#' @param format date format; default="%Y-%m-%d". DOY = "%Y.%j" 

#' @return single value, vector or time series object of class "ts"

#' @export 

time2date <- function(time,format = "%Y-%m-%d"){
  year <- floor(time)
  jday <- round((time - year) * 365 + 1)
  jday <- formatC(jday, width = 3, flag = 0)
  dates <- as.Date(paste(year, jday, sep=""), format="%Y%j")
  return(strftime(dates, format = format))
}
