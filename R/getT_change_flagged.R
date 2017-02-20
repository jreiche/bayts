#' @title Get time at which confirmed change was flagged
#' 
#' @description Get time at which confirmed change was flagged from "bayts" time series data frame created with \code{\link{createBayts}} and \code{\link{detectBayts}}. 

#' @author Johannes Reiche (Wageningen University)

#' @param bayts "bayts" time series data frame

#' @return time at which confirmed change was flagged

#' @export 

getT_change_flagged <- function(bayts){
  return(index(bayts[min(which(bayts$Flag=="Change"))]))
} 
