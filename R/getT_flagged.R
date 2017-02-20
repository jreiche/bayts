#' @title Get time at which unconfirmed change is flagged
 
#' @description Get time at which unconfirmed change is flagged from "bayts" time series data frame created with \code{\link{createBayts}} and \code{\link{detectBayts}}. 

#' @author Johannes Reiche (Wageningen University)

#' @param bayts "bayts" time series data frame

#' @return Time at which unconfirmed change is flagged

#' @export 

getT_flagged <- function(bayts){
  return(index(bayts[min(which(bayts$Flag=="Flag"))]))
} 
