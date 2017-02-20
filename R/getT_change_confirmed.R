#' @title Get time at which change is confirmed

#' @description Get time at which change is confirmed from "bayts" time series data frame created with \code{\link{createBayts}} and \code{\link{detectBayts}}. 

#' @author Johannes Reiche (Wageningen University)

#' @param bayts "bayts" time series data frame

#' @return time at which change is confirmed

#' @export 

getT_change_confirmed <- function(bayts){
    return(index(bayts[max(which(bayts$Flag=="Change"))]))
  } 
 