#' @title Calculate change probability and detect changes
#' 
#' @description Iterative Bayesian updating of the conditional probability of change (PChange) based on PNF and detection of change
#' @details Short method description: Observations at time t (current observation) are flagged to be potentially changed in the case that the conditional NF probability (PNF) is larger than 0.5. 
#' For a flagged observation, the conditional probability of change (PChange) is computed by iterative Bayesian updating (\code{\link{calcPosterior}}), using the previous observation (t − 1), 
#' the current observation (t), as well as i upcoming observations (t + i) to confirm or reject a change event at t. A change is confirmed in case PChange exceeds a given threshold "chi".
#' A detailed description is provided in  \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. 2015} (Chapter 2.1.4).

#' @references \href{http://www.mdpi.com/2072-4292/7/5/4973}{Reiche et al. (2015): A Bayesian Approach to Combine Landsat and ALOS PALSAR Time Series for Near Real-Time Deforestation Detection. Remote Sensing. 7(5), 4973-4996; doi:10.3390/rs70504973}

#' @param bayts "bayts" time series data frame created with \code{\link{createBayts}}
#' @param chi Threshold of Pchange at which the change is confirmed; Default=0.5
#' @param start Start date of monitoring period. Default=NULL (start of "bayts" time series data frame).
#' @param end End date of monitoring period. Default=NULL (end of "bayts" time series data frame)

#' @author Johannes Reiche (Wageningen University)

#' @return Updated "bayts" time series data frame with changes if detected. 
#' @return Flag: "0" = no change flagged; 
#' "Flag" = change flagged (iterative Bayesian updating is ongoing); "oldFlag" = old flagged change that was rejected; "Change" = confirmed change and observations that were initially flagged as change

#' @export 


detectBayts <- function (bayts, chi = 0.5,
                         start = NULL, end = NULL) 
{
  
  ################################
  # step 1: set start and end date
  if (is.null(start)) {
    st <- 1
  }
  else {
    st <- min(which(index(bayts) > start))
  }
  if (is.null(end)) {
    en <- length(index(bayts))
  }
  else {
    en <- max(which(index(bayts) < end))
  }
  # in case of start is beginning of "bayts" time series, a zero element is added
  if (st == 1) {
    bayts_zero <- bayts[1]
    index(bayts_zero) <- index(bayts_zero) - 0.1
    bayts_zero$PNF <- 0.5
    bayts <- rbind(bayts_zero, bayts)
    st <- st + 1
    en <- en + 1
    bayts$Flag[(st - 1)] <- "0"
  }
  else {
    bayts$Flag[(st - 1)] <- "0"
  }
  
  ####################
  # step 2: Monitoring
  if (en >= st) {
    for (t in st:en) {
      
      #############################################################
      # step 2.1: Update Flag and PChange for current time step (t)
      # (case 1) No confirmed or flagged change: 
      if (bayts$Flag[(t - 1)] == "0" || bayts$Flag[(t - 1)] == "oldFlag") {
        #If PNF < 0.5 flag change and calcuate PChange using current and preveous PNF; otherwise continue
        if (bayts$PNF[t] >= 0.5) {
          i <- 0
          prior <- as.double(bayts$PNF[t - 1])
          likelihood <- as.double(bayts$PNF[t])
          postieror <- (prior * likelihood)/((prior * likelihood) + ((1 - prior) * (1 - likelihood)))
          bayts$Flag[t] <- "Flag"
          bayts$PChange[t] <- postieror
        } else {
          bayts$Flag[t] <- "0"
          next #go to next time step
        }
      }
      # (case 2) Flagged change at preveous time step: update PChange
      if (bayts$Flag[(t - 1)] == "Flag") {
        prior <- as.double(bayts$PChange[t - 1])
        likelihood <- as.double(bayts$PNF[t])
        postieror <- (prior * likelihood)/((prior * likelihood) + ((1 - prior) * (1 - likelihood)))
        bayts$PChange[t] <- postieror
        bayts$Flag[t] <- "Flag"
        i <- i + 1
      }
      
      ###############################################
      # step 2.2: Confirm and reject flagged changes
      if (bayts$Flag[(t)] == "Flag") {
        # reject changes if PChange < 0.5 for 2nd future observation
        if ((i > 0)) {
          if ((as.double(bayts$PChange[t])) < 0.5) {
            bayts$Flag[(t - i):t] <- 0
            bayts$Flag[(t - i)] <- "oldFlag"
            next #go to next time step
          }
        }
        # confirm change in case PChange >= chi
        if ((as.double(bayts$PChange[t])) >= chi) {
          if ((as.double(bayts$PNF[t])) >= 0.5) {
            bayts$Flag[min(which(bayts$Flag == "Flag")):t] <- "Change"
            return(bayts) 
          }
        }
      }
    }
  }
  return(bayts)
}


