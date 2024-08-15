#' Run counter for dry (zero) days
#'
#' @param x A sci_ts object.
#' @param window Time window for the moving average. In days or months, depending on the resolution of the x time series.
#' @param p0 Whether to allow for zeroes. TRUE/FALSE. Useful for the SPI, where precipitation can be zero. 
#' @param max_na_prop Maximum proportion of NA values within the time window. Number from 0-1. Default is 10 percent (0.1) 
#' @return x The rolling mean of the original time series. An sci_ts object.
#' @export
    dry_run_counter  <- function(x) {
      n <- length(x)
      i <- 0
      out <- rep(NA, n)
      out[[1]] <- x[[1]]

      for(i in seq(2, n)){
        out_prev <- out[[i-1]]
        x_i <- x[[i]]
        ### If the current is NA NA
        if(is.na(x_i)){
          out[[i]] <- NA
        } else if(x_i == 1){
          ### If the current day is dry either sum or give a 1 if the previous day was NA
          if(is.na(out_prev)){
            out[[i]] <- 1
          } else {
            out[[i]] <- out_prev + x_i
          }
        } else if(x_i == 0){
          ### If the current day is wet reset to zero
          out[[i]] <- 0
        }
      }

      return(out)
    }
