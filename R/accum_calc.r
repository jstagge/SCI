#' Rolling mean calculator, calls preproccessing function
#'
#' @description Calculates a rolling mean for use in the SCI. Calls the pre-processing function automatically. No need to run pre-processing function. For an SCI without a moving window (sometimes useful for streamflow), use a window of 1.
#' @param x A sci_ts object.
#' @param window Time window for the moving average. In days or months, depending on the resolution of the x time series.
#' @param p0 Whether to allow for zeroes. TRUE/FALSE. Useful for the SPI, where precipitation can be zero. 
#' @param max_na_prop Maximum proportion of NA values within the time window. Number from 0-1. Default is 10 percent (0.1) 
#' @return x The rolling mean of the original time series. An sci_ts object.
#' @seealso  sci_ts
#' @examples
#' data("oh_precip_daily_ts")
#' example_ts <- sci_ts(ts = oh_precip_daily_ts, resolution = "daily")
#' roll_ts <- rollmean_calc(example_ts, window = 91, max_na_prop = 0.1, p0 = TRUE)
#' str(roll_ts)
#' head(roll_ts$df)
#' plot(roll_ts)
#' @export

rollmean_calc <- function(x, window, p0, max_na_prop = 0.1){
  require(tidyverse)
  require(zoo)

  if(sum(class(x) %in% "sci_ts") == 0){
    stop("Must be a sci_ts object")
  }


  x_res <- x$resolution

  ### Check for errors
  if(window < 1) {stop("window must be > 1")}
  if(window%%1!=0) { stop("window must be a integer number (1, 2, ...)")}
  if(window < 15 & x_res == "daily"){warning(paste0("You have supplied a daily resolution time series but a window of only ", window, " days. Did you mean ", round(window * 30.42), " days?"))}

  ### Pre Process the time series
  x <- pre_process(x)
  x_ts <- x$orig_ts
  x_df <- x$df

  ### Calculate the rolling mean
  x_df <- x_df %>%
    mutate(roll_mean = rollmeanr(value, k = window, fill = NA, na.rm=TRUE))

  ### Calculate the proportion of NA values
  x_df <- x_df %>%
    mutate(na_prop = rollmeanr(is.na(value), k =  window, fill = NA))

  ### Remove any period with more than max.na.prop missing
  x_df$roll_mean[x_df$na_prop > max_na_prop] <- NA

  ### Count the number of continuous days with no rainfall
  if(p0 == TRUE){
    x_df$dry_day <- as.numeric(x_df$value == 0)
    x_df$dry_runs <- dry_run_counter(x_df$dry_day)

    ### Calculate the rolling sum of dry days
    x_df <- x_df %>%
      mutate(roll_dry_days = rollsumr(dry_day, k = window, fill = NA, na.rm=TRUE))
    x_df$roll_dry_days[x_df$na_prop > max_na_prop] <- NA
  }


  ### Convert to zoo object
  if(x_res == "daily"){
    x_ts <- zoo(x_df$roll_mean, x_df$date)
  } else if (x_res == "monthly"){
    x_ts <- suppressWarnings(zoo(x_df$roll_mean, x_df$yearmon))
    x_ts <- x_ts[is.na(date(x_ts)) == FALSE]
  }

  ### Crete output
  x$df <- x_df
  x$window <- window
  x$spi_ts <- x_ts
  x$stage <- "roll_mean"
  x$max_na_prop <- max_na_prop
  x$p0 <- p0
  return(x)

}
