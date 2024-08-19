
#' Calculate lagged correlation of SCI object
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
lag_corr <- function(x, lag_list = seq(0,365*2), method = "pearson") {
  require(fitdistrplus)
  require(tidyverse)

  ### Check that it is an sci_ts object
  if(sum(class(x) %in% "sci_ts") == 0){
    stop("Must be a sci_ts object")
  }
  ### Check that it has an SCI time series
  if(x$stage != "sci"){
    stop("Must be a sci_ts object with a generated SCI time series")
  }

  # Function to calculate correlation for a given lag
  calculate_lag_correlation <- function(lag_value) {
    lagged_data <- x$df %>%
      mutate(lagged = lag(sci, lag_value))
    cor_value <- cor(lagged_data$sci, lagged_data$lagged, use = "pairwise.complete.obs", method = method)
    return(c(lag = lag_value, correlation = cor_value))
  }

  # Use lapply to calculate correlations for all lags from 1 to max_lag
  lag_results <- do.call(rbind, lapply(lag_list, calculate_lag_correlation))
  lag_results <- as.data.frame(lag_results)

  return(lag_results)
}


#' Fit SCI
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
theor_sci_corr <- function(n_years, resolution, method = "pearson", n_rep = 100, window, lag_list = seq(0,365*2), ci = 0.95){
  #require(plyr)

  start_date <- as.Date("1900-01-01")


  if(resolution == "daily"){
      date_seq <- create_day_seq(n_years = n_years, start_date = start_date, leap_days = FALSE)
      date_seq <- zoo(NA, date_seq)
  } else if (resolution == "monthly"){
      date_seq <- seq(start_date, length.out = n_years * 12, by = "month")
      date_seq <- zoo(NA, date_seq)
  }

  ### Function to replicate simulating and taking the lagged correlation
  replicate_func <- function(){
    sci_df <- sim_sci(date_seq = date_seq, window = window)
    sci_df <- list(df = sci_df, resolution = resolution, window = window, stage = "sci")
    class(sci_df) <- "sci_ts"

    ### Calculate the lagged correlation
    temp_results <- lag_corr(x = sci_df, lag_list = lag_list, method = method)
    return(temp_results)
  }

  ### Run multiple replicates
  theor_df <- plyr::rdply(n_rep, replicate_func)
  theor_df <- theor_df %>% dplyr::rename(run = 1)

  ### Determine the upper and lower confidence intervals
  lower_ci <- (1-ci)/2
  upper_ci <- 1-lower_ci

  ### Summarize the results
  range_df <- theor_df %>%
   dplyr::group_by(lag) %>%
   dplyr::summarize(lower_ci = quantile(correlation, lower_ci), upper_ci = quantile(correlation, upper_ci), mean = mean(correlation, na.rm=TRUE), median = median(correlation, 0.5)) %>%
   dplyr::select(lag, mean, median, lower_ci, upper_ci)


  return(list(summary = range_df, raw = theor_df))
}




#' Calculate lagged correlation of SCI object
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
persist_test <- function(x, lag_list = seq(0,365*2), method = "pearson", n_rep = 100, ci= 0.95){

  x_res <- x$resolution
  window <- x$window

  ### Calculate lagged correlation for observations
  corr_obs <- lag_corr(x, lag_list = lag_list, method = method)

  ### Caulate the theoretical lagged correlation
  if(x_res == "daily"){
      n_years <- round(dim(x$df)[1]/365)
  } else if (x_res == "monthly"){
      n_years <- round(dim(x$df)[1]/12)
  }

  corr_theor <- theor_sci_corr(n_years = n_years, resolution = x_res, n_rep = n_rep, window = window, lag_list = lag_list, ci = ci)

  return(corr_obs = corr_obs, corr_theor = corr_theor)
}



#ma_interval_test <- function(n_rep = , n_years, accum_period, lag_list = seq(0,365*2), ci = 0.95){
#}
