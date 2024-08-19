#' Calculate lagged correlation of SCI object
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
create_day_seq <- function(n_years, start_date, leap_days = TRUE){
  require(lubridate)

  date_seq <- seq(start_date, start_date + years(n_years) - 1, by = "days")

  ## Remove leap Days to make a 365 day year
  if(leap_days == FALSE){
    date_df <- data.frame(date = date_seq) %>%
      mutate(year = year(date), month = month(date), day = day(date)) %>%
      mutate(leap_day = case_when(month == 2 & day == 29~1,
        TRUE ~0)
      ) %>%
      filter(leap_day == 0)

    date_seq <- date_df$date
   }

  return(date_seq)
}


#' Calculate lagged correlation of SCI object
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
sim_sci <- function(date_seq, window){
  ###
  require(tidyverse)
  require(lubridate)

  ### Create the dataframe object to hold
  sci_df <- data.frame(date = date_seq) %>%
    mutate(jdate = yday(date)) %>%
  	mutate(month = month(date), day = day(date), year = year(date))

  ### Simulate the sci
  n_period <- dim(sci_df)[1]

  ### Use a 92 day moving average MA(91) with innovations of sqrt(92)/n_roll, which produces N(0,1)
  ### Technically, the moving average would be on precip, not sci, but this is a very close approximation
  innov_c <- rnorm(n_period, 0, sqrt(window))

  ### Generate sci series using an MA(91) model with coef of 1. Need to remove the first 91 values
  sci <- arima.sim(list(order = c(0,0,(window - 1)), ma = rep(1,(window -1))), n = n_period, innov=innov_c/window)
  sci[seq(1,(window-1))] <- NA

  ### Add back to the dataframe
  sci_df <- sci_df %>%
      mutate(innov = innov_c/window) %>%
	    mutate(sci = c(sci)) %>%
      dplyr::select(date, year, month, day, jdate, innov, sci)

    ### Return result
    return(sci_df)

}
