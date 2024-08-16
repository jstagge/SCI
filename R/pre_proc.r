#' Generic read in function
#'
#' @description To fill in
#' @param x A sci_ts object.
#' @return x An sci_ts object.
#' @seealso  sci_ts
#' @export
pre_process <- function(x){
  require(tidyverse)
  require(zoo)
  ### Find the class of the time series
  x_ts <- x$orig_ts
  x_res <- x$resolution

  if(sum(class(x) %in% "sci_ts") == 0){
    stop("Must be a sci_ts object")
  }

  ### Run processing scheme
  if(x_res == "daily"){
    x_df <- pre_process_daily(x_ts)
  } else if(x_res == "monthly"){
    x_df <- pre_process_monthly(x_ts)
  }

  x$df <- x_df
  x$stage <- "orig_ts"

  return(x)

}



#' Read in function for daily time series
#'
#' @description To fill in
#' @param x A sci_ts object.
#' @return x An sci_ts object.
#' @seealso  sci_ts
#' @export
pre_process_daily <- function(x){
  x <- fortify.zoo(x)
  x <- x %>%
    rename(date = Index) %>%
    rename(value = 2)

    ### Expand the time frame to the given resolution
    date_df <- data.frame(date = seq(as.Date(min(x$date)), as.Date(max(x$date)), by = "1 day"))
    x <- date_df %>%
      left_join(x, by = "date") %>%
      arrange(date)

    ### Add in additional time columns
    x <- x %>%
      mutate(year = year(date), month = month(date), day = day(date), jdate = yday(date)) %>%
      dplyr::select(date, year, month, day, jdate, value)

  return(x)
}



#' Read in function for monthly time series
#'
#' @description To fill in
#' @param x A sci_ts object.
#' @return x An sci_ts object.
#' @seealso  sci_ts
#' @export
pre_process_monthly <- function(x){
  x <- fortify.zoo(x)
  x <- x %>%
    rename(yearmon = Index) %>%
    rename(value = 2) %>%
    mutate(year = year(yearmon), month = month(yearmon))

  ### Expand the time frame to the given resolution
  date_df <- expand.grid(year=unique(x$year), month = unique(x$month))
  x <- date_df %>%
    left_join(x, by = c("year", "month")) %>%
    arrange(year, month)

  return(x)
}
