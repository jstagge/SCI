#' Create sci_ts object
#'
#' @description Wrapper to create an sci_ts object from a zoo time series
#' @param ts A zoo object.
#' @param resolution Character in lowercase, daily or monthly.
#' @param stage Stage in the SCI calculation process. Options include orig_ts, roll_mean, sci
#' @return  An sci_ts object.
#' @export
sci_ts <- function(ts, resolution, stage = "orig_ts"){
  ### Find the class of the time series
  x_ts <- ts
  x_class <- class(x_ts)
  x_res <- resolution

  if(sum(x_class %in% "zoo") == 0){
    stop("ts must be a zoo object")
  }

  if(sum(x_res %in% c("daily", "monthly")) == 0){
    stop("Resolution must be either daily or monthly")
  }

  if(x_res == "monthly" & frequency(x_ts) != 12){
    stop("For monthly data ts should be a zoo object with yearmon")
  }

  ### Create object
  x <- list(spi_ts = NA, orig_ts = x_ts, df = NA, resolution = x_res, window = NA, stage = stage, method = NA, max_na_prop = NA, p0 = NA)
  attr(x, "class") <- "sci_ts"


  return(x)
}



#' Create sci_fit object
#'
#' @description Wrapper to create an sci_fit object
#' @param fit 
#' @param roll_mean 
#' @param nn 
#' @param n_zero 
#' @param monitor 
#' @param distr 
#' @param method 
#' @param ref_years 
#' @return  An sci_fit object.
#' @export
sci_fit <- function(fit, roll_mean, nn, n_zero, monitor, distr, method, ref_years){

  x <- list(fit = fit, roll_mean = roll_mean, nn = nn, n_zero = n_zero, monitor = monitor, distr = distr, method = method, ref_years =  ref_years)
  attr(x, "class") <- "sci_fit"

  return(x)
}
