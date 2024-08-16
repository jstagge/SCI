#' Transform SCI
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
transformSCI <- function(fit, new_ts=NA, p0.center.mass=FALSE, sci.limit=Inf, ci = 0.95, ci_method = "mvn", ci_n = 200){
    require(tidyverse)
    require(zoo)

    ### If the user doesn't specify a new time series use the one from fitting
    if(is.na(new_ts)){
      new_ts <- fit$roll_mean
    } else {
      ### You need to run the rolling mean
      new_ts <- rollmean_calc(new_ts, window = fit$roll_mean$window, max_na_prop = fit$roll_mean$max_na_prop, p0 = fit$roll_mean$p0)
    }

    ts_res <- new_ts$resolution
    fit_res <- fit$roll_mean$resolution

    ### Put in some warnings
    if(sum(class(fit) %in% "sci_fit") == 0){
      stop("fit must be a sci_fit object")
    }

    if(sum(class(new_ts) %in% "sci_ts") == 0){
      stop("new_ts must be a sci_ts object")
    }

    if(new_ts$stage != "roll_mean"){
      stop("new_ts must be processed as a rolling mean")
    }

    if(ts_res != fit_res){
      stop("Resolution (daily or monthly) of the time series must match the fit object")
    }

    ###
    if(fit$method == "MLE"){
      sci_df <- transform_mle(fit = fit, new_ts = new_ts, p0.center.mass = p0.center.mass, ci = ci, ci_method = ci_method, ci_n=ci_n)
    }

    ### Fix limits
    sci_df$sci[sci_df$sci >= sci.limit] <- sci.limit
    sci_df$sci[sci_df$sci <= -sci.limit] <- -sci.limit
    if(!is.na(ci)){
      sci_df$sci_lower[sci_df$sci_lower >= sci.limit] <- sci.limit
      sci_df$sci_lower[sci_df$sci_lower <= -sci.limit] <- -sci.limit
      sci_df$sci_upper[sci_df$sci_upper >= sci.limit] <- sci.limit
      sci_df$sci_upper[sci_df$sci_upper <= -sci.limit] <- -sci.limit
    }

    ###
    x <- new_ts
    x$stage <- "sci"
    x$df <- sci_df

    if(ts_res == "daily"){
      x$spi_ts <- zoo(sci_df$sci, sci_df$date)
    } else if (ts_res == "monthly"){
      x$spi_ts <- zoo(sci_df$sci, sci_df$yearmon)
    }


    return(x)
}



#' Transform SCI for MLE
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
transform_mle <- function(fit, new_ts, p0.center.mass, ci = ci, ci_method = ci_method, ci_n=ci_n){

  ### Extract the df and resolution
  x_df <- new_ts$df
  x_res <- new_ts$resolution

  ### Extract the distribution
  pdistr <- fit$distr
  pdistr <- match.fun(paste("p",pdistr,sep=""))

  x_df$sci_perc <- NA
  x_df$sci <- NA

  if(x_res == "daily"){j_seq <- seq(1,365)}
  if(x_res == "monthly"){j_seq <- seq(1,12)}

  for(j in j_seq){
    ### Extract the fit
    fit_j <- fit$fit[[j]]
    j_test <- x_df$jdate == j

    ### Calculate percentile
    x_j <- x_df$roll_mean[j_test]
    sci_percentile <- do.call(pdistr,c(list(x_j),fit_j$estimate))

    ### For zeros
    if(fit$roll_mean$p0 == TRUE){
      sci_percentile <- zero_mle(sci_percentile = sci_percentile, roll_mean = x_j, nn = fit$nn[[j]], n_zero = fit$n_zero[[j]], p0.center.mass = p0.center.mass)
    }

    ### Add in percentile
    x_df$sci_perc[j_test] <- sci_percentile
    rm(sci_percentile)
  }

  ### Convert to SCI
  x_df$sci <- qnorm(x_df$sci_perc)

  ### CI
  if(!is.na(ci)){
   ci_result <-  sapply(j_seq, mle_ci, fit, x_df, ci_n = ci_n, ci=ci, ci_method = ci_method)
   ci_result <- bind_rows(apply(ci_result, 2, bind_rows))

   x_df <- x_df %>%
     left_join(ci_result, by = "date")
  } else {
    x_df <- x_df %>%
      mutate(sci_lower = NA, sci_upper = NA)
  }

  return(x_df)

}




#' Zero handling function for MLE
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
zero_mle <- function(sci_percentile, roll_mean, nn, n_zero, p0.center.mass){
    if(p0.center.mass == TRUE){
        ### Put sci at the center of the zero probability
        sci_zero <- (n_zero + 1)/(2*nn + 1)
    } else {
        ### Put sci at the top of the zero probability
        sci_zero <- (n_zero )/(nn + 1)
    }

    lower_thresh <- n_zero/nn
    sci_percentile[roll_mean > 0 & !is.na(roll_mean)] <- lower_thresh + (1-lower_thresh) * sci_percentile[roll_mean > 0  & !is.na(roll_mean)]
    sci_percentile[roll_mean == 0 & !is.na(roll_mean)] <-  sci_zero

  return(sci_percentile)
  }







#' Confidence Intervals for MLE
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
mle_ci <- function(j, fit, x_df, ci_n, ci, ci_method){

  fit_j <- fit$fit[[j]]
  j_test <- x_df$jdate == j
  x_j <- x_df[j_test,]
  x_res <- fit$roll_mean$resolution

  if(ci_method == "mvn"){
    ### Multivariate normal
    draws <- as.data.frame(mvrnorm(ci_n, mu = coef(fit_j), Sigma = vcov(fit_j)) )
  } else if (ci_method == "boot"){
    ### Bootstrap
    draws <- bootdist(fit_j, niter=ci_n)
  	draws <- draws$estim
  }

  ### Loop through each of the draws
  for(k in seq(1,dim(draws)[1])){
      draw_k <- unlist(draws[k,])
      x_temp <- x_j
      x_temp$sci_perc <- do.call(pdistr,c(list(x_temp$roll_mean),draw_k))
      x_temp$draw <- k
      if(k == 1){
        x_draw <- x_temp
      } else {
        x_draw <- x_draw %>%
          bind_rows(x_temp)
      }
  }

  ### For zeros
  if(fit$roll_mean$p0 == TRUE){
    x_draw$sci_perc <- zero_mle(sci_percentile = x_draw$sci_perc, roll_mean = x_temp$roll_mean, nn = fit$nn[[j]], n_zero = fit$n_zero[[j]], p0.center.mass = p0.center.mass)
  }


  ### Calculate the sci
  x_draw$sci <- qnorm(x_draw$sci_perc)

  ### Calculate the upper and lower CI
  ci_values <- (1-ci)/2
  ci_values <- c(ci_values, 1 - ci_values)

  if(x_res == "daily"){
  x_draw <- x_draw %>%
    group_by(date) %>%
    summarize(sci_lower = quantile(sci, ci_values[[1]], na.rm=TRUE), sci_upper = quantile(sci, ci_values[[2]], na.rm=TRUE))
  } else if (x_res == "monthly"){
    x_draw <- x_draw %>%
      group_by(yearmon) %>%
      summarize(sci_lower = quantile(sci, ci_values[[1]], na.rm=TRUE), sci_upper = quantile(sci, ci_values[[2]], na.rm=TRUE))
  }

	return(x_draw)
}


