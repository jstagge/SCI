#' Fit SCI
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
fitSCI <- function(x,
                             window,
                             distr,
                             method=c("mle", "lmom", "spline", "ecdf"),
                             ref_years = c(-Inf, Inf),
                             gof = TRUE,
                             bimodal_test = FALSE,
                             p0,
                             max_na_prop = 0.1,
                             scaling=c("no","max","sd"),
                             start.fun=dist.start,
                             start.fun.fix=FALSE
  ){

    if(sum(class(x) %in% "sci_ts") == 0){
      stop("Must be a sci_ts object")
    }

    ##############################################
    ### Apply rolling mean
    ##############################################
    x <- rollmean_calc(example_ts, window = window, max_na_prop = max_na_prop, p0 = p0)

    x_df <- x$df
    x_ts <- x$ts
    x_res <- x$resolution


    ##############################################
    ### Handle Reference Years
    ##############################################
    x_df <- x_df %>%
       filter(year >= ref_years[[1]]) %>%
       filter(year <= ref_years[[2]])

    ##############################################
    ### Process scaling
    ##############################################
    scaling <- match.arg(scaling)
    scale.val <- switch(scaling,
      no=1,
      max=max(x_df$roll_mean,na.rm=TRUE),
      sd=sd(x_df$roll_mean,na.rm=TRUE))
     x_df$roll_mean <- x_df$roll_mean/scale.val

     ##############################################
     ### I got rid of catch for distribution starting fit. Build that into MLE fit
     ##############################################


     ##############################################
     ### Bimodal test
     ##############################################
     if(bimodal_test == TRUE){
       bimodal_result <- bimodal_test(x_ts)
     }

     ##############################################
     ### Choose fit based on method
     ##############################################
     if (method == "mle"){
       fit_result <- sci_mle(x_df, resolution = x_res, distr = distr, p0 = p0, warn = warn, start.fun.fix = start.fun.fix, ref_years = ref_years)
    } else if (method == "lmom"){
       #fit_result <- sci_lmom(x_df, x_res, distr)
       stop("ecdf not yet implemented")
    } else if (method == "spline" ){
      #fit_result <- sci_spline(x_df, x_res, distr)
      stop("ecdf not yet implemented")
    } else if (method == "ecdf"){
      stop("ecdf not yet implemented")
    }


    ##############################################
    ### Add in the rolling mean time series to the object
    ##############################################
    fit_result$roll_mean <- x


    ##############################################
    ### Return result
    ##############################################
     return(fit_result)

}




#' Wrapper for MLE fit
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
sci_mle <- function(x_ts, resolution, distr, p0, warn, start.fun.fix, ref_years){

  require(fitdistrplus)

  if(x_res == "daily") {
    timestep_list <- 1:365
  } else if (x_res == "monthly") {
    timestep_list <- 1:12
  }

  x_fit <- sapply(timestep_list, sci_mle_j, x_ts = x_ts, x_res = x_res, distr = distr , p0 = p0, warn = warn, start.fun.fix = start.fun.fix)

  ### Create a fit object
  fit_result <- sci_fit(fit = x_fit[1,], roll_mean = NA, nn = unlist(x_fit[3,]), n_zero = unlist(x_fit[4,]), monitor = unlist(x_fit[2,]), method = "MLE", distr = distr, ref_years = ref_years)

  ### Return the fit object
  return(fit_result)
}



#' MLE fit for a single day or month j
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
sci_mle_j <- function(j, x_ts, distr, x_res, p0, warn = TRUE, start.fun.fix = FALSE){

  ### Extract the given day or month
  if(x_res == "daily") {x_j <- x_ts %>% filter(jdate == j)}
  if(x_res == "monthly") {x_j <- x_ts %>% filter(month == j)}

  #mledist.par$data <- mledist.par$data[is.finite(mledist.par$data)]

  ### Create tracking objects
  monitor_j <- NA
  mledist.par <- list()

  ### Extract values
  x_j <- x_j$roll_mean[is.finite(x_j$roll_mean)]

  if(length(x_j)==0){
      ### If there are no non-NA values in period NA
      x_fit <- NA ## if there are no data in the fitting period: NA
      monitor_j <- 4
      nn <- 0
      if(warn){
          if(x_res == "daily"){warning("All values in day ",j," are 'NA'")}
          if(x_res == "monthly"){warning("All values in month ",j," are 'NA'")}
      }
  } else if(all(x_j==x_j[1])){
      ### If all values are the same NA
      x_fit <- NA
      monitor_j <- 5
      nn <- length(x_j)
      if(warn){
          if(x_res == "daily"){warning("All values in day ",j," are constant, distribution not defined")}
          if(x_res == "monthly"){warning("All values in month ",j," are constant, distribution not defined")}
      }
  } else {
    ### Do the MLE fitting
    nn <- length(x_j)

    ### If there are zeros
    if(p0 == TRUE){
      np0 <- sum(x_j==0)
      x_j <- x_j[x_j > 0]
    }

      #if(p0.est>0){
          #mledist.par$data <- mledist.par$data[mledist.par$data>0]
          ## Catch that adds a single value close to zero if there are zeros in the
          ## fitting period.  This forces the distribution to tend towards zero,
          ## preventing a "gap" betwen 0 and data.
        #  mledist.par$data <- c(mledist.par$data,0.01*min(mledist.par$data,na.rm=TRUE))
      #}

    ### Try the start function
    start <- start.fun(x=x_j,distr=distr)
    fail.value <- start
    fail.value <- unlist(fail.value)
    if(!start.fun.fix){ fail.value[] <- NA }

    ### If the start function doesn't run
    if(any(is.na(unlist(start)))){
      if(any(is.na(unlist(start))) & warn)
          warning("starting values in month ",mm, " not properly estimated \n parameter are NA.")
          fail.value[] <- NA
          x_fit <- fail.value
          monitor_j <- 1
    } else {
      ### If the start function runs
      mledist.par$distr <- distr
      mledist.par$data <- x_j
      mledist.par$start <- start

      ### Run fitdist mle
      sink <- capture.output(
          #  x_fit <- try(suppressWarnings(do.call(mledist,mledist.par)),silent=TRUE)
            x_fit <- try(suppressWarnings(fitdist(x_j, distr = distr, method = "mle", start = start)), silent = TRUE)
      )

      ### Catch potential errors
      if(class(x_fit)=="try-error"){
          x_fit <- fail.value
          monitor_j <- 2
      } else if(x_fit$convergence > 0){
          x_fit <- fail.value
          monitor_j <- 3
      } else {
          monitor_j <- 0
          x_fit <- x_fit
      }

      ### Return warnings
      if(monitor_j > 1 & start.fun.fix & warn & x_res == "monthly"){warning("Maximum likelihood estimation failed for month ", j , "\n use starting values instead.")}
      if(monitor_j > 1 & start.fun.fix & warn & x_res == "daily"){warning("Maximum likelihood estimation failed for day ", j , "\n use starting values instead.")}
      if(monitor_j > 1 & !start.fun.fix & warn & x_res == "monthly"){warning("Maximum likelihood estimation failed for month ", j , "\n parameters are set to NA.")}
      if(monitor_j > 1 & !start.fun.fix & warn & x_res == "daily"){warning("Maximum likelihood estimation failed for day ", j , "\n parameters are set to NA.")}

    }
  }

    fit_result <- list(fit = NA, monitor = NA, nn = NA, n_zero = NA)
    fit_result$fit <- x_fit
    fit_result$monitor <- monitor_j
    fit_result$nn <- nn

    if(p0 == TRUE){
      fit_result$n_zero <- np0
    }

  #  x_fit$fit <- x_fit
  #  x_fit$monitor <- monitor_j
  #  x_fit$nn <- nn

    return(fit_result)

}

