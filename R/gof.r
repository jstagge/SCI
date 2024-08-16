#' Goodness of fit for SCI
#'
#' @param fit Fill in
#' @return An Lmoment fit.
#' @export

gof_sci <- function(fit, p0.center.mass=FALSE, sci.limit=Inf){
  require(fitdistrplus)

  ### Extract useful information
  x_res <- fit$roll_mean$resolution
  nn <- fit$nn
  ref_years <- fit$ref_years

  ### Use gofstat to get standard gof statistics
  gof_df <- t(sapply(fit$fit, gofstat))
  ### Dangerous to hard code
  gof_df <- as.data.frame(gof_df[,6:13])


  ### Add useful columns
  if(x_res == "daily"){
    gof_df <- gof_df %>%
      mutate(jdate = seq(1,365)) %>%
      mutate(n = nn)
  } else if (x_res == "monthly"){
    gof_df <- gof_df %>%
      mutate(month = seq(1,12))    %>%
      mutate(n = nn)
  }

  ### Add a shapiro wilks test on transformed values
  sci_values <- transformSCI(fit, p0.center.mass=p0.center.mass, sci.limit=sci.limit, ci = NA)

  ### Only the reference years
  sci_check_df <- sci_values$df
  sci_check_df <- sci_check_df %>%
     filter(year >= ref_years[[1]]) %>%
     filter(year <= ref_years[[2]])

  if(x_res == "daily"){
    sw_result <-  data.frame(t(sapply(1:365, shapiro_wilk_j, sci_df = sci_check_df, x_res = x_res)))
    sw_result <- sw_result %>%
      mutate(jdate = 1:365)
    gof_df <- gof_df %>%
        left_join(sw_result, by = "jdate")
  } else if (x_res == "monthly") {
    sw_result <-  data.frame(t(sapply(1:12, shapiro_wilk_j, sci_df = sci_check_df, x_res = x_res)))
    sw_result <- sw_result %>%
      mutate(month = 1:12)
    gof_df <- gof_df %>%
      left_join(sw_result, by = "month")
  }

  ### Fix columns
  if(x_res == "daily"){
    gof_df <- gof_df %>%
      dplyr::select(jdate, n, aic, bic, sw_stat, sw_p, cvm, cvmtest, ad, adtest)
  } else if (x_res == "monthly") {
    gof_df <- gof_df %>%
      dplyr::select(month, n, aic, bic, sw_stat, sw_p, cvm, cvmtest, ad, adtest)
  }

  gof_df$aic <- as.numeric(gof_df$aic)
  gof_df$bic <- as.numeric(gof_df$bic)
  gof_df$sw_stat <- as.numeric(gof_df$sw_stat)
  gof_df$sw_p <- as.numeric(gof_df$sw_p)
  gof_df$cvm <- as.numeric(gof_df$cvm)
  gof_df$cvmtest <- as.character(gof_df$cvmtest)
  gof_df$ad <- as.numeric(gof_df$ad)
  gof_df$adtest <- as.character(gof_df$adtest)


  results_list <- list(gof = gof_df, sci_df = sci_check_df, class = "sci_gof")

}



#' Shapiro Wilks test
#'
#' @param j Day or month
#' @return An Lmoment fit.
#' @export
shapiro_wilk_j <- function(j, sci_df, x_res){
  if(x_res == "daily"){
    sci_j <- sci_df %>%
        filter(jdate == j)
  } else if (x_res == "monthly"){
    sci_j <- sci_df %>%
        filter(month == j)
  }

  sw_test <- shapiro.test(sci_j$sci)

  return(list(sw_stat = sw_test$statistic, sw_p = sw_test$p.value))
}
