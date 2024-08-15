#' Summary function for sci_fit class
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @method summary sci_fit
#' @export
summary.sci_fit <- function(x, suppress = FALSE){

  x_res <- x$roll_mean$resolution

  estimate_mat <- sapply(x$fit, function(x){x$estimate})
  colnames(estimate_mat) <- as.character(seq(1,dim(estimate_mat)[2]))

  aic_mat <- sapply(x$fit, function(x){x$aic})

  sd_mat <- sapply(x$fit, function(x){x$sd})
  colnames(sd_mat) <- as.character(seq(1,dim(sd_mat)[2]))
  rownames(sd_mat) <- paste0(rownames(sd_mat), "_stderror")


  if(x_res == "daily"){
    summary_df <- data.frame(jdate = seq(1,365), as.data.frame(t(estimate_mat)))
  } else if (x_res == "monthly"){
    summary_df <- data.frame(month = seq(1,12), as.data.frame(t(estimate_mat)))
  }

  summary_df <- summary_df %>%
    mutate(n = x$nn, distr = x$distr, method = x$method) %>%
    mutate(p0 = x$n_zero/x$nn) %>%
    bind_cols(as.data.frame(t(sd_mat))) %>%
    mutate(aic = aic_mat, monitor = x$monitor)

  if(suppress == FALSE){
  message(paste0("Parameter estimates for ", x$distr, " distribution"))
  #message("\n")
  print(summary_df)
  }

  return(summary_df)
}
