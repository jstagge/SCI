#' Plotting function for spi_ts class
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @method plot sci_ts
#' @export
plot.sci_ts <- function(x, plot_type = "ts"){
  require(tidyverse)
  require(zoo)
  require(ggplot2)

  ### Find the class of the time series
  x_res <- x$resolution

  if(sum(class(x) %in% "sci_ts") == 0){
    stop("Must be a sci_ts object")
  }

  #########
  ### If orig_ts
  #########
  if(x$stage == "orig_ts"){
    plot_df <- fortify(x$orig_ts, melt = TRUE)

      p <- ggplot(aes(x = Index, y = Value), data = plot_df) %>%
        + geom_line() %>%
        + xlab( "Date") %>%
        + scale_y_continuous(name = "x") %>%
        + theme_classic(12)
      p
  }


  #########
  ### If roll_mean
  #########
  if(x$stage == "roll_mean"){
    plot_df <- fortify(x$spi_ts, melt = TRUE)

    x_res <- x$resolution
    x_window <- x$window
    if(x_res == "daily"){y_label <- paste0("Rolling Mean of ", x_window, " Days")}
    if(x_res == "monthly"){y_label <- paste0("Rolling Mean of ", x_window, " Months")}

  ### Monthly breaks in case you need them
  month_breaks <- c(yday(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month")), 365)
  month_labels <- c(as.character(month(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month"), label=TRUE)), "Jan")

  #### Time series plot
   if(plot_type == "ts"){
     p <- ggplot(aes(x = Index, y = Value), data = plot_df) %>%
        + geom_line() %>%
        + xlab( "Date") %>%
        + scale_y_continuous(name = y_label) %>%
        + theme_classic(12)
      p
    }

  #### Seasonal plot
   if(plot_type == "seasonal"){
     plot_df <- x$df

     if(x_res == "daily"){
       p <- ggplot(aes(x = jdate, y = roll_mean, group = year), data = plot_df) %>%
        + geom_line() %>%
        + scale_x_continuous(name = "Julian Day", breaks=month_breaks, expand = c(0,0), sec.axis = sec_axis(~ . + 0, breaks = month_breaks, labels = month_labels)) %>%
        + scale_y_continuous(name = y_label) %>%
        + theme_classic(12)
      p
    } else if(x_res == "monthly"){
      p <- ggplot(aes(x = month, y = roll_mean, group = year), data = plot_df) %>%
       + geom_line() %>%
       + scale_x_continuous(name = "Month", breaks=seq(1,12), expand = c(0,0)) %>%
       + scale_y_continuous(name = y_label) %>%
       + theme_classic(12)
     p
    }
   }

   #### NA plot
    if(plot_type == "nas"){
      plot_df <- x$df
      max_na_prop <- x$max_na_prop

      if(x_res == "daily"){
        p <- ggplot(aes(x = date, y = na_prop), data = plot_df) %>%
         + geom_line() %>%
         + geom_point(data = filter(plot_df, na_prop >= max_na_prop), colour = "red") %>%
         + geom_hline(yintercept = max_na_prop, colour = "red", linetype = "dashed") %>%
         + scale_x_date(name = "Date") %>%
         + scale_y_continuous(name = "Proportion of NA Days in Moving Window") %>%
         + theme_classic(12)
       p
      } else if(x_res == "monthly"){
        p <- ggplot(aes(x = yearmon, y = na_prop), data = plot_df) %>%
         + geom_line() %>%
         + geom_point(data = filter(plot_df, na_prop >= max_na_prop), colour = "red") %>%
         + geom_hline(yintercept = max_na_prop, colour = "red", linetype = "dashed") %>%
         + xlab("Date") %>%
         + scale_y_continuous(name = "Proportion of NA Months in Moving Window") %>%
         + theme_classic(12)
       p
      }

    }

    #### Zero plot
    if(plot_type == "zeros"){
      plot_df <- x$df

      if(x_res == "daily"){
        p <- ggplot(aes(x = date, y = roll_dry_days), data = plot_df) %>%
         + geom_line() %>%
        + geom_hline(yintercept = x_window, colour = "red", linetype = "solid", alpha = 0.5) %>%
         + geom_point(data = filter(plot_df, roll_dry_days == x_window), colour = "red") %>%
         + scale_x_date(name = "Date") %>%
         + scale_y_continuous(name = "Zero Days in Moving Window") %>%
         + theme_classic(12)
       p
      } else if(x_res == "monthly"){
        p <- ggplot(aes(x = yearmon, y = roll_dry_days), data = plot_df) %>%
         + geom_line() %>%
         + geom_hline(yintercept = x_window, colour = "red", linetype = "solid", alpha = 0.5) %>%
         + geom_point(data = filter(plot_df, na_prop == x_window), colour = "red") %>%
         + xlab( "Date") %>%
         + scale_y_continuous(name = "Zero Months in Moving Window") %>%
         + theme_classic(12)
       p
      }

    }
  } ### Close out bracket for rolling mean

  #########
  ### If roll_mean
  #########
  if(x$stage == "roll_mean"){
    plot_df <- fortify(x$spi_ts, melt = TRUE)

    x_res <- x$resolution
    x_window <- x$window
    if(x_res == "daily"){y_label <- paste0("Rolling Mean of ", x_window, " Days")}
    if(x_res == "monthly"){y_label <- paste0("Rolling Mean of ", x_window, " Months")}

  ### Monthly breaks in case you need them
  month_breaks <- c(yday(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month")), 365)
  month_labels <- c(as.character(month(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month"), label=TRUE)), "Jan")

  #### Time series plot
  if(plot_type == "sci"){
    y_label <- "SCI"
    plot_df <- spi_results$df

    if(x_res == "daily"){
      p <- ggplot(aes(x = date, y = sci), data = plot_df) %>%
        + geom_ribbon(aes(ymin = sci_lower, ymax = sci_upper), fill = "grey80") %>%
        + geom_hline(yintercept = 0, colour = "grey60") %>%
        + geom_line() %>%
        + xlab( "Date") %>%
        + scale_y_continuous(name = y_label) %>%
        + theme_classic(12)
    }
    if(x_res == "monthly"){
      p <- ggplot(aes(x = yearmon, y = sci), data = plot_df) %>%
        + geom_hline(yintercept = 0, colour = "grey80") %>%
         + geom_line() %>%
         + xlab( "Date") %>%
         + scale_y_continuous(name = y_label) %>%
         + theme_classic(12)
    }


     p
    }
  }

  return(p)
}





#' Plotting function for spi_fit class
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @method plot sci_fit
#' @export
plot.sci_fit <- function(x, plot_type = "parameter"){
  require(tidyverse)
  require(zoo)

  if(plot_type == "parameter"){
    p <- param_plot(x)
  }

  if(plot_type == "qq"){

  }

  if(plot_type == "dens"){

  }

  return(p)
}



#' Parameter plot
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
param_plot <- function(x){
  ### Find the class of the time series
  x_res <- x$roll_mean$resolution

  ### Monthly breaks in case you need them
  month_breaks <- c(yday(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month")), 365)
  month_labels <- c(as.character(month(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month"), label=TRUE)), "Jan")

  ### Extract estimates
  estimate_mat <- sapply(x$fit, function(x){x$estimate})
  param_names <- rownames(estimate_mat)

  sd_mat <- sapply(x$fit, function(x){x$sd})

  ### Create plotting dataframes
  if(x_res == "daily"){
    plot_df <- data.frame(jdate = seq(1,365), as.data.frame(t(estimate_mat)))
    sd_df <- data.frame(jdate = seq(1,365), as.data.frame(t(sd_mat)))
      if(x$roll_mean$p0 == TRUE){
        zero_df <- data.frame(jdate = seq(1,365), parameter = "p0", estimate = x$n_zero/x$nn)
        param_names <- c(param_names, "p0")
      }
  } else if (x_res == "monthly"){
    plot_df <- data.frame(month = seq(1,12), as.data.frame(t(estimate_mat)))
    sd_df <- data.frame(month = seq(1,12), as.data.frame(t(sd_mat)))
      if(x$roll_mean$p0){
        zero_df <- data.frame(jdate = seq(1,365), parameter = "p0", estimate = x$n_zero/x$nn)
        param_names <- c(param_names, "p0")
      }
  }

  ### Data manipulation
  plot_df <- plot_df %>%
    pivot_longer(-1)
  sd_df <- sd_df %>%
    pivot_longer(-1)

  colnames(plot_df)[[2]] <- "parameter"
  colnames(plot_df)[[3]] <- "estimate"
  colnames(sd_df)[[2]] <- "parameter"
  colnames(sd_df)[[3]] <- "sd"

  if(x$roll_mean$p0 == TRUE){
    plot_df <- plot_df %>%
      bind_rows(zero_df)
  }

  plot_df <- plot_df %>%
    left_join(sd_df, by = c(colnames(plot_df)[1], "parameter"))

  plot_df$parameter <- factor(plot_df$parameter, levels = param_names)

  plot_df <- plot_df %>%
    mutate(ylower = estimate - 1.96*sd, yupper = estimate + 1.96*sd)

  ### Plotting function
  if(x_res == "daily"){
  p <- ggplot(plot_df, aes(x=jdate, y=estimate)) %>%
      + geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "grey50", alpha = 0.5) %>%
      + geom_line() %>%
      + facet_grid(parameter ~ ., scales = "free_y") %>%
      + scale_x_continuous(name = "Julian Day", breaks=month_breaks, expand = c(0,0), sec.axis = sec_axis(~ . + 0, breaks = month_breaks, labels = month_labels)) %>%
      + scale_y_continuous(name = "Parameter estimate") %>%
      + theme_bw(12)
  } else if (x_res == "monthly"){
    p <- ggplot(plot_df, aes(x=month, y=estimate)) %>%
        + geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "grey50", alpha = 0.5) %>%
        + geom_line() %>%
        + facet_grid(parameter ~ ., scales = "free_y") %>%
        + scale_x_continuous(name = "Month", expand = c(0,0)) %>%
        + scale_y_continuous(name = "Parameter estimate") %>%
        + theme_bw(12)
  }

p
return(p)
}



#' Density plot
#'
#' @param x Fill in
#' @param window Fill in 
#' @return An Lmoment fit.
#' @export
dens_plot <- function(x){
  ### Find the class of the time series
  x_res <- x$roll_mean$resolution


    hist_df <- data.frame(value = x$fit[[1]]$data)

    value_range <- c(min(hist_df$value), max(hist_df$value))

    ggplot(hist_df, aes(x=value)) %>%
      + geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "grey80", bins = 20) %>%
      + geom_density(aes(y = after_stat(density)), bw = 20) %>%
      + theme_bw(12)


  ### Extract estimates
  estimate_mat <- sapply(x$fit, function(x){x$estimate})
  param_names <- rownames(estimate_mat)


}
