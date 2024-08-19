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
      + theme_bw(12) %>%
      + theme(panel.grid.minor = element_blank())
  } else if (x_res == "monthly"){
    p <- ggplot(plot_df, aes(x=month, y=estimate)) %>%
        + geom_ribbon(aes(ymin = ylower, ymax = yupper), fill = "grey50", alpha = 0.5) %>%
        + geom_line() %>%
        + facet_grid(parameter ~ ., scales = "free_y") %>%
        + scale_x_continuous(name = "Month", expand = c(0,0)) %>%
        + scale_y_continuous(name = "Parameter estimate") %>%
        + theme_bw(12) %>%
        + theme(panel.grid.minor = element_blank())
  }

p
return(p)
}



#' Density calculation
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
dens_calc <- function(x){
  ### Find the class of the time series
  x_res <- x$roll_mean$resolution
  distr <- x$distr

  ### Extract estimates
  estimate_mat <- sapply(x$fit, function(x){x$estimate})
  param_names <- rownames(estimate_mat)
  ddistr <- match.fun(paste("d",distr,sep=""))
  pdistr <- match.fun(paste("p",distr,sep=""))
  qdistr <- match.fun(paste("q",distr,sep=""))

  ### Monthly breaks in case you need them
  if(x_res == "daily"){
    date_breaks <- seq(as.Date("1900-02-01"), as.Date("1901-1-31"), by = "1 month") -1
    date_labels <- as.character(format(date_breaks, "%b %d"))
    date_breaks <- yday(date_breaks)
  } else if (x_res == "monthly"){
    date_breaks <- seq(1,12)
    date_labels <- c(as.character(month(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month"), label=TRUE)))
  }

  ### Loop through and calculate density
  for(j in seq(1, length(date_breaks))){
    date_j <- date_breaks[[j]]
    label_j <- date_labels[[j]]

    ### Extract histogram
    hist_temp <- data.frame(value = x$fit[[date_j]]$data, label = label_j) %>%
      mutate(ecdf = (rank(value))/(length(value) + 1)) %>%
      mutate(theor =  do.call(qdistr,c(list(ecdf),estimate_mat[,date_j])))

    ### Choose range to plot fitted distribution
    value_range <- c(min(hist_temp$value), max(hist_temp$value))
    x_buffer <- abs(diff(value_range)*0.25)
    value_range <- c(value_range[[1]] - x_buffer, value_range[[2]] + x_buffer)
    x_points <- seq(value_range[[1]], value_range[[2]], length.out = 100)

    ### Calculate the density
    fit_dens_temp <- do.call(ddistr,c(list(x_points),estimate_mat[,date_j]))
    fit_cum_temp <- do.call(pdistr,c(list(x_points),estimate_mat[,date_j]))

    fit_temp <- data.frame(value = x_points, dens = fit_dens_temp, cum = fit_cum_temp, label = label_j)# %>%
    #  mutate(count = dens * n_hist * bw)

    if(j == 1){
      fit_dens <- fit_temp
      hist_df <- hist_temp
    } else {
      fit_dens <- fit_dens %>%
        bind_rows(fit_temp)
      hist_df <- hist_df %>%
        bind_rows(hist_temp)
    }

  }

  hist_df$label <- factor(hist_df$label, levels = date_labels)
  fit_dens$label <- factor(fit_dens$label, levels = date_labels)

  return(list(hist_df = hist_df, fit_dens = fit_dens))
}



#' Density calculation
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
dens_plot <- function(x, ...){
  UseMethod("dens_plot")
}

#' @rdname dens_plot
#' @method  dens_plot sci_fit
#' @export
dens_plot.sci_fit <- function(x){
  plot_list <- dens_calc(x)
  hist_df <- plot_list$hist_df
  fit_dens <- plot_list$fit_dens

  hist_df$colour = "Observed"
  fit_dens$colour = "Fit"

  #binwidth = bw, 

  p <- ggplot(hist_df, aes(x=value)) %>%
     + geom_histogram(aes(y = after_stat(density)),colour = NA, fill = "grey87", alpha = 1) %>%
     + geom_density(aes(colour = colour)) %>%
     + geom_line(data = fit_dens, aes(y=dens, colour = colour)) %>%
     + scale_colour_manual(name = "", values = c("black", "red"), breaks = c("Observed", "Fit")) %>%
     + scale_x_continuous(name = "Value") %>%
     + scale_y_continuous(name = "Density") %>%
     + facet_wrap(.~label) %>%
     + theme_bw(12) %>%
     + theme(panel.grid.minor = element_blank()) %>%
     + theme(legend.position="bottom")

  return(p)
}

#' Cumulative Plot
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
cum_plot <- function(x, ...){
  UseMethod("cum_plot")
}

#' @rdname cum_plot
#' @method  cum_plot sci_fit
#' @export
cum_plot.sci_fit <- function(x){
  plot_list <- dens_calc(x)
  hist_df <- plot_list$hist_df
  fit_dens <- plot_list$fit_dens

  hist_df$colour = "Observed"
  fit_dens$colour = "Fit"

  p <- ggplot(hist_df, aes(x=value)) %>%
    + geom_point(aes(y=ecdf, colour = colour)) %>%
    + geom_line(data = fit_dens, aes(y=cum, colour = colour)) %>%
     + scale_colour_manual(name = "", values = c("black", "red"), breaks = c("Observed", "Fit")) %>%
     + scale_x_continuous(name = "Value") %>%
     + scale_y_continuous(name = "Cumulative Prob") %>%
     + facet_wrap(.~label) %>%
     + theme_bw(12) %>%
     + theme(panel.grid.minor = element_blank()) %>%
     + theme(legend.position="bottom")

  return(p)
}

#' QQ Plot
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @export
qq_plot <- function(x, ...){
  UseMethod("qq_plot")
}

#' @rdname qq_plot
#' @method  qq_plot sci_fit
#' @export
qq_plot.sci_fit <- function(x){
  plot_list <- dens_calc(x)
  hist_df <- plot_list$hist_df
  fit_dens <- plot_list$fit_dens

  hist_df$colour = "Observed"
  fit_dens$colour = "Fit"

  p <- ggplot(hist_df, aes(x = theor, y=value)) %>%
    + geom_abline(slope = 1, intercept = 0, colour = "grey75") %>%
    + geom_point() %>%
     + scale_colour_manual(name = "", values = c("black", "red"), breaks = c("Observed", "Fit")) %>%
     + scale_x_continuous(name = "Theoretical") %>%
     + scale_y_continuous(name = "Sample") %>%
     + facet_wrap(.~label, scales = "free") %>%
    # + coord_fixed() %>%
     + theme_bw(12) %>%
     + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) %>%
     + theme(legend.position="bottom")

  return(p)
}




#' Plotting function for spi_fit class
#'
#' @param x Fill in
#' @param window Fill in
#' @return An Lmoment fit.
#' @method  plot sci_gof
#' @export
plot.sci_gof <- function(x){
  require(tidyverse)
  require(zoo)

  gof_df <- x$gof


  if(dim(x$gof)[1] == 365){
    plot_indices <- gof_df %>%
      dplyr::select(jdate, aic, sw_p, cvm, ad) %>%
      pivot_longer(-jdate)

    ### Extract just the rejected days
    sw_rejected <- gof_df %>%
      filter(sw_p <= 0.05) %>%
      dplyr::select(jdate, sw_p) %>%
      pivot_longer(-jdate)

    cvm_rejected <- gof_df %>%
        filter(cvmtest == "rejected") %>%
        dplyr::select(jdate, cvm) %>%
        pivot_longer(-jdate)

    ad_rejected <- gof_df %>%
      filter(adtest == "rejected") %>%
      dplyr::select(jdate, ad) %>%
      pivot_longer(-jdate)
  } else if(dim(x$gof)[1] == 12){
    plot_indices <- gof_df %>%
      dplyr::select(month, aic, sw_p, cvm, ad) %>%
      pivot_longer(-month) %>%

    ### Extract just the rejected days
    sw_rejected <- gof_df %>%
      filter(sw_p <= 0.05) %>%
      dplyr::select(month, sw_p) %>%
      pivot_longer(-month)

    cvm_rejected <- gof_df %>%
        filter(cvmtest == "rejected") %>%
        dplyr::select(month, cvm) %>%
        pivot_longer(-month)

    ad_rejected <- gof_df %>%
      filter(adtest == "rejected") %>%
      dplyr::select(month, ad) %>%
      pivot_longer(-month)
  }

  plot_indices <- plot_indices %>%
    mutate(name = factor(name, levels = c("aic",  "sw_p", "cvm", "ad"), labels = c("AIC", "Shapiro-Wilks p", "Cramer-Von Mises", "Anderson-Darling")))

  rejected_df <- sw_rejected %>%
    bind_rows(cvm_rejected) %>%
    bind_rows(ad_rejected)  %>%
    mutate(name = factor(name, levels = c("aic", "sw_p", "cvm", "ad"), labels = c("AIC",  "Shapiro-Wilks p", "Cramer-Von Mises", "Anderson-Darling")))

  if(dim(x$gof)[1] == 365){
    month_breaks <- c(yday(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month")), 365)
    month_labels <- c(as.character(month(seq(as.Date("1900-01-01"), as.Date("1900-12-31"), by = "1 month"), label=TRUE)), "Jan")

    p <-  ggplot(plot_indices, aes(x=jdate, y=value)) %>%
      + geom_point() %>%
      + geom_point(data = rejected_df, colour = "red") %>%
      + facet_grid(name~., scales = "free_y") %>%
      + scale_x_continuous(name = "Julian Day", breaks=month_breaks, expand = c(0,0), sec.axis = sec_axis(~ . + 0, breaks = month_breaks, labels = month_labels)) %>%
      + theme_bw(12) %>%
      + theme(panel.grid.minor = element_blank())
  } else {
    p <-  ggplot(plot_indices, aes(x=month, y=value)) %>%
      + geom_point() %>%
      + geom_point(data = rejected_df, colour = "red") %>%
      + facet_grid(name~., scales = "free_y") %>%
      + scale_x_continuous(name = "Month", breaks=seq(1,12), expand = c(0,0), sec.axis = sec_axis(~ . + 0, breaks = seq(1,12), labels = month_labels)) %>%
      + theme_bw(12) %>%
      + theme(panel.grid.minor = element_blank())
  }


  return(p)
}
