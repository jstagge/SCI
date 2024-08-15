## code to prepare `DATASET` dataset goes here
###########################################################################
###  Load functions
###########################################################################
require(tidyverse)

### To download precip
require(rnoaa)

require(zoo)

select <- dplyr::select

###########################################################################
## Set initial values
###########################################################################
### Set seed so everyone gets the same random numbers (reproducible example)
set.seed(7890)

###########################################################################
## Download Ohio data
###########################################################################
### Ohio
precip_df <- meteo_tidy_ghcnd(stationid="USC00334979",var="prcp")

### Create daily series
oh_precip_daily <- precip_df %>%
  select(date, prcp) %>%
  mutate(prcp_daily_mm = as.numeric(as.character(prcp))/10) %>%
  select(-prcp)

#head(oh_precip_daily)
#ggplot(oh_precip_daily, aes(x=date, y=prcp_daily_mm)) + geom_line() + theme_classic()

oh_precip_daily_ts <- zoo(oh_precip_daily$prcp_daily_mm, oh_precip_daily$date)
#plot(oh_precip_daily_ts)

### Create a monthly series
oh_precip_monthly <- oh_precip_daily %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(prcp_monthly_mean_mm_day = mean(prcp_daily_mm, na.rm=TRUE), count = n()) %>%
  mutate(year_mon = zoo::as.yearmon(paste0(year, "-", month))) %>%
  mutate(date = ceiling_date(as.Date(paste0(year, "-", month, "-15")), "month") - days(1)) %>%
  ungroup()

#  ggplot(precip_monthly, aes(x=year_mon, y=prcp_monthly_mean_mm_day)) + geom_line() + theme_classic()

oh_precip_monthly_ts <- zoo(oh_precip_monthly$prcp_monthly_mean_mm_day, oh_precip_monthly$year_mon)
#plot(precip_monthly_ts)


#use_data(x, y, internal = TRUE) 

usethis::use_data(oh_precip_daily, oh_precip_daily_ts, oh_precip_monthly, oh_precip_monthly_ts, overwrite = TRUE) 


###########################################################################
## Download Arizona data
###########################################################################
precip_df <- meteo_tidy_ghcnd(stationid="USC00028619",var="prcp")

### Create daily series
az_precip_daily <- precip_df %>%
  select(date, prcp) %>%
  mutate(prcp_daily_mm = as.numeric(as.character(prcp))/10) %>%
  select(-prcp)

#head(oh_precip_daily)
#ggplot(az_precip_daily, aes(x=date, y=prcp_daily_mm)) + geom_line() + theme_classic()

az_precip_daily_ts <- zoo(az_precip_daily$prcp_daily_mm, az_precip_daily$date)
#plot(az_precip_daily_ts)

### Create a monthly series
az_precip_monthly <- az_precip_daily %>%
  mutate(month = month(date), year = year(date)) %>%
  group_by(year, month) %>%
  summarize(prcp_monthly_mean_mm_day = mean(prcp_daily_mm, na.rm=TRUE), count = n()) %>%
  mutate(year_mon = zoo::as.yearmon(paste0(year, "-", month))) %>%
  mutate(date = ceiling_date(as.Date(paste0(year, "-", month, "-15")), "month") - days(1)) %>%
  ungroup()

#  ggplot(az_precip_monthly, aes(x=year_mon, y=prcp_monthly_mean_mm_day)) + geom_line() + theme_classic()

az_precip_monthly_ts <- zoo(az_precip_monthly$prcp_monthly_mean_mm_day, az_precip_monthly$year_mon)
#plot(az_precip_monthly_ts)


#use_data(x, y, internal = TRUE) 

usethis::use_data(az_precip_daily, az_precip_daily_ts, az_precip_monthly, az_precip_monthly_ts, overwrite = TRUE) 
