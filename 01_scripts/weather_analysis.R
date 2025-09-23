# Weather Analysis
# Data cleaning portion adapted from code by Jenna Loesberg
# Robin Bradley
# robin.bradley@ubc.ca
# Created: 23 September 2025
# Last updated: 23 September 2025

# 1. Set-up ----
# Set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/LoggerData/weather station/WS_cleaned")

# Packages
library(tidyverse)
library(dplyr)

# Read in data
weather <- read.csv("cgop_weather_daily_interp.csv", header = TRUE)
# 2. Data Cleaning ----
## read in the cleaned, daily interpolated weather station data:
weather$Date <- parse_date_time(weather$Date, "Y-m-d") #may need to change the format to whatever R reads it in as
weather$month_year <- format(as.Date(weather$Date), "%Y-%m") # add year-month column
weather$month <- format(as.Date(weather$Date), "%m") # add month column
weather$year <- format(as.Date(weather$Date), "%Y") # add year column

# summarize by month and year
month_weather <- weather %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2025-05-31")) %>%
  dplyr::group_by(year, month_year) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm),
            mean.min.temp = round(mean(minTemp_C), 3),
            mean.max.temp = round(mean(maxTemp_C),3),
            mean.temp = round(mean(AveTemp_C),3))
# summarize by year
year_weather <-  weather %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2024-12-31")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm),
                   mean.min.temp = round(mean(minTemp_C), 3),
                   mean.max.temp = round(mean(maxTemp_C),3),
                   mean.temp = round(mean(AveTemp_C),3))
# pivot longer for plotting
long_year_weather <- year_weather %>%
  pivot_longer(
    cols = ends_with(".temp"),
    names_to = "temp_stat",
    values_to = "temp"
  )

# 3. How does ambient climate change over time in the experiment? ----
## Data exploration
### Precipitation
ggplot(year_weather, aes(x = year, y = tot.precip))+
  geom_point()+
  ylab("Daily Precipitation (mm)")+
  xlab("Date")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(weather, aes(x = month, y = total_precip_mm))+
  geom_point()+
  ylab("Daily Precipitation (mm)")+
  xlab("Date")+
  theme_classic()+
  facet_wrap(~year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

### Temperature
ggplot(long_year_weather, aes(x = year, y = temp, color = temp_stat))+
  geom_point()+
  ylab("Daily Temperature (C)")+
  xlab("Year")+
  scale_color_discrete(name = "", labels = c("Maximum", "Minimum", "Overall"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
