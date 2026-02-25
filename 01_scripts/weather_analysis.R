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
weather$Date <- parse_date_time(weather$Date, "Y-m-d")
weather$month_year <- format(as.Date(weather$Date), "%Y-%m") # add year-month column
weather$month <- format(as.Date(weather$Date), "%m") # add month column
weather$day <- format(as.Date(weather$Date), "%d") # add day column
weather$year <- format(as.Date(weather$Date), "%Y") # add year column

weather_clean <- weather %>%
  mutate(season = case_when(
    month == "01" ~ "wi",
    month == "02" ~ "wi",
    month == "12" ~ "wi",
    month == "03" ~ "sp",
    month == "04" ~ "sp",
    month == "05" ~ "sp",
    month == "06" ~ "su",
    month == "07" ~ "su",
    month == "08" ~ "su",
    month == "09" ~ "fa",
    month == "10" ~ "fa",
    month == "11" ~ "fa"
  )) %>%
  mutate(grow_season = case_when(
    month == "04" ~ "Y",
    month == "05" ~ "Y",
    month == "06" & day == "01" ~ "Y",
    month == "06" & day == "02" ~ "Y",
    month == "06" & day == "03" ~ "Y",
    month == "06" & day == "04" ~ "Y",
    month == "06" & day == "05" ~ "Y",
    month == "06" & day == "06" ~ "Y",
    month == "06" & day == "07" ~ "Y",
    month == "06" & day == "08" ~ "Y",
    month == "06" & day == "09" ~ "Y",
    month == "06" & day == "10" ~ "Y",
    month == "06" & day == "11" ~ "Y",
    month == "06" & day == "12" ~ "Y",
    month == "06" & day == "13" ~ "Y",
    month == "06" & day == "14" ~ "Y",
    month == "06" & day == "15" ~ "Y",
    TRUE ~ "N"
  ))

# summarize by month and year
month_weather <- weather_clean %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2025-05-31")) %>%
  dplyr::group_by(year, month_year) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm),
            mean.min.temp = round(mean(minTemp_C), 3),
            mean.max.temp = round(mean(maxTemp_C),3),
            mean.temp = round(mean(AveTemp_C),3))

# summarize by year
year_weather <-  weather_clean %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2024-12-31")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm), # annual precip
                   mean.min.temp = round(mean(minTemp_C), 3), # average minimum daily temperature over the year
                   mean.max.temp = round(mean(maxTemp_C),3), # average maximum daily temperature over the year
                   mean.temp = round(mean(AveTemp_C),3), # average daily temperature over the year
                   mean.range.temp = round(mean(maxTemp_C-minTemp_C), 3)) # average daily range of temperature over the year

# summarize by season
season_weather <- weather_clean %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2025-05-31")) %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm), # total seasonal precip
                   mean.min.temp = round(mean(minTemp_C), 3), # average minimum daily temperature over the season
                   mean.max.temp = round(mean(maxTemp_C),3), # average maximum daily temperature over the season
                   mean.temp = round(mean(AveTemp_C),3), # average daily temperature over the season
                   mean.range.temp = round(mean(maxTemp_C-minTemp_C), 3)) # average daily range of temperature over the season

weather_gs <- weather_clean %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June 9th
         Date <= as.Date("2025-06-09")) %>%
  filter(grow_season == "Y")%>%
  dplyr::group_by(year) %>%
  dplyr::summarize(gs.precip = sum(total_precip_mm), # total seasonal precip
                   gs.mean.min.temp = round(mean(minTemp_C), 3), # average minimum daily temperature over the season
                   gs.mean.max.temp = round(mean(maxTemp_C),3), # average maximum daily temperature over the season
                   gs.mean.temp = round(mean(AveTemp_C),3), # average daily temperature over the season
                   gs.mean.range.temp = round(mean(maxTemp_C-minTemp_C), 3)) # average daily range of temperature over the season

# summarize by previous year's weather
weather.prev.gs <- weather_gs[, -c(3, 4, 6)]
colnames(weather.prev.gs) <- paste("prev", colnames(weather.prev.gs), sep = ".")
weather.prev.gs$year <- as.character(as.numeric(weather.prev.gs$prev.year) + 1)

# summarize by 365 days prior to community sampling
wea.2015 <- weather_clean %>%
  filter(Date >= as.Date("2014-06-03"),  
         Date <= as.Date("2015-06-03")) %>%
  dplyr::summarize(year = 2015,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3)) 

wea.2016 <- weather_clean %>%
  filter(Date >= as.Date("2015-05-10"),  
         Date <= as.Date("2016-05-10")) %>%
  dplyr::summarize(year = 2016,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2017 <- weather_clean %>%
  filter(Date >= as.Date("2016-05-15"),  
         Date <= as.Date("2017-05-15")) %>%
  dplyr::summarize(year = 2017,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2018 <- weather_clean %>%
  filter(Date >= as.Date("2017-05-07"),  
         Date <= as.Date("2018-05-07")) %>%
  dplyr::summarize(year = 2018,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2019 <- weather_clean %>%
  filter(Date >= as.Date("2018-05-03"),  
         Date <= as.Date("2019-05-03")) %>%
  dplyr::summarize(year = 2019,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2020 <- weather_clean %>%
  filter(Date >= as.Date("2019-05-18"),  
         Date <= as.Date("2020-05-18")) %>%
  dplyr::summarize(year = 2020,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2021 <- weather_clean %>%
  filter(Date >= as.Date("2020-05-13"),  
         Date <= as.Date("2021-05-13")) %>%
  dplyr::summarize(year = 2021,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2022 <- weather_clean %>%
  filter(Date >= as.Date("2021-05-13"),  
         Date <= as.Date("2022-05-13")) %>%
  dplyr::summarize(year = 2022,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2023 <- weather_clean %>%
  filter(Date >= as.Date("2022-05-15"),  
         Date <= as.Date("2023-05-15")) %>%
  dplyr::summarize(year = 2023,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2024 <- weather_clean %>%
  filter(Date >= as.Date("2023-05-07"),  
         Date <= as.Date("2024-05-07")) %>%
  dplyr::summarize(year = 2024,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

wea.2025 <- weather_clean %>%
  filter(Date >= as.Date("2024-05-14"),  
         Date <= as.Date("2025-05-14")) %>%
  dplyr::summarize(year = 2025,
                   tot.precip = sum(total_precip_mm),
                   mean.temp = round(mean(AveTemp_C), 3))

weather.prev.365 <- rbind(wea.2015, wea.2016, wea.2017, wea.2018,
                          wea.2019, wea.2020, wea.2021, wea.2022,
                          wea.2023, wea.2024, wea.2025)

rm(wea.2015, wea.2016, wea.2017, wea.2018,
   wea.2019, wea.2020, wea.2021, wea.2022,
   wea.2023, wea.2024, wea.2025)

# make yearly stats table for analysis
fin_weather <- gs_weather %>%
  pivot_wider(
    names_from = season,
    values_from = c(tot.precip, mean.min.temp, mean.max.temp, mean.temp, mean.range.temp)
  ) %>%
  left_join(year_weather)

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
#### Yearly totals
ggplot(year_weather, aes(x = year, y = tot.precip))+
  geom_point()+
  ylab("Yearly Precipitation Total (mm)")+
  xlab("Date")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

#### Monthly Totals
ggplot(month_weather, aes(x = month_year, y = tot.precip))+
  geom_point()+
  ylab("Monthly Precipitation Total (mm)")+
  xlab("Date")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


#### Daily Totals split by month and year
ggplot(weather, aes(x = month, y = total_precip_mm))+
  geom_point()+
  ylab("Daily Precipitation (mm)")+
  xlab("Date")+
  theme_classic()+
  facet_wrap(~year)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

### Temperature
#### Yearly averages
ggplot(long_year_weather[long_year_weather$temp_stat != "mean.range.temp", ], aes(x = year, y = temp, color = temp_stat))+
  geom_point()+
  ylab("Daily Temperature (C)")+
  xlab("Year")+
  scale_color_discrete(name = "", labels = c("Maximum", "Minimum", "Overall"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

#### Monthly averages
ggplot(month_weather, aes(x = month_year, y = mean.temp))+
  geom_point()+
  ylab("Daily Temperature (C)")+
  xlab("Date")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
