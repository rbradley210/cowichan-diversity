# Cowichan Plant Community and Weather Data Cleaning script
# Adapted from code by Lauren Smith (plants_cleanup.Rmd)
# email: robinbradley210@gmail.com
# created: June 2024
# last updated: 23 February 2026


# Set Up ----
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
library(vegan)
library(ggplot2)
library(reshape)

# 1. Diversity Data ----
## Read in data 
### Read in plot data 
plots <- read.csv("IDE_plotinfo.csv", header = TRUE)
names(plots)[names(plots) == "treatment"] <- "trt"

### Read in cover data
## Mid-growing season surveys only
### 2015-2021
cover_mids20152021 <- read.csv("Diversity/cowichan_community5_19_2021.csv", 
                               header = TRUE, na.strings = c("", " "), strip.white=TRUE) %>% 
  dplyr::select(-quadID, -notes, -X, -X.1, -X.2, -X.3, -X.4, -X.5)

### 2022
cover_mid2022 <- read.csv("Diversity/2022_cowichan_community.csv", 
                          header = TRUE, na.strings = c("", " "), strip.white=TRUE) %>% 
  slice(1:802) %>% 
  dplyr::select(-quadID, -notes)

### 2023
cover_mid2023 <- read.csv("Diversity/2023_cowichan_community.csv", 
                          header = TRUE, na.strings = c("", " "), strip.white=TRUE) %>% 
  slice(1:808) %>% 
  dplyr::select(-quadID, -notes)

### 2024
cover_mid2024 <- read.csv("Diversity/2024_cowichan_community.csv", 
                          header = TRUE, na.strings = c("", " "), strip.white=TRUE) %>% 
  slice(1:813) %>% 
  dplyr::select(-quadID, -notes)

### 2025
cover_mid2025 <- read.csv("Diversity/2025_cowichan_community.csv", 
                          header = TRUE, na.strings = c("", " "), strip.white=TRUE) %>% 
  slice(1:799) %>% 
  dplyr::select(-quadID, -notes)

## Cleaning cover data ----
### bind all diversity surveys together
cover_all <- rbind(cover_mids20152021,
                   cover_mid2022, 
                   cover_mid2023, 
                   cover_mid2024,
                   cover_mid2025)

### remove individual year dataframes
rm(cover_mids20152021)
rm(cover_mid2022)
rm(cover_mid2023)
rm(cover_mid2024)
rm(cover_mid2025)

# change cover column to numbers not characters
cover_all$cover <- as.numeric(as.character(cover_all$cover))  

# get rid of NAs in the cover column
cover_all$cover[cover_all$cover == 0] <- NA
cover_all <- cover_all %>%
  filter(cover != "NA")

# fix treatments
cover_all$treatment[cover_all$treatment == "Irrigated"] <- "irrigated"
cover_all$treatment[cover_all$treatment == "Control"] <- "control"
cover_all$treatment[cover_all$treatment == "Drought"] <- "drought"

# change plot, year, and treatment to factor
cover_all$treatment <- factor(cover_all$treatment, levels = c("control", "drought", "irrigated"))

cover_all$year <- as.factor(cover_all$year)
cover_all$year <- factor(cover_all$year, levels = c("2015", "2016", "2017", "2018", "2019", 
                                              "2020", "2021", "2022", "2023", "2024", "2025"))

cover_all$plot <- as.factor(cover_all$plot)
cover_all$plot <- factor(cover_all$plot, levels = c("1", "2", "3", "4", "5", "6", 
                                              "7", "8", "9", "10", "11", "12", 
                                              "13", "14", "15"))


# separate coarse cover from plant cover
# just plant cover
plants <- cover_all %>%
  filter(species != "Litter") %>%
  filter(species != "Bare Soil") %>%
  filter(species != "Moss") %>%
  filter(species != "total grass") 

# Fix plant name errors - spelling errors and unknown plants that were later identified
plants$species[plants$species == "Unkown furry"] <- "Lonicera hispidula"
plants$species[plants$species == "Unknown fuzzy"] <- "Lonicera hispidula"
plants$species[plants$species == "Mystery fuzzy"] <- "Lonicera hispidula"
plants$species[plants$species == "mystery honey suckle"] <- "Lonicera hispidula"
plants$species[plants$species == "Unknown H"] <- "Lonicera hispidula"

plants$species[plants$species == 'Unknown mint'] <- "Clinopodium douglasii"
plants$species[plants$species == 'Unknown F'] <- "Clinopodium douglasii"

plants$species[plants$species == 'Wood rush'] <- "Luzula sp."
plants$species[plants$species == 'wood rush'] <- "Luzula sp."
plants$species[plants$species == 'Luzula'] <- "Luzula sp."

plants$species[plants$species == 'Unknown B'] <- "Trifolium dubium"
#plants$species[plants_tr$species == 'Unknown C'] <- "Teesdalia nudicaulis"

plants$species[plants$species == "Grass B"] <- "Vulpia sp."
plants$species[plants$species == "Vulpia spp"] <- "Vulpia sp."

plants$species[plants$species == "Grass 9" & plants$plot == "14"] <- "Bromus hordeaceus"
plants$species[plants$species == "Grass 9"] <- "Bromus carinatus"
plants$species[plants$species == "Bromus hordaceous"] <- "Bromus hordeaceus"
plants$species[plants$species == "Bromus tectorum"] <- "Bromus sterilis"

plants$species[plants$species == "Fuzzy giant thistle"] <- "Cirsium vulgare"
plants$species[plants$species == "fuzzy giant thistle"] <- "Cirsium vulgare"

plants$species[plants$species == "Alium ampoplectens"] <- "Allium amplectens"

plants$species[plants$species == "mystery orchid (Platanthera from twitter?)"] <- "Platanthera sp."
plants$species[plants$species == "Platanthera spp"] <- "Platanthera sp."

plants$species[plants$species == "bur chervil/mystery carrot"] <- "Torilis arvensis"
plants$species[plants$species == "mystery carrot (not bur chervil)"] <- "Torilis arvensis"
plants$species[plants$species == 'Unknown G '] <- "Torilis arvensis"
plants$species[plants$species == "Unknown G"] <- "Torilis arvensis"

plants$species[plants$species == "Berberis nervosa"] <- "Berberis aquifolium"

plants$species[plants$species == "Pseudostuga menziesii"] <- "Pseudotsuga menziesii"

plants$species[plants$species == "Oemlaria cerasiformis"] <- "Oemleria cerasiformis"
plants$species[plants$species == "mystery oval pointy leaf"] <- "Chenopodium album"
plants$species[plants$species == "Triteleia spp"] <- "Triteleia sp."
plants$species[plants$species == "Dodecatheon hendersonii"] <- "Primula hendersonii"

# Fix errors from entering data
## 2017 Plot 14 Collinsia -> Claytonia
plants$species[plants$species == "Collinsia parviflora" & plants$plot == "14" & plants$year == "2017"] <- "Claytonia perfoliata"

## 2021 plot 4 Danthonia -> Elymus
plants$species[plants$species == "Danthonia californica" & plants$plot == "4" & plants$year == "2021"] <- "Elymus glaucus"

plants <- plants %>%
  group_by(plot, treatment, year, species)%>%
  dplyr::summarize(cover = sum(cover))


# Removing species
plants <- plants %>%
  filter(species != "Unknown weed") %>%
  filter(species != "Unknown Grass") %>%
  filter(species != "Unknown E") %>%
  filter(species != "Unknown D") %>%
  filter(species != "Unknown A") %>%
  filter(species != "Mystery 1") %>%
  filter(species != "Pseudotsuga menziesii") %>% # never survive
  filter(species != "Acer macrophyllum") %>% # never survive
  filter(species != "Cytisus scoparius") # these are pulled immediately

plants_allbromus <- plants

# Pooling invasive annual bromes
plants$species[plants$species == "Bromus hordeaceus"] <- "Annual Bromus"
plants$species[plants$species == "Bromus sterilis"] <- "Annual Bromus"

## Create list of unique species names
plant_species <- data.frame(unique(plants$species[plants$cover > 0]))

## Species-Plot Matrix ----
spe.matrix <- cast(plants, plot + year ~ species, value = 'cover', fun.aggregate = sum) %>% # convert to matrix
  unite("ID", plot:year)
spe.matrix[is.na(spe.matrix)] <- 0 # convert NAs to 0s
spe.matrix <- data.frame(spe.matrix, row.names = 1) # convert plot-year ID to row names

## Make plot-year metadata file ----
plot_year <- data.frame(plot_year = row.names(spe.matrix)) %>% 
  separate(col = plot_year,
           into = c("plot", "year"),
           sep = "_", remove = FALSE, convert=TRUE) %>%
  left_join(., plots, by = "plot")

# Setting variables to factors and assigning levels
plot_year$trt <- factor(plot_year$trt, levels = c("control","drought", "irrigated"))

plot_year$year <- as.factor(plot_year$year)
plot_year$year <- factor(plot_year$year, levels = c("2015", "2016", "2017", 
                                                    "2018", "2019", "2020", "2021", 
                                                    "2022", "2023", "2024", "2025"))
plot_year$plot <- as.factor(plot_year$plot)
plot_year$plot <- factor(plot_year$plot, levels = c("1", "2", "3", "4", "5", "6", 
                                                    "7", "8", "9", "10", "11", "12", 
                                                    "13", "14", "15"))

# 2. Weather Data ----
## Read in data
weather <- read.csv("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/LoggerData/weather station/WS_cleaned/cgop_weather_daily_interp.csv", header = TRUE)

## Fix and add date columns
weather$Date <- parse_date_time(weather$Date, "Y-m-d")
weather$month <- format(as.Date(weather$Date), "%m") # add month column
weather$day <- format(as.Date(weather$Date), "%d") # add day column
weather$year <- format(as.Date(weather$Date), "%Y") # add year column

## Filter to relevant dates and add season and growing season column
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

## summarize by year
year_weather <-  weather_clean %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2024-12-31")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm), # annual precip
                   mean.min.temp = round(mean(minTemp_C), 3), # average minimum daily temperature over the year
                   mean.max.temp = round(mean(maxTemp_C),3), # average maximum daily temperature over the year
                   mean.temp = round(mean(AveTemp_C),3), # average daily temperature over the year
                   mean.range.temp = round(mean(maxTemp_C-minTemp_C), 3)) # average daily range of temperature over the year

## summarize by season
season_weather <- weather_clean %>%
  filter(Date >= as.Date("2013-01-01"),  #before 2013 has NA's and 2025 data only goes up to June
         Date <= as.Date("2025-05-31")) %>%
  dplyr::group_by(year, season) %>%
  dplyr::summarize(tot.precip = sum(total_precip_mm), # total seasonal precip
                   mean.min.temp = round(mean(minTemp_C), 3), # average minimum daily temperature over the season
                   mean.max.temp = round(mean(maxTemp_C),3), # average maximum daily temperature over the season
                   mean.temp = round(mean(AveTemp_C),3), # average daily temperature over the season
                   mean.range.temp = round(mean(maxTemp_C-minTemp_C), 3)) # average daily range of temperature over the season

## pull previous summer's weather into own dataframe
season_weather$year<- as.numeric(season_weather$year)
weather.prev.su <- season_weather %>%
  filter(season == "su") %>%
  select(-c(2, 4, 5, 7)) %>%
  dplyr::rename(prev.year = year, 
                prev.su.mean.temp = mean.temp,
                prev.su.precip = tot.precip)%>%
  dplyr::mutate(year = prev.year + 1) # change 'year' column to following year values
weather.prev.su$prev.year<- as.character(weather.prev.su$prev.year)
weather.prev.su$year<- as.character(weather.prev.su$year)

## pull previous spring's weather into own dataframe
weather.prev.sp <- season_weather %>%
  filter(season == "sp") %>%
  select(-c(2, 4, 5, 7)) %>%
  dplyr::rename(prev.year = year, 
                prev.sp.mean.temp = mean.temp,
                prev.sp.precip = tot.precip)%>%
  dplyr::mutate(year = prev.year + 1) # change 'year' column to following year values
weather.prev.sp$prev.year<- as.character(weather.prev.sp$prev.year)
weather.prev.sp$year<- as.character(weather.prev.sp$year)

# summarize for growing season (April 1-June 15 every year)
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

# summarize by difference in g.s. weather from prev. year and 2016
# weather.diff.gs <- weather_gs[, -c(3, 4, 6)] %>%
#   mutate(temp.diff.prev.gs = gs.mean.temp - lag(gs.mean.temp))%>%
#   mutate(precip.diff.prev.gs = gs.precip - lag(gs.precip))%>%
#   mutate(temp.diff.2016.gs = gs.mean.temp - gs.mean.temp[weather_gs$year == "2016"])%>%
#   mutate(precip.diff.2016.gs = gs.precip - gs.precip[weather_gs$year == "2016"])

# summarize by 365 days prior to community sampling
# wea.2015 <- weather_clean %>%
#   filter(Date >= as.Date("2014-06-03"),  
#          Date <= as.Date("2015-06-03")) %>%
#   dplyr::summarize(year = 2015,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3)) 
# 
# wea.2016 <- weather_clean %>%
#   filter(Date >= as.Date("2015-05-10"),  
#          Date <= as.Date("2016-05-10")) %>%
#   dplyr::summarize(year = 2016,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2017 <- weather_clean %>%
#   filter(Date >= as.Date("2016-05-15"),  
#          Date <= as.Date("2017-05-15")) %>%
#   dplyr::summarize(year = 2017,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2018 <- weather_clean %>%
#   filter(Date >= as.Date("2017-05-07"),  
#          Date <= as.Date("2018-05-07")) %>%
#   dplyr::summarize(year = 2018,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2019 <- weather_clean %>%
#   filter(Date >= as.Date("2018-05-03"),  
#          Date <= as.Date("2019-05-03")) %>%
#   dplyr::summarize(year = 2019,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2020 <- weather_clean %>%
#   filter(Date >= as.Date("2019-05-18"),  
#          Date <= as.Date("2020-05-18")) %>%
#   dplyr::summarize(year = 2020,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2021 <- weather_clean %>%
#   filter(Date >= as.Date("2020-05-13"),  
#          Date <= as.Date("2021-05-13")) %>%
#   dplyr::summarize(year = 2021,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2022 <- weather_clean %>%
#   filter(Date >= as.Date("2021-05-13"),  
#          Date <= as.Date("2022-05-13")) %>%
#   dplyr::summarize(year = 2022,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2023 <- weather_clean %>%
#   filter(Date >= as.Date("2022-05-15"),  
#          Date <= as.Date("2023-05-15")) %>%
#   dplyr::summarize(year = 2023,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2024 <- weather_clean %>%
#   filter(Date >= as.Date("2023-05-07"),  
#          Date <= as.Date("2024-05-07")) %>%
#   dplyr::summarize(year = 2024,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# wea.2025 <- weather_clean %>%
#   filter(Date >= as.Date("2024-05-14"),  
#          Date <= as.Date("2025-05-14")) %>%
#   dplyr::summarize(year = 2025,
#                    tot.precip.365 = sum(total_precip_mm),
#                    mean.temp.365 = round(mean(AveTemp_C), 3))
# 
# weather.prev.365 <- rbind(wea.2015, wea.2016, wea.2017, wea.2018,
#                           wea.2019, wea.2020, wea.2021, wea.2022,
#                           wea.2023, wea.2024, wea.2025)
# weather.prev.365$year <- as.character(weather.prev.365$year)
# 
# rm(wea.2015, wea.2016, wea.2017, wea.2018,
#    wea.2019, wea.2020, wea.2021, wea.2022,
#    wea.2023, wea.2024, wea.2025)

## make yearly stats table for analysis
# weather_fin <- season_weather %>%
#   pivot_wider(
#     names_from = season,
#     values_from = c(tot.precip, mean.min.temp, mean.max.temp, mean.temp, mean.range.temp)
#   ) %>%
#   left_join(year_weather)

## Remove other weather dataframes
rm(weather)
rm(weather_clean)
rm(year_weather)
rm(season_weather)
