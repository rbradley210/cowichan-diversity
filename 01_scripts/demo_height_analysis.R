# Examining height data from demography datasets
# Robin Bradley
# robinbradley210@gmail.com
# created: 9 January 2026
# last updated: 9 January 2026

# 1. Set up ----

# set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Cowichan_DemographyData")

# packages
library(tidyverse)

# read in + clean data
## plots
plots <- read.csv("/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info/IDE_plotinfo.csv", header = TRUE, stringsAsFactors = TRUE)

## Bromus sterilis
BRST_2021 <- read.csv("Bromus sterilis/2021_Sterilis_Demography.csv", header = TRUE, stringsAsFactors = TRUE) %>%
  select(c(1, 2, 3, 4, 5))
BRST_2022 <- read.csv("Bromus sterilis/2022_Sterilis_Demography.csv", header = TRUE, stringsAsFactors = TRUE)%>%
  select(c(1, 2, 3, 4, 5))
BRST_2023 <- read.csv("Bromus sterilis/2023_Sterilis_Demography.csv", header = TRUE, stringsAsFactors = TRUE)%>%
  select(c(1, 2, 3, 4, 5))
BRST_2024 <- read.csv("Bromus sterilis/2024_Sterilis_Demography.csv", header = TRUE, stringsAsFactors = TRUE)%>%
  select(c(1, 2, 3, 4, 5))
BRST_2025 <- read.csv("Bromus sterilis/2025_Sterilis_Demography.csv", header = TRUE, stringsAsFactors = TRUE)%>%
  select(c(1, 2, 3, 4, 5))

brst_ht <- rbind(BRST_2021, BRST_2022, BRST_2023, BRST_2024, BRST_2025) %>%
  left_join(plots)
brst_ht$plot <- as.factor(brst_ht$plot)
rm(BRST_2021, BRST_2022, BRST_2023, BRST_2024, BRST_2025)

## Dactylis glomerata
DAGL_2016 <- read.csv("Dactylis/2016_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2017 <- read.csv("Dactylis/2017_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2018 <- read.csv("Dactylis/2018_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2019 <- read.csv("Dactylis/2019_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2020 <- read.csv("Dactylis/2020_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2021 <- read.csv("Dactylis/2021_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2022 <- read.csv("Dactylis/2022_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2023 <- read.csv("Dactylis/2023_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
DAGL_2024 <- read.csv("Dactylis/2024_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-9)
DAGL_2025 <- read.csv("Dactylis/2025_Dactylis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-9)

dagl_ht <- rbind(DAGL_2016, DAGL_2017, DAGL_2018, DAGL_2019, DAGL_2020, DAGL_2021, DAGL_2022, DAGL_2023, DAGL_2024, DAGL_2025)%>%
  rename(plot= Plot) %>%
  left_join(plots)
dagl_ht$plot <- as.factor(dagl_ht$plot)
rm(DAGL_2016, DAGL_2017, DAGL_2018, DAGL_2019, DAGL_2020, DAGL_2021, DAGL_2022, DAGL_2023, DAGL_2024, DAGL_2025)


## Plectritis congesta

## Lomatium utriculatum

# 2. Data Exploration ----
## BRST

## DAGL

## PLCO

## LOUT

# 3. Data Analysis ----

# 4. Data Visualization ----