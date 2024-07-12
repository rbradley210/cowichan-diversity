# Cowichan Diversity Analysis Data Cleaning
# Adapted from code by Lauren Smith (plants_cleanup.Rmd)
# email: robinbradley210@gmail.com


## Set Up
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
library(vegan)
library(ggplot2)

#### Diversity Data ####
## Read in plot data
plots <- read.csv("IDE_plotinfo.csv", header = TRUE)
plots <- plots %>% 
  dplyr::rename("treatment" = "trt")
plots$plot <- as.character(plots$plot)

## Read in cover data
# IDE surveys (mid growing season, 2015-2021)
cover_mids20152021 <- read.csv("Diversity/cowichan_community5_19_2021.csv", header = TRUE, na.strings = c("", " ")) %>% 
  dplyr::select(-quadID, -notes, -X, -X.1, -X.2, -X.3, -X.4, -X.5)
cover_mids20152021$date <- "mid"

# late growing season resurvey 2020
cover_late2020 <- read.csv("Diversity/cowichan_community_LSmith_6_12_20.csv", header = TRUE, na.strings = c("", " ")) %>%
  slice(1:729) %>% 
  dplyr::select(-quadID, -notes)
cover_late2020$date <- "late"

# early growing season resurvey 2021
cover_early2021 <- read.csv("Diversity/cowichan_community_LSmith_4_17_21.csv", header = TRUE, na.strings = c("", " ")) %>%
  slice(1:766) %>% 
  dplyr::select(-quadID, -notes)
cover_early2021$date <- "early"

# late growing season resurvey 2021
cover_late2021 <- read.csv("Diversity/cowichan_community_LSmith_6_11_21.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:788) %>% 
  dplyr::select(-quadID, -notes, -X, -X.1, -X.2, -X.3, -X.4, -X.5)
cover_late2021$date <- "late"

# cover mid 2022
cover_mid2022 <- read.csv("Diversity/2022_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:802) %>% 
  dplyr::select(-quadID, -notes)
cover_mid2022$date <- "mid"

# cover mid 2023
cover_mid2023 <- read.csv("Diversity/2023_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:808) %>% 
  dplyr::select(-quadID, -notes)
cover_mid2023$date <- "mid"

# cover mid 2024
cover_mid2024 <- read.csv("Diversity/2024_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:813) %>% 
  dplyr::select(-quadID, -notes)
cover_mid2024$date <- "mid"

# cover late 2024
cover_late2024 <- read.csv("Diversity/cowichan_community_6_09_2024.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:816) %>% 
  dplyr::select(-quadID, -notes)
cover_late2024$date <- "late"

## Read in trait info
traits <- read.csv("species_traits.csv", header = TRUE)

#traits <- traits %>% 
  #dplyr::rename("species" = "ï..species") ## not sure what this is for and I cant get it to work

## bind all diversity surveys together
cover_all <- rbind(cover_mids20152021, cover_late2020, cover_early2021, cover_late2021, 
                   cover_mid2022, cover_mid2023, cover_mid2024, cover_late2024)

# fix plot column name
#cover_all <- cover_all %>% 
  #dplyr::rename("plot" = "ï..plot") ##again unclear on what this is doing

# change cover column to numbers not characters, and plot to factor
cover_all$cover <- as.numeric(as.character(cover_all$cover))  
cover_all$plot <- as.factor(as.numeric(cover_all$plot))

# get rid of NAs in the cover column
cover_all <- cover_all %>%
  filter(cover != "NA")

# fix treatments
cover_all$treatment[cover_all$treatment == "Irrigated"] <- "irrigated"
cover_all$treatment[cover_all$treatment == "Control"] <- "control"
cover_all$treatment[cover_all$treatment == "Drought"] <- "drought"


#### Biomass Data ####
