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
biomass <- read.csv("Biomass/cowichan_biomass_master_2024.csv", header = TRUE)

# read in the biomass data, get rid of unnecessary columns and rename mass column
biomass <- biomass %>% 
  dplyr::select(-treatmentyear, -notes, -MassNew, -MassExtra, -MassRecalculated) #%>% 
#  dplyr::rename("plot" = "ï..plot") 
# dplyr::rename("mass" = "mass..grams.")

# change mass column to numbers not characters; year to factor
biomass$mass <- as.numeric(biomass$mass)
biomass$year <- as.factor(biomass$year)

# convert to g/m^2
biomass$mass <- (biomass$mass)/(.2)

biomass$taxa[biomass$taxa == "Grass"] <- "grass"
biomass$taxa[biomass$taxa == "Bryophyte"] <- "bryo"
biomass$taxa[biomass$taxa == "Bryo"] <- "bryo"
biomass$taxa[biomass$taxa == "Dead"] <- "dead"
biomass$taxa[biomass$taxa == "Shrub"] <- "shrub"
biomass$taxa[biomass$taxa == "Forb"] <- "forb"
biomass$taxa[biomass$taxa == "Tree"] <- "tree"
biomass$taxa[biomass$taxa == "Legume"] <- "legume"

# remove dead and moss
biomass <- biomass %>%
  filter(taxa!="dead" & taxa != "bryo")

#adjusting shrub biomass
biomass$mass_adj <- ifelse(biomass$mass > 5 & 
                             biomass$taxa == "shrub" & 
                             biomass$year != 2020 & 
                             biomass$year != 2021 & 
                             biomass$year != 2019 & 
                             biomass$year != 2017, 
                           biomass$mass/2, biomass$mass)
# calculate total biomass across all growth forms
totbiomass <- biomass %>% 
  group_by(plot, treatment, year) %>% 
  dplyr::summarize(totbio = sum(mass, na.rm = TRUE), 
                   totbio_adj = sum(mass_adj, na.rm = TRUE))

# widen so that there is one column for each growthform
biomass_wide <- biomass %>% 
  pivot_wider(names_from = taxa,
              names_sep = "_",
              values_from = c(mass, mass_adj))

# replace NAs with 0  
biomass_wide <- biomass_wide %>% replace(is.na(biomass_wide), 0)

#plotting
biomass_timeseries_total <- ggplot(totbiomass,
                             aes(x = year,
                                 y = totbio_adj,
                                 color = as.factor(treatment)))+
  geom_boxplot()+
  labs(color='Treatment')+
  xlab("Year")+
  ylab("Total Mass (g/m^2)")+
  theme_test()
biomass_timeseries_total

biomass_timeseries_total <- ggplot(totbiomass,
                                   aes(x = treatment,
                                       y = totbio_adj,
                                       color = as.factor(treatment)))+
  geom_boxplot()+
  labs(color='Treatment')+
  xlab("Treatment")+
  ylab("Total Mass (g/m^2)")+
  theme_test()
biomass_timeseries_total

## 2018 biomass
biomass2018 <- ggplot(biomass[biomass$year == 2018, ],
                      aes(x = taxa,
                          y = mass_adj))+
  geom_point()+
  theme_test ()
biomass2018

## grass biomass
biomassgrass <- ggplot(biomass[biomass$taxa == "grass", ],
                       aes(x = year,
                           y = mass_adj,
                       color = as.factor(treatment)))+
  geom_point()+
  theme_test ()
biomassgrass

#biomass by treatment
biomasstrt <- ggplot(biomass,
                       aes(x = treatment,
                           y = mass_adj,
                           color = as.factor(taxa)))+
  geom_boxplot()+
  theme_test ()
biomasstrt
