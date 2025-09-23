# Cowichan Plant Community and Weather Data Cleaning script
# Adapted from code by Lauren Smith (plants_cleanup.Rmd)
# email: robinbradley210@gmail.com
# created: June 2024
# last updated: 23 September 2025


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
cover_mids20152021 <- read.csv("Diversity/cowichan_community5_19_2021.csv", header = TRUE, na.strings = c("", " ")) %>% 
  dplyr::select(-quadID, -notes, -X, -X.1, -X.2, -X.3, -X.4, -X.5)

### 2022
cover_mid2022 <- read.csv("Diversity/2022_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:802) %>% 
  dplyr::select(-quadID, -notes)

### 2023
cover_mid2023 <- read.csv("Diversity/2023_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:808) %>% 
  dplyr::select(-quadID, -notes)

### 2024
cover_mid2024 <- read.csv("Diversity/2024_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:813) %>% 
  dplyr::select(-quadID, -notes)

### 2025
cover_mid2025 <- read.csv("Diversity/2025_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:799) %>% 
  dplyr::select(-quadID, -notes)

## Cleaning cover data
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

# Pooling invasive annual bromes
plants$species[plants$species == "Bromus hordeaceus"] <- "Annual Bromus"
plants$species[plants$species == "Bromus sterilis"] <- "Annual Bromus"

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

## Create list of unique species names
plant_species <- data.frame(unique(plants$species[plants$cover > 0]))

## Species-Plot Matrix
spe.matrix <- cast(plants, plot + year ~ species, value = 'cover', fun.aggregate = sum) %>% # convert to matrix
  unite("ID", plot:year)
spe.matrix[is.na(spe.matrix)] <- 0 # convert NAs to 0s
spe.matrix <- data.frame(spe.matrix, row.names = 1) # convert plot-year ID to row names

# make plot-year metadata file
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