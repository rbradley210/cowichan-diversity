# traits analysis
# Robin Bradley
# robin.bradley@ubc.ca
# Last updated: 2 October 2025

# 1. Set-up ----
# set WD
setwd("C:/Users/Robin/Documents/School/Williams Lab")

# packages
# library(rtry) # for reading in and cleaning data from TRY database
library(readxl)
library(tidyverse)

# read in data
# height <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Height")
weight <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", 
                     sheet = "Dry Weight")
plots <- read.csv("/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info/IDE_plotinfo.csv", header = TRUE)

leafarea <- read.csv("~/School/Williams Lab/cowichan-diversity/00_rawdata/EROR_leafarea.csv")

## selecting plants for N analysis ----
# weight.red <- weight %>% 
#   select(c("Plant Species", "plot", "Plant ID", "Leaf ID", "Part ID", "Weight (mg)"))
# 
# weight.leaves <- weight.red %>%
#   group_by(`Plant Species`, plot, `Plant ID`, `Leaf ID`) %>%
#   summarise(tot.weight = sum(`Weight (mg)`)) # combine part IDs to leaf level
# 
# plants <- weight.leaves %>% 
#   group_by(`Plant Species`, plot, `Plant ID`) %>%
#   summarise(`leaf_ids` = list(`Leaf ID`),
#             tot.weight = sum(tot.weight)) # combine leaf IDs to plant level
# 
# plants$leaf_ids <- gsub("c", "", plants$leaf_ids) # clean leaf IDs column for readability
# plants$leaf_ids <- gsub("\\(", "", plants$leaf_ids)
# plants$leaf_ids <- gsub("\\)", "", plants$leaf_ids)
# 
# plants <- left_join(plants, plots, by = "plot")
# 
# plants$sampleID <- paste(plants$`Plant Species`, 
#                          plants$plot, 
#                          plants$`Plant ID`, 
#                          sep = "_") # create sample ID column
# 
# # check for duplicates
# n_occur <- data.frame(table(plants$sampleID))
# plants[plants$sampleID %in% n_occur$Var1[n_occur$Freq > 1],]
# unique(n_occur$Freq)
# rm(n_occur)
# 
# # remove plants with less than 3mg
# plants <- plants[plants$tot.weight > 3, ]
# 
# # export CSV
# write.csv(plants, 
#           "~/School/Williams Lab/cowichan-diversity/04_other_outputs/samplesforNanalysis_inclLOUT.csv", 
#           row.names = FALSE)
# # edited down in excel
# 
# # bring in new sheet
# Nplants <- read.csv("~/School/Williams Lab/cowichan-diversity/04_other_outputs/samplesforNanalysis.csv",
#                   header = TRUE)
# 
# # make table with number of plants per species per trt
# Nplants.tbl <- Nplants %>%
#   group_by(Plant.Species, trt)%>%
#   summarise(count = n())
# 
# # make table with number of trts per species
# Nplants.tbl.2 <- Nplants.tbl %>%
#   group_by(Plant.Species)%>%
#   summarise(count = n())


# 2. Data Cleaning ----
## Height

## Weight
weight <- weight %>%
  dplyr::group_by(`Plant Species`, plot, `Plant ID`, `Leaf ID`) %>%
  dplyr::summarize(tot.weight = sum(`Weight (mg)`))

weight$sample <- paste(weight$`Plant Species`, 
                         weight$plot,
                         weight$`Plant ID`, 
                         sep = "_") # create sample ID column to match leaf area
weight$sample <- paste(weight$sample,
                         weight$`Leaf ID`, 
                         sep = "") # add leaf ID

## Leaf Nitrogen

## Specific Leaf Area
sla <- left_join(leafarea, weight) # combine datasets
sla$sla <- sla$total.leaf.area/sla$tot.weight # calculate SLA

# 3. Data Analysis
