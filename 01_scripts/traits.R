# traits analysis
# Robin Bradley
# robin.bradley@ubc.ca
# Last updated: 15 August 2025

# Set-up
library(rtry) # for reading in and cleaning data from TRY database
library(readxl)
library(tidyverse)


# read in data
height <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", 
                                            sheet = "Height")
weight <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", 
                     sheet = "Dry Weight")


# figuring out how many heights I should have
weight.red <- weight %>% 
  select(c("Plant Species", "Plot", "Plant ID", "Leaf ID"))
weight.plants <- weight.red[!duplicated(weight.red),] # 880 plants
weight.red <- weight %>% 
  select(c("Plant Species", "Plot", "Plant ID"))
height.plants <- weight.red[!duplicated(weight.red),] # 344 plants



