# Cleaning 2020-2021 data for IDE
# Robin Bradley
# 20 December 2024

# Set up
library(tidyverse)
library(dplyr)
library(tidyr)
library(plyr)
library(vegan)
library(ggplot2)
library(reshape)

# Read in data
cover <- read.csv("IDEdata20202021scratch.csv")

# List of unique species names
plant_species <- data.frame(unique(cover$taxa[cover$cover > 0]))
plant_species
