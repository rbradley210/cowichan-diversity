# Script to extract leaf area from photos of scanned leaves
# Robin Bradley
# robin.bradley@ubc.ca
# Created: 22 Sept 2025
# Last updated: 7 Jan 2026

# 1. Set up ----
# set WD
setwd("C:/Users/Robin/Documents/School/Williams Lab")

# packages
library(LeafArea)
library(readxl)

# 2. Extract leaf area from images ----
ALAM <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/ALAM",
                distance.pixel = 21,
                low.size = 0.05,
                known.distance = 0.09,
                save.image = FALSE)

ANOD <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/ANOD",
               distance.pixel = 21,
               trim.pixel = 5,
               low.size = 0.1,
               known.distance = 0.09,
               save.image = FALSE)

BEAQ <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/BEAQ",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

BRCA <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/BRCA",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

BRHO <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/BRHO",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

BRST <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/BRST",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = TRUE)

EROR <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/EROR",
                 distance.pixel = 21,
                 low.size = 0.1,
                 known.distance = 0.09,
                 save.image = FALSE)


# 3. Export final leaf areas as .csv -----
write.csv(EROR, "C:/Users/Robin/Documents/School/Williams Lab/cowichan-diversity/00_rawdata/EROR_leafarea.csv", 
          row.names = FALSE)


