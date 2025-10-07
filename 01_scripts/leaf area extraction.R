# Script to extract leaf area from photos of scanned leaves
# Robin Bradley
# robin.bradley@ubc.ca
# Created: 22 Sept 2025
# Last updated: 23 Sept 2025

# 1. Set up ----
# set WD
setwd("C:/Users/Robin/Documents/School/Williams Lab")

# packages
library(LeafArea)
library(readxl)
#library(pliman)

# 2. Package testing ----
## Pliman
# leaf <- image_import("SLA scans/test/IMG_0004-sm.jpg",
#          plot = TRUE) # import image
# image_index(leaf) # look at how different indexes work with image
# 
# count <- analyze_objects(leaf, marker = "id",
#                          index = "B",
#                          watershed = FALSE,
#                          object_size = "large") # identify leaves
# area <- get_measures(count, dpi = 600) # get area

## LeafArea test
# run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/test",
#         distance.pixel = 21,
#         low.size = 0.1,
#         known.distance = 0.09,
#         save.image = TRUE)

## LeafArea is much more user friendly + faster so going with that. Also using ImageJ, which is standard

# 3. Extract leaf area from images ----
EROR <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/EROR",
                 distance.pixel = 21,
                 low.size = 0.1,
                 known.distance = 0.09,
                 save.image = TRUE)

# 4. Export final leaf areas as .csv
write.csv(EROR, "C:/Users/Robin/Documents/School/Williams Lab/cowichan-diversity/00_rawdata/EROR_leafarea.csv", 
          row.names = FALSE)


