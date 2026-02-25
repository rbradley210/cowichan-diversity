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
                save.image = FALSE)

CAIN <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/CAIN",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.07,
                known.distance = 0.09,
                save.image = FALSE)

CAQU <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/CAQU",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

CEGL <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/CEGL",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.005,
                known.distance = 0.09,
                save.image = FALSE)

CLPE <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/CLPE",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

DAGL <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/DAGL",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

DEME <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/DEME",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

ELGL <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/ELGL",
                distance.pixel = 21,
                trim.pixel = 3,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

EROR <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/EROR",
                 distance.pixel = 21,
                 low.size = 0.1,
                 known.distance = 0.09,
                 save.image = FALSE)

GAAP <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/GAAP",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.05,
                known.distance = 0.09,
                save.image = FALSE)

GEMO <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/GEMO",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

LASP <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/LASP",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.005,
                known.distance = 0.09,
                save.image = FALSE)

LOUT <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/LOUT",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.0001,
                known.distance = 0.09,
                save.image = FALSE)

LUSP <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/LUSP",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

MESU <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/MESU",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.01,
                known.distance = 0.09,
                save.image = FALSE)

NEPA <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/NEPA",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

PLCO <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/PLCO",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.01,
                known.distance = 0.09,
                save.image = FALSE)

POPR <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/POPR",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

PRHE <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/PRHE",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

RAOC <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/RAOC",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

SACR <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/SACR",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

SYAL <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/SYAL",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

VALO <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/VALO",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.1,
                known.distance = 0.09,
                save.image = FALSE)

VEAR <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/VEAR",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.01,
                known.distance = 0.09,
                save.image = FALSE)

VIHI <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/VIHI",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.001,
                known.distance = 0.09,
                save.image = FALSE)

VISA <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/VISA",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.001,
                known.distance = 0.09,
                save.image = FALSE)

VUSP <- run.ij (set.directory = "C:/Users/Robin/Documents/School/Williams Lab/SLA scans/VUSP",
                distance.pixel = 21,
                trim.pixel = 5,
                low.size = 0.01,
                known.distance = 0.09,
                save.image = FALSE)

sla <- rbind(ALAM, ANOD, BEAQ, BRCA, BRHO, BRST, CAIN, CAQU, CEGL, CLPE, DAGL, 
             DEME, ELGL, EROR, GAAP, GEMO, LASP, LOUT, LUSP, MESU, NEPA, PLCO, 
             POPR, PRHE, RAOC, SACR, SYAL, VALO, VEAR, VIHI, VISA, VUSP)


# 3. Export final leaf areas as .csv -----
write.csv(sla, "C:/Users/Robin/Documents/School/Williams Lab/cowichan-diversity/00_rawdata/leafarea.csv", 
          row.names = FALSE)


