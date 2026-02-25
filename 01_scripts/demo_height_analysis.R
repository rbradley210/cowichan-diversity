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
library(car)
library(lme4)
library(visreg)
library(emmeans)

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
brst_ht$year <- as.factor(brst_ht$year)
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
dagl_ht$Year <- as.factor(dagl_ht$Year)
rm(DAGL_2016, DAGL_2017, DAGL_2018, DAGL_2019, DAGL_2020, DAGL_2021, DAGL_2022, DAGL_2023, DAGL_2024, DAGL_2025)


## Plectritis congesta
PLCO_2016 <-  read.csv("Plectritis/2016_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2017 <-  read.csv("Plectritis/2017_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2018 <-  read.csv("Plectritis/2018_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2019 <-  read.csv("Plectritis/2019_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2020 <-  read.csv("Plectritis/2020_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5, 12))
PLCO_2021 <-  read.csv("Plectritis/2021_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2022 <-  read.csv("Plectritis/2022_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2023 <-  read.csv("Plectritis/2023_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE) %>% select(-c(4,5))
PLCO_2024 <-  read.csv("Plectritis/2024_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)
PLCO_2025 <-  read.csv("Plectritis/2025_Plectritis_Demography_Data.csv", header = TRUE, stringsAsFactors = TRUE)

plco_ht <- rbind(PLCO_2016, PLCO_2017, PLCO_2018, PLCO_2019, PLCO_2020, PLCO_2021, PLCO_2022, PLCO_2023, PLCO_2024, PLCO_2025)%>%
  left_join(plots)
plco_ht$plot <- as.factor(plco_ht$plot)
plco_ht$year <- as.factor(plco_ht$year)
rm(PLCO_2016, PLCO_2017, PLCO_2018, PLCO_2019, PLCO_2020, PLCO_2021, PLCO_2022, PLCO_2023, PLCO_2024, PLCO_2025)

# 2. Data Analysis ----
## BRST
lm_brst <- lmer(ht ~ trt*year + (1|plot), data = brst_ht)
summary(lm_brst)
Anova(lm_brst)

brst_grpmeans <- emmeans(lm_brst, c("trt", "year"))
pairs(brst_grpmeans)
plot(brst_grpmeans)
pwpm(brst_grpmeans, means = FALSE, diffs = FALSE)


## DAGL
lm_dagl <- lmer(Height ~ trt*Year + (1|plot), data = dagl_ht)
summary(lm_dagl)
Anova(lm_dagl)

dagl_grpmeans <- emmeans(lm_dagl, c("trt", "Year"))
pairs(dagl_grpmeans)
plot(dagl_grpmeans)
pwpm(dagl_grpmeans, means = FALSE, diffs = FALSE)

## PLCO
lm_plco <- lmer(ht ~ trt*year + (1|plot), data = plco_ht)
summary(lm_plco)
Anova(lm_plco)
plco_grpmeans <- emmeans(lm_plco, c("trt", "year"), pbkrtest.limit = 3560)
pairs(plco_grpmeans)
plot(plco_grpmeans)
pwpm(plco_grpmeans, means = FALSE, diffs = FALSE)

# 4. Data Visualization ----
## BRST
plot(visreg(lm_brst, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "B. sterilis height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)

plot(visreg(lm_brst, xvar = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "B. sterilis height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)

plot(visreg(lm_brst, xvar = "year", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "B. sterilis height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)

## DAGL
plot(visreg(lm_dagl, xvar = "Year", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "D. glomerata height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)

## PLCO
plot(visreg(lm_plco, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "P. congesta height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)

plot(visreg(lm_plco, xvar = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "P. congesta height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)

plot(visreg(lm_plco, xvar = "year", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "P. congesta height patterns",
     xlab = "Year",
     ylab = "Height",
     overlay = TRUE)



## LOUT
