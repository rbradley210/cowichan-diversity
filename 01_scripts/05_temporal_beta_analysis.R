# Script for: How does precipitation change influence temporal beta diversity?
# Robin Bradley
# robin.bradley@ubc.ca
# created: 25 November 2025
# last updated: 25 November 2025

# 1. Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## Packages
library(tidyverse)
library(adespatial)
library(nlme)
library(car)
library(visreg)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

# 2. Data Formatting ----
## Calculate the temporal beta diversity index (using bray-curtis dissimilarity) between a plot and itself in the starting year
## Create site x species matrix for each year
### pulling out rows into plot and year columns
spe.matrix$ID <- rownames(spe.matrix)
spe.matrix <- spe.matrix %>%
  separate(
    col = ID, 
    into = c("plot", "year"),
    sep = "_"
  )

### Creating year matrices
spe.matrix.2015 <- spe.matrix %>%
  filter(year == 2015)
rownames(spe.matrix.2015) <- spe.matrix.2015$plot
spe.matrix.2015 <- spe.matrix.2015[, - c(62, 63)]

spe.matrix.2016 <- spe.matrix %>%
  filter(year == 2016)
rownames(spe.matrix.2016) <- spe.matrix.2016$plot
spe.matrix.2016 <- spe.matrix.2016[, - c(62, 63)]

spe.matrix.2017 <- spe.matrix %>%
  filter(year == 2017)
rownames(spe.matrix.2017) <- spe.matrix.2017$plot
spe.matrix.2017 <- spe.matrix.2017[, - c(62, 63)]

spe.matrix.2018 <- spe.matrix %>%
  filter(year == 2018)
rownames(spe.matrix.2018) <- spe.matrix.2018$plot
spe.matrix.2018 <- spe.matrix.2018[, - c(62, 63)]

spe.matrix.2019 <- spe.matrix %>%
  filter(year == 2019)
rownames(spe.matrix.2019) <- spe.matrix.2019$plot
spe.matrix.2019 <- spe.matrix.2019[, - c(62, 63)]

spe.matrix.2020 <- spe.matrix %>%
  filter(year == 2020)
rownames(spe.matrix.2020) <- spe.matrix.2020$plot
spe.matrix.2020 <- spe.matrix.2020[, - c(62, 63)]

spe.matrix.2021 <- spe.matrix %>%
  filter(year == 2021)
rownames(spe.matrix.2021) <- spe.matrix.2021$plot
spe.matrix.2021 <- spe.matrix.2021[, - c(62, 63)]

spe.matrix.2022 <- spe.matrix %>%
  filter(year == 2022)
rownames(spe.matrix.2022) <- spe.matrix.2022$plot
spe.matrix.2022 <- spe.matrix.2022[, - c(62, 63)]

spe.matrix.2023 <- spe.matrix %>%
  filter(year == 2023)
rownames(spe.matrix.2023) <- spe.matrix.2023$plot
spe.matrix.2023 <- spe.matrix.2023[, - c(62, 63)]

spe.matrix.2024 <- spe.matrix %>%
  filter(year == 2024)
rownames(spe.matrix.2024) <- spe.matrix.2024$plot
spe.matrix.2024 <- spe.matrix.2024[, - c(62, 63)]

spe.matrix.2025 <- spe.matrix %>%
  filter(year == 2025)
rownames(spe.matrix.2025) <- spe.matrix.2025$plot
spe.matrix.2025 <- spe.matrix.2025[, - c(62, 63)]


## Calculating TBI for each year and the starting year (2015)
### "D" is the same as the TBI values in the output matrices
tbi.2016 <- TBI(spe.matrix.2015, spe.matrix.2016,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2017 <- TBI(spe.matrix.2015, spe.matrix.2017,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2018 <- TBI(spe.matrix.2015, spe.matrix.2018,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2019 <- TBI(spe.matrix.2015, spe.matrix.2019,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2020 <- TBI(spe.matrix.2015, spe.matrix.2020,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2021 <- TBI(spe.matrix.2015, spe.matrix.2021,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2022 <- TBI(spe.matrix.2015, spe.matrix.2022,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2023 <- TBI(spe.matrix.2015, spe.matrix.2023,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2024 <- TBI(spe.matrix.2015, spe.matrix.2024,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2025 <- TBI(spe.matrix.2015, spe.matrix.2025,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)


## Pull TBI values and combine into single dataframe
## pull bcd dataframes and add column for year and plot
temp.2016 <- tbi.2016$BCD.mat
temp.2016$year <- 2016
temp.2016$plot <- plots$plot

temp.2017 <- tbi.2017$BCD.mat
temp.2017$year <- 2017
temp.2017$plot <- plots$plot

temp.2018 <- tbi.2018$BCD.mat
temp.2018$year <- 2018
temp.2018$plot <- plots$plot

temp.2019 <- tbi.2019$BCD.mat
temp.2019$year <- 2019
temp.2019$plot <- plots$plot

temp.2020 <- tbi.2020$BCD.mat
temp.2020$year <- 2020
temp.2020$plot <- plots$plot

temp.2021 <- tbi.2021$BCD.mat
temp.2021$year <- 2021
temp.2021$plot <- plots$plot

temp.2022 <- tbi.2022$BCD.mat
temp.2022$year <- 2022
temp.2022$plot <- plots$plot

temp.2023 <- tbi.2023$BCD.mat
temp.2023$year <- 2023
temp.2023$plot <- plots$plot

temp.2024 <- tbi.2024$BCD.mat
temp.2024$year <- 2024
temp.2024$plot <- plots$plot

temp.2025 <- tbi.2025$BCD.mat
temp.2025$year <- 2025
temp.2025$plot <- plots$plot

# combine bcd dataframes
TBI <- rbind(temp.2016, temp.2017, temp.2018,
             temp.2019, temp.2020, temp.2021, 
             temp.2022, temp.2023, temp.2024,
             temp.2025) %>%
  dplyr::rename(TBI = "D=(B+C)/(2A+B+C)")%>%
  left_join(plots)

TBI$trt <- factor(TBI$trt, levels = c("control","drought", "irrigated"))

TBI$plot <- as.factor(TBI$plot)
TBI$plot <- factor(TBI$plot, levels = c("1", "2", "3", "4", "5", "6", 
                                                    "7", "8", "9", "10", "11", "12", 
                                                    "13", "14", "15"))
TBI$year <- as.integer(as.factor(TBI$year))


# 3. Model temporal beta diversity as a function of year and treatment ----
## linear mixed effects models
base <- lme(TBI ~ trt*year, # base
               data = TBI,
               random = ~1|plot)
plot(base)

base.cor <- lme(TBI ~ trt*year, # base
                data = TBI,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))
plot(base.cor)

## Calculate AIC
myAIC <- c(AIC(base), AIC(base.cor))
delta <- myAIC - min(myAIC)
model <- c("base", "base.cor")
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
                   stringsAsFactors = FALSE)

# summary
summary(base.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(base.cor, type = 3) #marginal, not sequential

# 4. Plots
plot(visreg(base.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 11, 10),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "TBI",
     xlab = "Years since beginning of experiment",
     ylab = "TBI",
     overlay = TRUE)
legend(x = "bottomright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "darkorange", "deepskyblue3"),
       lwd = 3,
       pch = 1)

ggplot(TBI, aes(year, TBI, color = trt))+
  geom_point()
