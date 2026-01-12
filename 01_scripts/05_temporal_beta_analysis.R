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


## Calculating TBI for each year and the starting year (2016)
### "D" is the same as the TBI values in the output matrices
# tbi.2016 <- TBI(spe.matrix.2015, spe.matrix.2016,
#                 method = "%difference",
#                 nperm = 99,
#                 BCD = TRUE,
#                 seed. = 42
# )

tbi.2017 <- TBI(spe.matrix.2016, spe.matrix.2017,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2018 <- TBI(spe.matrix.2016, spe.matrix.2018,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2019 <- TBI(spe.matrix.2016, spe.matrix.2019,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2020 <- TBI(spe.matrix.2016, spe.matrix.2020,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2021 <- TBI(spe.matrix.2016, spe.matrix.2021,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2022 <- TBI(spe.matrix.2016, spe.matrix.2022,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2023 <- TBI(spe.matrix.2016, spe.matrix.2023,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2024 <- TBI(spe.matrix.2016, spe.matrix.2024,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.2025 <- TBI(spe.matrix.2016, spe.matrix.2025,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)


## Pull TBI values and combine into single dataframe
## pull bcd dataframes and add column for year and plot

# temp.2016 <- tbi.2016$BCD.mat
# temp.2016$year <- 2016
# temp.2016$plot <- plots$plot

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
TBI <- rbind(#temp.2016, 
             temp.2017, temp.2018,
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

TBI$year <- as.character(TBI$year) # convert to character to match weather dataframe

TBI <- left_join(TBI, weather.prev.gs) # add weather
TBI <- left_join(TBI, weather.prev.365)
TBI <- TBI[, -8]

TBI$year <- as.integer(as.factor(TBI$year)) # convert year to integer

# remove spe.matrix, tbi and temp dataframes to clean up environment
rm(spe.matrix.2015, spe.matrix.2016, spe.matrix.2017, spe.matrix.2018, spe.matrix.2019, spe.matrix.2020, spe.matrix.2021, spe.matrix.2022, spe.matrix.2023, spe.matrix.2024, spe.matrix.2025)

rm(tbi.2017, tbi.2018, tbi.2019, tbi.2020, tbi.2021, tbi.2022, tbi.2023, tbi.2024, tbi.2025)

rm(temp.2017, temp.2018, temp.2019, temp.2020, temp.2021, temp.2022, temp.2023, temp.2024, temp.2025)


# 3. Model temporal beta diversity as a function of year and treatment ----
## linear mixed effects models

# base models
base <- lme(TBI ~ trt+year, # base
               data = TBI,
               random = ~1|plot)
plot(base)

base.int <- lme(TBI ~ trt*year, # base + int
                data = TBI,
                random = ~1|plot)
plot(base.int)

base.cor <- lme(TBI ~ trt+year, # base + cor
                data = TBI,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))
plot(base.cor)

base.int.cor <- lme(TBI ~ trt*year, # base + int + cor
                    data = TBI,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(base.int.cor)

# weather models (no INT, no COR)
b.pgs.p <- lme(TBI ~ trt+year+prev.gs.precip, # base + prev gs precip
                data = TBI,
                random = ~1|plot)
plot(b.pgs.p) 

b.pgs.t <- lme(TBI ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
                data = TBI,
                random = ~1|plot)
plot(b.pgs.t) # examine residuals,

b.pgs.p.t <- lme(TBI ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                  data = TBI,
                  random = ~1|plot)
plot(b.pgs.p.t) # examine residuals

b.365.p <- lme(TBI ~ trt+year+tot.precip.365, # base + prev 365 precip
                data = TBI,
                random = ~1|plot)
plot(b.365.p) # examine residuals

b.365.t <- lme(TBI ~ trt+year+mean.temp.365, # base + prev 365 temp
                data = TBI,
                random = ~1|plot)
plot(b.365.t) # examine residuals,

b.365.p.t <- lme(TBI ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                  data = TBI,
                  random = ~1|plot)
plot(b.365.p.t) # examine residuals

# weather models (no INT, with COR)
b.pgs.p.cor <- lme(TBI ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                    data = TBI,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.cor) # examine residuals

b.pgs.t.cor <- lme(TBI ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                    data = TBI,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.pgs.t.cor) # examine residuals

b.pgs.p.t.cor <- lme(TBI ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                      data = TBI,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.t.cor) # examine residuals

b.365.p.cor <- lme(TBI ~ trt+year+tot.precip.365, # base + prev 365 precip + cor
                    data = TBI,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.365.p.cor) # examine residuals

b.365.t.cor <- lme(TBI ~ trt+year+mean.temp.365, # base + prev 365 temp + cor
                    data = TBI,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.365.t.cor) # examine residuals

b.365.p.t.cor <- lme(TBI ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                      data = TBI,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(b.365.p.t.cor) # examine residuals


# weather models (with INT, no COR)
b.pgs.p.int <- lme(TBI ~ trt*year*prev.gs.precip, # base + prev gs precip + int
               data = TBI,
               random = ~1|plot)
plot(b.pgs.p.int) 

b.pgs.t.int <- lme(TBI ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
               data = TBI,
               random = ~1|plot)
plot(b.pgs.t.int) # examine residuals

b.pgs.p.t.int <- lme(TBI ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                 data = TBI,
                 random = ~1|plot)
plot(b.pgs.p.t.int) # examine residuals

b.365.p.int <- lme(TBI ~ trt*year*tot.precip.365, # base + prev 365 precip + int
               data = TBI,
               random = ~1|plot)
plot(b.365.p.int) # examine residuals

b.365.t.int <- lme(TBI ~ trt*year*mean.temp.365, # base + prev 365 temp + int
               data = TBI,
               random = ~1|plot)
plot(b.365.t.int) # examine residuals

b.365.p.t.int <- lme(TBI ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + int
                 data = TBI,
                 random = ~1|plot)
plot(b.365.p.t.int) # examine residuals


# weather models (with INT, with COR)
b.pgs.p.cor.int <- lme(TBI ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                   data = TBI,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.cor.int) # examine residuals

b.pgs.t.cor.int <- lme(TBI ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                   data = TBI,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.pgs.t.cor.int) # examine residuals

b.pgs.p.t.cor.int <- lme(TBI ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                     data = TBI,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.t.cor.int) # examine residuals

b.365.p.cor.int <- lme(TBI ~ trt*year*tot.precip.365, # base + prev 365 precip + cor + int
                   data = TBI,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.365.p.cor.int) # examine residuals

b.365.t.cor.int <- lme(TBI ~ trt*year*mean.temp.365, # base + prev 365 temp + cor + int
                   data = TBI,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.365.t.cor.int) # examine residuals

b.365.p.t.cor.int <- lme(TBI ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + cor +int
                     data = TBI,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(b.365.p.t.cor.int) # examine residuals




## Calculate AIC
myAIC <- c(AIC(base), AIC(base.cor), AIC(base.int), AIC(base.int.cor),
           AIC(b.365.p), AIC(b.365.p.cor), AIC(b.365.p.int), AIC(b.365.p.cor.int),
           AIC(b.365.t), AIC(b.365.t.cor), AIC(b.365.t.int), AIC(b.365.t.cor.int),
           AIC(b.365.p.t), AIC(b.365.p.t.cor), AIC(b.365.p.t.int), AIC(b.365.p.t.cor.int),
           AIC(b.pgs.p), AIC(b.pgs.p.cor), AIC(b.pgs.p.int), AIC(b.pgs.p.cor.int),
           AIC(b.pgs.t), AIC(b.pgs.t.cor), AIC(b.pgs.t.int), AIC(b.pgs.t.cor.int),
           AIC(b.pgs.p.t), AIC(b.pgs.p.t.cor), AIC(b.pgs.p.t.int), AIC(b.pgs.p.t.cor.int))
delta <- myAIC - min(myAIC)
model <- c("base", "base.cor", "base.int", "base.cor.int",
           "365.p", "365.p.cor", "365.p.int", "365.p.cor.int",
           "365.t", "365.t.cor", "365.t.int", "365.t.cor.int",
           "365.p.t", "365.p.t.cor", "365.p.t.int", "365.p.t.cor.int",
           "pgs.p", "pgs.p.cor", "pgs.p.int", "pgs.p.cor.int",
           "pgs.t", "pgs.t.cor", "pgs.t.int", "pgs.t.cor.int",
           "pgs.p.t", "pgs.p.t.cor", "pgs.p.t.int", "pgs.p.t.cor.int")
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
                   stringsAsFactors = FALSE)

# summary
summary(b.pgs.t.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(b.pgs.t.cor, type = 3) #marginal, not sequential

# 4. Plots
## year vs. TBI split by treatment
plot(visreg(b.pgs.t.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 9, 8),
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

## mean previous growing season temp vs. TBI, split by trt
plot(visreg(b.pgs.t.cor, xvar = "prev.gs.mean.temp", by = "trt", overlay = TRUE, plot = FALSE),
     #xaxp = c(1, 9, 8),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "TBI",
     xlab = "Mean Temperature (°C) in Previous Growing Season",
     ylab = "TBI",
     overlay = TRUE)
legend(x = "topright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "darkorange", "deepskyblue3"),
       lwd = 3,
       pch = 1)
