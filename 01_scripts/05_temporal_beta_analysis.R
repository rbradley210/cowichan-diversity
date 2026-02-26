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
library(paletteer)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

# 2. Data Formatting ----
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


## Calculating TBI for each year and the starting year (2016) ----
### "D" is the same as the TBI values in the output matrices
tbi.beg.2017 <- TBI(spe.matrix.2016, spe.matrix.2017,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2018 <- TBI(spe.matrix.2016, spe.matrix.2018,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2019 <- TBI(spe.matrix.2016, spe.matrix.2019,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2020 <- TBI(spe.matrix.2016, spe.matrix.2020,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2021 <- TBI(spe.matrix.2016, spe.matrix.2021,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2022 <- TBI(spe.matrix.2016, spe.matrix.2022,
                method = "%difference",
                nperm = 999,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2023 <- TBI(spe.matrix.2016, spe.matrix.2023,
                method = "%difference",
                nperm = 999,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2024 <- TBI(spe.matrix.2016, spe.matrix.2024,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2025 <- TBI(spe.matrix.2016, spe.matrix.2025,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)


## Pull TBI values and combine into single dataframe
## pull bcd dataframes and add column for year and plotu
temp.2017 <- tbi.beg.2017$BCD.mat
temp.2017$year <- 2017
temp.2017$plot <- plots$plot

temp.2018 <- tbi.beg.2018$BCD.mat
temp.2018$year <- 2018
temp.2018$plot <- plots$plot

temp.2019 <- tbi.beg.2019$BCD.mat
temp.2019$year <- 2019
temp.2019$plot <- plots$plot

temp.2020 <- tbi.beg.2020$BCD.mat
temp.2020$year <- 2020
temp.2020$plot <- plots$plot

temp.2021 <- tbi.beg.2021$BCD.mat
temp.2021$year <- 2021
temp.2021$plot <- plots$plot

temp.2022 <- tbi.beg.2022$BCD.mat
temp.2022$year <- 2022
temp.2022$plot <- plots$plot

temp.2023 <- tbi.beg.2023$BCD.mat
temp.2023$year <- 2023
temp.2023$plot <- plots$plot

temp.2024 <- tbi.beg.2024$BCD.mat
temp.2024$year <- 2024
temp.2024$plot <- plots$plot

temp.2025 <- tbi.beg.2025$BCD.mat
temp.2025$year <- 2025
temp.2025$plot <- plots$plot

# combine bcd dataframes
TBI.beg <- rbind(#temp.2016, 
             temp.2017, temp.2018,
             temp.2019, temp.2020, temp.2021, 
             temp.2022, temp.2023, temp.2024,
             temp.2025) %>%
  dplyr::rename(TBI = "D=(B+C)/(2A+B+C)", B = "B/(2A+B+C)", C = "C/(2A+B+C)")%>%
  mutate(b.c.ratio = C/B)%>% # greater than 1: gains (C) dominate, less than 1: losses(B) dominate
  left_join(plots)

TBI.beg$trt <- factor(TBI.beg$trt, levels = c("control","drought", "irrigated"))

TBI.beg$plot <- as.factor(TBI.beg$plot)
TBI.beg$plot <- factor(TBI.beg$plot, levels = c("1", "2", "3", "4", "5", "6",
                                        "7", "8", "9", "10", "11", "12",
                                        "13", "14", "15"))

TBI.beg$year <- as.character(TBI.beg$year) # convert to character to match weather dataframe

TBI.beg <- left_join(TBI.beg, weather.prev.gs) # add weather
TBI.beg <- left_join(TBI.beg, weather.prev.365)
TBI.beg <- TBI.beg[, -9]

TBI.beg$year <- as.integer(as.factor(TBI.beg$year)) # convert year to integer

## Calculating TBI for each year and the previous year ----
### "D" is the same as the TBI values in the output matrices
tbi.pre.2017 <- TBI(spe.matrix.2016, spe.matrix.2017,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2018 <- TBI(spe.matrix.2017, spe.matrix.2018,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2019 <- TBI(spe.matrix.2018, spe.matrix.2019,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2020 <- TBI(spe.matrix.2019, spe.matrix.2020,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2021 <- TBI(spe.matrix.2020, spe.matrix.2021,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2022 <- TBI(spe.matrix.2021, spe.matrix.2022,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2023 <- TBI(spe.matrix.2022, spe.matrix.2023,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2024 <- TBI(spe.matrix.2023, spe.matrix.2024,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2025 <- TBI(spe.matrix.2024, spe.matrix.2025,
                    method = "%difference",
                    nperm = 999,
                    BCD = TRUE,
                    seed. = 42
)


## Pull TBI values and combine into single dataframe
## pull bcd dataframes and add column for year and plot
temp.2017 <- tbi.pre.2017$BCD.mat
temp.2017$year <- 2017
temp.2017$plot <- plots$plot

temp.2018 <- tbi.pre.2018$BCD.mat
temp.2018$year <- 2018
temp.2018$plot <- plots$plot

temp.2019 <- tbi.pre.2019$BCD.mat
temp.2019$year <- 2019
temp.2019$plot <- plots$plot

temp.2020 <- tbi.pre.2020$BCD.mat
temp.2020$year <- 2020
temp.2020$plot <- plots$plot

temp.2021 <- tbi.pre.2021$BCD.mat
temp.2021$year <- 2021
temp.2021$plot <- plots$plot

temp.2022 <- tbi.pre.2022$BCD.mat
temp.2022$year <- 2022
temp.2022$plot <- plots$plot

temp.2023 <- tbi.pre.2023$BCD.mat
temp.2023$year <- 2023
temp.2023$plot <- plots$plot

temp.2024 <- tbi.pre.2024$BCD.mat
temp.2024$year <- 2024
temp.2024$plot <- plots$plot

temp.2025 <- tbi.pre.2025$BCD.mat
temp.2025$year <- 2025
temp.2025$plot <- plots$plot

# combine bcd dataframes
TBI.pre <- rbind(temp.2017, temp.2018,
  temp.2019, temp.2020, temp.2021, 
  temp.2022, temp.2023, temp.2024,
  temp.2025) %>%
  dplyr::rename(TBI = "D=(B+C)/(2A+B+C)", B = "B/(2A+B+C)", C = "C/(2A+B+C)")%>%
  mutate(b.c.ratio = C/B)%>% # greater than 1: gains (C) dominate, less than 1: losses(B) dominate
  left_join(plots)

TBI.pre$trt <- factor(TBI.pre$trt, levels = c("control","drought", "irrigated"))

TBI.pre$plot <- as.factor(TBI.pre$plot)
TBI.pre$plot <- factor(TBI.pre$plot, levels = c("1", "2", "3", "4", "5", "6",
                                                "7", "8", "9", "10", "11", "12",
                                                "13", "14", "15"))

TBI.pre$year <- as.character(TBI.pre$year) # convert to character to match weather dataframe

TBI.pre <- left_join(TBI.pre, weather.prev.gs) # add weather
TBI.pre <- left_join(TBI.pre, weather.prev.365)
TBI.pre <- TBI.pre[, -9]

TBI.pre$year <- as.integer(as.factor(TBI.pre$year)) # convert year to integer

## remove spe.matrix, tbi and temp dataframes to clean up environment ----
rm(spe.matrix.2015, spe.matrix.2016, spe.matrix.2017, spe.matrix.2018, spe.matrix.2019, spe.matrix.2020, spe.matrix.2021, spe.matrix.2022, spe.matrix.2023, spe.matrix.2024, spe.matrix.2025)

rm(tbi.beg.2017, tbi.beg.2018, tbi.beg.2019, tbi.beg.2020, tbi.beg.2021, tbi.beg.2022, tbi.beg.2023, tbi.beg.2024, tbi.beg.2025)
rm(tbi.pre.2017, tbi.pre.2018, tbi.pre.2019, tbi.pre.2020, tbi.pre.2021, tbi.pre.2022, tbi.pre.2023, tbi.pre.2024, tbi.pre.2025)

rm(temp.2017, temp.2018, temp.2019, temp.2020, temp.2021, temp.2022, temp.2023, temp.2024, temp.2025)

# 3. Model temporal beta diversity as a function of year and treatment ----
## compare year to starting year (2016) ----
# base models
base <- lme(TBI ~ trt+year, # base
               data = TBI.beg,
               random = ~1|plot)
plot(base)

base.int <- lme(TBI ~ trt*year, # base + int
                data = TBI.beg,
                random = ~1|plot)
plot(base.int)

base.cor <- lme(TBI ~ trt+year, # base + cor
                data = TBI.beg,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))
plot(base.cor)

base.int.cor <- lme(TBI ~ trt*year, # base + int + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(base.int.cor)

# weather models (no INT, no COR)
b.pgs.p <- lme(TBI ~ trt+year+prev.gs.precip, # base + prev gs precip
                data = TBI.beg,
                random = ~1|plot)
plot(b.pgs.p) 

b.pgs.t <- lme(TBI ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
                data = TBI.beg,
                random = ~1|plot)
plot(b.pgs.t) # examine residuals,

b.pgs.p.t <- lme(TBI ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                  data = TBI.beg,
                  random = ~1|plot)
plot(b.pgs.p.t) # examine residuals

b.365.p <- lme(TBI ~ trt+year+tot.precip.365, # base + prev 365 precip
                data = TBI.beg,
                random = ~1|plot)
plot(b.365.p) # examine residuals

b.365.t <- lme(TBI ~ trt+year+mean.temp.365, # base + prev 365 temp
                data = TBI.beg,
                random = ~1|plot)
plot(b.365.t) # examine residuals,

b.365.p.t <- lme(TBI ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                  data = TBI.beg,
                  random = ~1|plot)
plot(b.365.p.t) # examine residuals

# weather models (no INT, with COR)
b.pgs.p.cor <- lme(TBI ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.cor) # examine residuals

b.pgs.t.cor <- lme(TBI ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.pgs.t.cor) # examine residuals

b.pgs.p.t.cor <- lme(TBI ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                      data = TBI.beg,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.t.cor) # examine residuals

b.365.p.cor <- lme(TBI ~ trt+year+tot.precip.365, # base + prev 365 precip + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.365.p.cor) # examine residuals

b.365.t.cor <- lme(TBI ~ trt+year+mean.temp.365, # base + prev 365 temp + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.365.t.cor) # examine residuals

b.365.p.t.cor <- lme(TBI ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                      data = TBI.beg,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(b.365.p.t.cor) # examine residuals


# weather models (with INT, no COR)
b.pgs.p.int <- lme(TBI ~ trt*year*prev.gs.precip, # base + prev gs precip + int
               data = TBI.beg,
               random = ~1|plot)
plot(b.pgs.p.int) 

b.pgs.t.int <- lme(TBI ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
               data = TBI.beg,
               random = ~1|plot)
plot(b.pgs.t.int) # examine residuals

b.pgs.p.t.int <- lme(TBI ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                 data = TBI.beg,
                 random = ~1|plot)
plot(b.pgs.p.t.int) # examine residuals

b.365.p.int <- lme(TBI ~ trt*year*tot.precip.365, # base + prev 365 precip + int
               data = TBI.beg,
               random = ~1|plot)
plot(b.365.p.int) # examine residuals

b.365.t.int <- lme(TBI ~ trt*year*mean.temp.365, # base + prev 365 temp + int
               data = TBI.beg,
               random = ~1|plot)
plot(b.365.t.int) # examine residuals

b.365.p.t.int <- lme(TBI ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + int
                 data = TBI.beg,
                 random = ~1|plot)
plot(b.365.p.t.int) # examine residuals


# weather models (with INT, with COR)
b.pgs.p.cor.int <- lme(TBI ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                   data = TBI.beg,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.cor.int) # examine residuals

b.pgs.t.cor.int <- lme(TBI ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                   data = TBI.beg,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.pgs.t.cor.int) # examine residuals

b.pgs.p.t.cor.int <- lme(TBI ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                     data = TBI.beg,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(b.pgs.p.t.cor.int) # examine residuals

b.365.p.cor.int <- lme(TBI ~ trt*year*tot.precip.365, # base + prev 365 precip + cor + int
                   data = TBI.beg,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.365.p.cor.int) # examine residuals

b.365.t.cor.int <- lme(TBI ~ trt*year*mean.temp.365, # base + prev 365 temp + cor + int
                   data = TBI.beg,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.365.t.cor.int) # examine residuals

b.365.p.t.cor.int <- lme(TBI ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + cor +int
                     data = TBI.beg,
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

## Compare year to previous year ----
# base models
p.base <- lme(TBI ~ trt+year, # base
            data = TBI.pre,
            random = ~1|plot)
plot(p.base)

p.base.int <- lme(TBI ~ trt*year, # base + int
                data = TBI.pre,
                random = ~1|plot)
plot(p.base.int)

p.base.cor <- lme(TBI ~ trt+year, # base + cor
                data = TBI.pre,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))
plot(p.base.cor)

p.base.int.cor <- lme(TBI ~ trt*year, # base + int + cor
                    data = TBI.pre,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(p.base.int.cor)

# weather models (no INT, no COR)
p.pgs.p <- lme(TBI ~ trt+year+prev.gs.precip, # base + prev gs precip
               data = TBI.pre,
               random = ~1|plot)
plot(p.pgs.p) 

p.pgs.t <- lme(TBI ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
               data = TBI.pre,
               random = ~1|plot)
plot(p.pgs.t) # examine residuals,

p.pgs.p.t <- lme(TBI ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = TBI.pre,
                 random = ~1|plot)
plot(p.pgs.p.t) # examine residuals

p.365.p <- lme(TBI ~ trt+year+tot.precip.365, # base + prev 365 precip
               data = TBI.pre,
               random = ~1|plot)
plot(p.365.p) # examine residuals

p.365.t <- lme(TBI ~ trt+year+mean.temp.365, # base + prev 365 temp
               data = TBI.pre,
               random = ~1|plot)
plot(p.365.t) # examine residuals,

p.365.p.t <- lme(TBI ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                 data = TBI.pre,
                 random = ~1|plot)
plot(p.365.p.t) # examine residuals

# weather models (no INT, with COR)
p.pgs.p.cor <- lme(TBI ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                   data = TBI.pre,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(p.pgs.p.cor) # examine residuals

p.pgs.t.cor <- lme(TBI ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                   data = TBI.pre,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(p.pgs.t.cor) # examine residuals

p.pgs.p.t.cor <- lme(TBI ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                     data = TBI.pre,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(p.pgs.p.t.cor) # examine residuals

p.365.p.cor <- lme(TBI ~ trt+year+tot.precip.365, # base + prev 365 precip + cor
                   data = TBI.pre,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(p.365.p.cor) # examine residuals

p.365.t.cor <- lme(TBI ~ trt+year+mean.temp.365, # base + prev 365 temp + cor
                   data = TBI.pre,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(p.365.t.cor) # examine residuals

p.365.p.t.cor <- lme(TBI ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                     data = TBI.pre,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(p.365.p.t.cor) # examine residuals


# weather models (with INT, no COR)
p.pgs.p.int <- lme(TBI ~ trt*year*prev.gs.precip, # base + prev gs precip + int
                   data = TBI.pre,
                   random = ~1|plot)
plot(p.pgs.p.int) 

p.pgs.t.int <- lme(TBI ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
                   data = TBI.pre,
                   random = ~1|plot)
plot(p.pgs.t.int) # examine residuals

p.pgs.p.t.int <- lme(TBI ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                     data = TBI.pre,
                     random = ~1|plot)
plot(p.pgs.p.t.int) # examine residuals

p.365.p.int <- lme(TBI ~ trt*year*tot.precip.365, # base + prev 365 precip + int
                   data = TBI.pre,
                   random = ~1|plot)
plot(p.365.p.int) # examine residuals

p.365.t.int <- lme(TBI ~ trt*year*mean.temp.365, # base + prev 365 temp + int
                   data = TBI.pre,
                   random = ~1|plot)
plot(p.365.t.int) # examine residuals

p.365.p.t.int <- lme(TBI ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + int
                     data = TBI.pre,
                     random = ~1|plot)
plot(p.365.p.t.int) # examine residuals


# weather models (with INT, with COR)
p.pgs.p.cor.int <- lme(TBI ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                       data = TBI.pre,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(p.pgs.p.cor.int) # examine residuals

p.pgs.t.cor.int <- lme(TBI ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                       data = TBI.pre,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(p.pgs.t.cor.int) # examine residuals

p.pgs.p.t.cor.int <- lme(TBI ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                         data = TBI.pre,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(p.pgs.p.t.cor.int) # examine residuals

p.365.p.cor.int <- lme(TBI ~ trt*year*tot.precip.365, # base + prev 365 precip + cor + int
                       data = TBI.pre,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(p.365.p.cor.int) # examine residuals

p.365.t.cor.int <- lme(TBI ~ trt*year*mean.temp.365, # base + prev 365 temp + cor + int
                       data = TBI.pre,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(p.365.t.cor.int) # examine residuals

p.365.p.t.cor.int <- lme(TBI ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + cor +int
                         data = TBI.pre,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(p.365.p.t.cor.int) # examine residuals



## Calculate AIC
myAIC <- c(AIC(p.base), AIC(p.base.cor), AIC(p.base.int), AIC(p.base.int.cor),
           AIC(p.365.p), AIC(p.365.p.cor), AIC(p.365.p.int), AIC(p.365.p.cor.int),
           AIC(p.365.t), AIC(p.365.t.cor), AIC(p.365.t.int), AIC(p.365.t.cor.int),
           AIC(p.365.p.t), AIC(p.365.p.t.cor), AIC(p.365.p.t.int), AIC(p.365.p.t.cor.int),
           AIC(p.pgs.p), AIC(p.pgs.p.cor), AIC(p.pgs.p.int), AIC(p.pgs.p.cor.int),
           AIC(p.pgs.t), AIC(p.pgs.t.cor), AIC(p.pgs.t.int), AIC(p.pgs.t.cor.int),
           AIC(p.pgs.p.t), AIC(p.pgs.p.t.cor), AIC(p.pgs.p.t.int), AIC(p.pgs.p.t.cor.int))
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
summary(p.base.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(p.base.cor, type = 3) #marginal, not sequential

# 4. Plots ----
## comparison to first year ----
## year vs. TBI split by treatment
plot(visreg(b.pgs.t.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 9, 8),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "TBI",
     xlab = "Years since beginning of experiment",
     ylab = "Dissimilarity from starting year",
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
     ylab = "Dissimilarity from the starting year",
     overlay = TRUE)
legend(x = "topright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "darkorange", "deepskyblue3"),
       lwd = 3,
       pch = 1)


## comparison to previous year ----
plot(visreg(p.base.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 9, 8),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "TBI",
     xlab = "Years since beginning of experiment",
     ylab = "Dissimilarity from previous year",
     overlay = TRUE)
legend(x = "topright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "darkorange", "deepskyblue3"),
       lwd = 3,
       pch = 1)

## B-C plots ----
plot(tbi.beg.2017)
plot(tbi.beg.2018)
plot(tbi.beg.2025)
plot(tbi.pre.2023)
plot(tbi.pre.2024)
plot(tbi.pre.2025)

ggplot(TBI.beg, aes(B, C, color = year))+
  geom_point()+
  labs(title = "Losses vs. Gains When Comparing Plot to 2016", x = "B (losses)", y = "C (gains)")+
  geom_abline(intercept = 0, slope = 1)+
  theme_classic()

ggplot(TBI.pre, aes(B, C, color = year))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Losses vs. Gains When Comparing Plot to Prev. Year", x = "B (losses)", y = "C (gains)")+
  theme_classic()

ggplot(TBI.beg, aes(B, C, color = trt))+
  geom_point()+
  labs(title = "Losses vs. Gains When Comparing Plot to 2016", x = "B (losses)", y = "C (gains)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  geom_abline(intercept = 0, slope = 1)+
  theme_classic()

ggplot(TBI.pre, aes(B, C, color = trt))+
  geom_point()+
  geom_abline(intercept = 0, slope = 1)+
  labs(title = "Losses vs. Gains When Comparing Plot to Prev. Year", x = "B (losses)", y = "C (gains)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_classic()
