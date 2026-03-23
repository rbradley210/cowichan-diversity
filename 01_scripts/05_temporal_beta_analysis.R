# Script for: How does precipitation change influence temporal beta diversity?
# Robin Bradley
# robin.bradley@ubc.ca
# created: 25 November 2025
# last updated: 3 March 2026

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


## Calculating TBI for each year and the starting year (2017) ----
### "D" is the same as the TBI values in the output matrices
# tbi.beg.2017 <- TBI(spe.matrix.2016, spe.matrix.2017,
#                 method = "%difference",
#                 nperm = 99,
#                 BCD = TRUE,
#                 seed. = 42
# )

tbi.beg.2018 <- TBI(spe.matrix.2017, spe.matrix.2018,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2019 <- TBI(spe.matrix.2017, spe.matrix.2019,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2020 <- TBI(spe.matrix.2017, spe.matrix.2020,
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

tbi.beg.2022 <- TBI(spe.matrix.2017, spe.matrix.2022,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2023 <- TBI(spe.matrix.2017, spe.matrix.2023,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2024 <- TBI(spe.matrix.2017, spe.matrix.2024,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)

tbi.beg.2025 <- TBI(spe.matrix.2017, spe.matrix.2025,
                method = "%difference",
                nperm = 99,
                BCD = TRUE,
                seed. = 42
)


## Pull TBI values and combine into single dataframe
## pull bcd dataframes and add column for year and plotu
# temp.2017 <- tbi.beg.2017$BCD.mat
# temp.2017$year <- 2017
# temp.2017$plot <- plots$plot

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
TBI.beg <- rbind(temp.2018,
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
TBI.beg <- left_join(TBI.beg, weather.prev.sp)
TBI.beg <- left_join(TBI.beg, weather.prev.su)
TBI.beg <- TBI.beg[, -9]

TBI.beg$year <- as.integer(as.factor(TBI.beg$year)) # convert year to integer

## Calculating TBI for each year and the previous year ----
### "D" is the same as the TBI values in the output matrices
# tbi.pre.2017 <- TBI(spe.matrix.2016, spe.matrix.2017,
#                     method = "%difference",
#                     nperm = 999,
#                     BCD = TRUE,
#                     seed. = 42
# )

tbi.pre.2018 <- TBI(spe.matrix.2017, spe.matrix.2018,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2019 <- TBI(spe.matrix.2018, spe.matrix.2019,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2020 <- TBI(spe.matrix.2019, spe.matrix.2020,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2021 <- TBI(spe.matrix.2020, spe.matrix.2021,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2022 <- TBI(spe.matrix.2021, spe.matrix.2022,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2023 <- TBI(spe.matrix.2022, spe.matrix.2023,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2024 <- TBI(spe.matrix.2023, spe.matrix.2024,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)

tbi.pre.2025 <- TBI(spe.matrix.2024, spe.matrix.2025,
                    method = "%difference",
                    nperm = 99,
                    BCD = TRUE,
                    seed. = 42
)


## Pull TBI values and combine into single dataframe
## pull bcd dataframes and add column for year and plot
# temp.2017 <- tbi.pre.2017$BCD.mat
# temp.2017$year <- 2017
# temp.2017$plot <- plots$plot

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
TBI.pre <- rbind(temp.2018,
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
TBI.pre <- left_join(TBI.pre, weather.prev.sp)
TBI.pre <- left_join(TBI.pre, weather.prev.su)
TBI.pre <- TBI.pre[, -9]

TBI.pre$year <- as.integer(as.factor(TBI.pre$year)) # convert year to integer

## remove spe.matrix, tbi and temp dataframes to clean up environment ----
rm(spe.matrix.2015, spe.matrix.2016, spe.matrix.2017, spe.matrix.2018, spe.matrix.2019, spe.matrix.2020, spe.matrix.2021, spe.matrix.2022, spe.matrix.2023, spe.matrix.2024, spe.matrix.2025)

rm(#tbi.beg.2017, 
   tbi.beg.2018, tbi.beg.2019, tbi.beg.2020, tbi.beg.2021, tbi.beg.2022, tbi.beg.2023, tbi.beg.2024, tbi.beg.2025)
rm(#tbi.pre.2017, 
   tbi.pre.2018, tbi.pre.2019, tbi.pre.2020, tbi.pre.2021, tbi.pre.2022, tbi.pre.2023, tbi.pre.2024, tbi.pre.2025)

rm(#temp.2017, 
   temp.2018, temp.2019, temp.2020, temp.2021, temp.2022, temp.2023, temp.2024, temp.2025)

# 3. Model temporal beta diversity as a function of year and treatment ----
## compare year to starting year (2017) ----
# null models
b.null <- lme(TBI~1, 
              data = TBI.beg, 
              random = ~1|plot)

b.null.cor <- lme(TBI ~ 1,
                  data = TBI.beg,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

b.yr <- lme(TBI~year,
            data = TBI.beg,
            random = ~1|plot)

b.yr.cor <- lme(TBI ~ year,
               data = TBI.beg,
               random = ~1|plot,
               correlation = corAR1(form = ~year|plot))

b.trt <- lme(TBI~trt,
            data = TBI.beg,
            random = ~1|plot)

b.trt.cor <- lme(TBI ~ trt,
                 data = TBI.beg,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

# base models
b.base <- lme(TBI ~ trt+year, # base
               data = TBI.beg,
               random = ~1|plot)
plot(b.base)

b.base.int <- lme(TBI ~ trt*year, # base + int
                data = TBI.beg,
                random = ~1|plot)
plot(b.base.int)

b.base.cor <- lme(TBI ~ trt+year, # base + cor
                data = TBI.beg,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))
plot(b.base.cor)

b.base.int.cor <- lme(TBI ~ trt*year, # base + int + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.base.int.cor)

# weather models 
## previous growing season
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

## previous spring
b.sp.p <- lme(TBI ~ trt+year+prev.sp.precip, # base + prev sp precip
               data = TBI.beg,
               random = ~1|plot)
plot(b.sp.p) 

b.sp.t <- lme(TBI ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
               data = TBI.beg,
               random = ~1|plot)
plot(b.sp.t) # examine residuals,

b.sp.p.t <- lme(TBI ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                 data = TBI.beg,
                 random = ~1|plot)
plot(b.sp.p.t) # examine residuals

## previous summer
b.su.p <- lme(TBI ~ trt+year+prev.su.precip, # base + prev su precip
              data = TBI.beg,
              random = ~1|plot)
plot(b.su.p) 

b.su.t <- lme(TBI ~ trt+year+prev.su.mean.temp, # base + prev su temp
              data = TBI.beg,
              random = ~1|plot)
plot(b.su.t) # examine residuals,

b.su.p.t <- lme(TBI ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                data = TBI.beg,
                random = ~1|plot)
plot(b.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
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

## previous spring
b.sp.p.cor <- lme(TBI ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                   data = TBI.beg,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.sp.p.cor) # examine residuals

b.sp.t.cor <- lme(TBI ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                   data = TBI.beg,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(b.sp.t.cor) # examine residuals

b.sp.p.t.cor <- lme(TBI ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                     data = TBI.beg,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(b.sp.p.t.cor) # examine residuals

## previous summer
b.su.p.cor <- lme(TBI ~ trt+year+prev.su.precip, # base + prev su precip + cor
                  data = TBI.beg,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(b.su.p.cor) # examine residuals

b.su.t.cor <- lme(TBI ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                  data = TBI.beg,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(b.su.t.cor) # examine residuals

b.su.p.t.cor <- lme(TBI ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                    data = TBI.beg,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(b.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
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

## previous spring
b.sp.p.int <- lme(TBI ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                   data = TBI.beg,
                   random = ~1|plot)
plot(b.sp.p.int) 

b.sp.t.int <- lme(TBI ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                   data = TBI.beg,
                   random = ~1|plot)
plot(b.sp.t.int) # examine residuals

b.sp.p.t.int <- lme(TBI ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                     data = TBI.beg,
                     random = ~1|plot)
plot(b.sp.p.t.int) # examine residuals

## previous summer
b.su.p.int <- lme(TBI ~ trt*year*prev.su.precip, # base + prev su precip + int
                  data = TBI.beg,
                  random = ~1|plot)
plot(b.su.p.int) 

b.su.t.int <- lme(TBI ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                  data = TBI.beg,
                  random = ~1|plot)
plot(b.su.t.int) # examine residuals

b.su.p.t.int <- lme(TBI ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                    data = TBI.beg,
                    random = ~1|plot)
plot(b.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
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

## previous spring
b.sp.p.cor.int <- lme(TBI ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                       data = TBI.beg,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(b.sp.p.cor.int) # examine residuals

b.sp.t.cor.int <- lme(TBI ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                       data = TBI.beg,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(b.sp.t.cor.int) # examine residuals

b.sp.p.t.cor.int <- lme(TBI ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                         data = TBI.beg,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(b.sp.p.t.cor.int) # examine residuals

## previous summer
b.su.p.cor.int <- lme(TBI ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                      data = TBI.beg,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(b.su.p.cor.int) # examine residuals

b.su.t.cor.int <- lme(TBI ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                      data = TBI.beg,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(b.su.t.cor.int) # examine residuals

b.su.p.t.cor.int <- lme(TBI ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                        data = TBI.beg,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(b.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(b.null), AIC(b.null.cor), AIC(b.yr), AIC(b.yr.cor), AIC(b.trt), AIC(b.trt.cor),
           AIC(b.base), AIC(b.base.cor), AIC(b.base.int), AIC(b.base.int.cor),
           
           AIC(b.pgs.p), AIC(b.pgs.p.cor), AIC(b.pgs.p.int), AIC(b.pgs.p.cor.int),
           AIC(b.pgs.t), AIC(b.pgs.t.cor), AIC(b.pgs.t.int), AIC(b.pgs.t.cor.int),
           AIC(b.pgs.p.t), AIC(b.pgs.p.t.cor), AIC(b.pgs.p.t.int), AIC(b.pgs.p.t.cor.int),
           
           AIC(b.sp.p), AIC(b.sp.p.cor), AIC(b.sp.p.int), AIC(b.sp.p.cor.int),
           AIC(b.sp.t), AIC(b.sp.t.cor), AIC(b.sp.t.int), AIC(b.sp.t.cor.int),
           AIC(b.sp.p.t), AIC(b.sp.p.t.cor), AIC(b.sp.p.t.int), AIC(b.sp.p.t.cor.int),
           
           AIC(b.su.p), AIC(b.su.p.cor), AIC(b.su.p.int), AIC(b.su.p.cor.int),
           AIC(b.su.t), AIC(b.su.t.cor), AIC(b.su.t.int), AIC(b.su.t.cor.int),
           AIC(b.su.p.t), AIC(b.su.p.t.cor), AIC(b.su.p.t.int), AIC(b.su.p.t.cor.int))
delta <- myAIC - min(myAIC)
model <- c("null","null.cor", "year", "year.cor", "trt", "trt.cor",
           "base", "base.cor", "base.int", "base.cor.int",

           "pgs.p", "pgs.p.cor", "pgs.p.int", "pgs.p.cor.int",
           "pgs.t", "pgs.t.cor", "pgs.t.int", "pgs.t.cor.int",
           "pgs.p.t", "pgs.p.t.cor", "pgs.p.t.int", "pgs.p.t.cor.int",
           
           "sp.p", "sp.p.cor", "sp.p.int", "sp.p.cor.int",
           "sp.t", "sp.t.cor", "sp.t.int", "sp.t.cor.int",
           "sp.p.t", "sp.p.t.cor", "sp.p.t.int", "sp.p.t.cor.int",
           
           "su.p", "su.p.cor", "su.p.int", "su.p.cor.int",
           "su.t", "su.t.cor", "su.t.int", "su.t.cor.int",
           "su.p.t", "su.p.t.cor", "su.p.t.int", "su.p.t.cor.int")
beg.TBI.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                   stringsAsFactors = FALSE)

# summary
summary(base)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(base, type = 3) #marginal, not sequential

## Compare year to previous year ----
# null models
p.null <- lme(TBI~1, 
              data = TBI.pre, 
              random = ~1|plot)

p.null.cor <- lme(TBI ~ 1,
                  data = TBI.pre,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

p.yr <- lme(TBI~year,
            data = TBI.pre,
            random = ~1|plot)

p.yr.cor <- lme(TBI ~ year,
                data = TBI.pre,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))

p.trt <- lme(TBI~trt,
             data = TBI.pre,
             random = ~1|plot)

p.trt.cor <- lme(TBI ~ trt,
                 data = TBI.pre,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

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

# weather models 
## previous growing season
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

## previous spring
p.sp.p <- lme(TBI ~ trt+year+prev.sp.precip, # base + prev sp precip
              data = TBI.pre,
              random = ~1|plot)
plot(p.sp.p) 

p.sp.t <- lme(TBI ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
              data = TBI.pre,
              random = ~1|plot)
plot(p.sp.t) # examine residuals,

p.sp.p.t <- lme(TBI ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                data = TBI.pre,
                random = ~1|plot)
plot(p.sp.p.t) # examine residuals

## previous summer
p.su.p <- lme(TBI ~ trt+year+prev.su.precip, # base + prev su precip
              data = TBI.pre,
              random = ~1|plot)
plot(p.su.p) 

p.su.t <- lme(TBI ~ trt+year+prev.su.mean.temp, # base + prev su temp
              data = TBI.pre,
              random = ~1|plot)
plot(p.su.t) # examine residuals,

p.su.p.t <- lme(TBI ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                data = TBI.pre,
                random = ~1|plot)
plot(p.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
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

## previous spring
p.sp.p.cor <- lme(TBI ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                  data = TBI.pre,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(p.sp.p.cor) # examine residuals

p.sp.t.cor <- lme(TBI ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                  data = TBI.pre,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(p.sp.t.cor) # examine residuals

p.sp.p.t.cor <- lme(TBI ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                    data = TBI.pre,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(p.sp.p.t.cor) # examine residuals

## previous summer
p.su.p.cor <- lme(TBI ~ trt+year+prev.su.precip, # base + prev su precip + cor
                  data = TBI.pre,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(p.su.p.cor) # examine residuals

p.su.t.cor <- lme(TBI ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                  data = TBI.pre,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(p.su.t.cor) # examine residuals

p.su.p.t.cor <- lme(TBI ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                    data = TBI.pre,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(p.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
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

## previous spring
p.sp.p.int <- lme(TBI ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                  data = TBI.pre,
                  random = ~1|plot)
plot(p.sp.p.int) 

p.sp.t.int <- lme(TBI ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                  data = TBI.pre,
                  random = ~1|plot)
plot(p.sp.t.int) # examine residuals

p.sp.p.t.int <- lme(TBI ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                    data = TBI.pre,
                    random = ~1|plot)
plot(p.sp.p.t.int) # examine residuals

## previous summer
p.su.p.int <- lme(TBI ~ trt*year*prev.su.precip, # base + prev su precip + int
                  data = TBI.pre,
                  random = ~1|plot)
plot(p.su.p.int) 

p.su.t.int <- lme(TBI ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                  data = TBI.pre,
                  random = ~1|plot)
plot(p.su.t.int) # examine residuals

p.su.p.t.int <- lme(TBI ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                    data = TBI.pre,
                    random = ~1|plot)
plot(p.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
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

## previous spring
p.sp.p.cor.int <- lme(TBI ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                      data = TBI.pre,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(p.sp.p.cor.int) # examine residuals

p.sp.t.cor.int <- lme(TBI ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                      data = TBI.pre,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(p.sp.t.cor.int) # examine residuals

p.sp.p.t.cor.int <- lme(TBI ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                        data = TBI.pre,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(p.sp.p.t.cor.int) # examine residuals

## previous summer
p.su.p.cor.int <- lme(TBI ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                      data = TBI.pre,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(p.su.p.cor.int) # examine residuals

p.su.t.cor.int <- lme(TBI ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                      data = TBI.pre,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(p.su.t.cor.int) # examine residuals

p.su.p.t.cor.int <- lme(TBI ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                        data = TBI.pre,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(p.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(p.null), AIC(p.null.cor), AIC(p.yr), AIC(p.yr.cor), AIC(p.trt), AIC(p.trt.cor),
           AIC(p.base), AIC(p.base.cor), AIC(p.base.int), AIC(p.base.int.cor),
           
           AIC(p.pgs.p), AIC(p.pgs.p.cor), AIC(p.pgs.p.int), AIC(p.pgs.p.cor.int),
           AIC(p.pgs.t), AIC(p.pgs.t.cor), AIC(p.pgs.t.int), AIC(p.pgs.t.cor.int),
           AIC(p.pgs.p.t), AIC(p.pgs.p.t.cor), AIC(p.pgs.p.t.int), AIC(p.pgs.p.t.cor.int),
           
           AIC(p.sp.p), AIC(p.sp.p.cor), AIC(p.sp.p.int), AIC(p.sp.p.cor.int),
           AIC(p.sp.t), AIC(p.sp.t.cor), AIC(p.sp.t.int), AIC(p.sp.t.cor.int),
           AIC(p.sp.p.t), AIC(p.sp.p.t.cor), AIC(p.sp.p.t.int), AIC(p.sp.p.t.cor.int),
           
           AIC(p.su.p), AIC(p.su.p.cor), AIC(p.su.p.int), AIC(p.su.p.cor.int),
           AIC(p.su.t), AIC(p.su.t.cor), AIC(p.su.t.int), AIC(p.su.t.cor.int),
           AIC(p.su.p.t), AIC(p.su.p.t.cor), AIC(p.su.p.t.int), AIC(p.su.p.t.cor.int))
delta <- myAIC - min(myAIC)
model <- c("null","null.cor", "year", "year.cor", "trt", "trt.cor",
           "base", "base.cor", "base.int", "base.cor.int",
           
           "pgs.p", "pgs.p.cor", "pgs.p.int", "pgs.p.cor.int",
           "pgs.t", "pgs.t.cor", "pgs.t.int", "pgs.t.cor.int",
           "pgs.p.t", "pgs.p.t.cor", "pgs.p.t.int", "pgs.p.t.cor.int",
           
           "sp.p", "sp.p.cor", "sp.p.int", "sp.p.cor.int",
           "sp.t", "sp.t.cor", "sp.t.int", "sp.t.cor.int",
           "sp.p.t", "sp.p.t.cor", "sp.p.t.int", "sp.p.t.cor.int",
           
           "su.p", "su.p.cor", "su.p.int", "su.p.cor.int",
           "su.t", "su.t.cor", "su.t.int", "su.t.cor.int",
           "su.p.t", "su.p.t.cor", "su.p.t.int", "su.p.t.cor.int")
pre.TBI.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                          stringsAsFactors = FALSE)

# summary
summary(p.365.t.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(p.365.t.cor, type = 3) #marginal, not sequential

# 4. Model B:C ratio ----

# 5. Plots ----
## comparison to first year ----
## year vs. TBI split by treatment
plot(visreg(base, xvar = "year", 
            #by = "trt",
            overlay = TRUE, plot = FALSE),
     xaxp = c(1, 9, 8),
     legend = FALSE,
     points = list(#col = c("black", "darkorange", "deepskyblue2"), 
                   cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Temporal Beta Diverisity Index",
     xlab = "Years since beginning of experiment",
     ylab = "Dissimilarity from starting year",
     overlay = TRUE)
legend(x = "bottomright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "darkorange", "deepskyblue3"),
       lwd = 3,
       pch = 1)

## mean previous growing season temp vs. TBI, split by trt ----
# plot(visreg(b.pgs.t.cor, xvar = "prev.gs.mean.temp", by = "trt", overlay = TRUE, plot = FALSE),
#      #xaxp = c(1, 9, 8),
#      legend = FALSE,
#      points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
#      line = list(col = c("black", "darkorange", "deepskyblue2")),
#      main = "TBI",
#      xlab = "Mean Temperature (°C) in Previous Growing Season",
#      ylab = "Dissimilarity from the starting year",
#      overlay = TRUE)
# legend(x = "topright", 
#        legend = c("Control", "Drought", "Irrigated"),
#        col = c("black", "darkorange", "deepskyblue3"),
#        lwd = 3,
#        pch = 1)


## comparison to previous year ----
plot(visreg(p.365.t.cor, xvar = "year", 
            #by = "trt", 
            overlay = TRUE, plot = FALSE),
     xaxp = c(1, 9, 8),
     legend = FALSE,
     points = list(#col = c("black", "darkorange", "deepskyblue2"),
                   cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Temporal Beta Diversity Index",
     xlab = "Years since beginning of experiment",
     ylab = "Dissimilarity from previous year",
     overlay = TRUE)
legend(x = "topright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "darkorange", "deepskyblue3"),
       lwd = 3,
       pch = 1)

plot(visreg(p.365.t.cor, xvar = "mean.temp.365", 
            #by = "trt", 
            overlay = TRUE, plot = FALSE),
     #xaxp = c(1, 9, 8),
     legend = FALSE,
     points = list(#col = c("black", "darkorange", "deepskyblue2"), 
                   cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Temporal Beta Diversity Index",
     xlab = "Mean temperature (°C) in previous 365 days",
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
