# Script for: How does precipitation change influence temporal beta diversity?
# Robin Bradley
# robin.bradley@ubc.ca
# created: 3 Sept 2025
# last updated: 3 Sept 2025

# 1. Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## Packages
library(tidyverse)
library(visreg)
library(betapart)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/cowichan_cleanup.R")

## Calculate Bray-Curtis pairwise dissimilarity between plots in each treatment in each year ----
bc.dist.values <- vegdist(spe.matrix) # calculate Bray-Curtis dissimilarity for each plot-year to every other plot-year
str(bc.dist.values) # check on object

### Convert to dataframe with plot-years as variables
bc.dist <- as.data.frame.table(as.matrix(bc.dist.values))|>
  transform(Var1 = as.character(Var1), Var2 = as.character(Var2)) |>
  subset(Var1<Var2)

### Separate plot-years into their respective plots and years
bc.dist <- bc.dist %>% 
  separate_wider_delim(Var1, "_", names = c("Var1_plot", "Var1_year")) %>%
  separate_wider_delim(Var2, "_", names = c("Var2_plot", "Var2_year"))

### Add treatments for plot-year
bc.dist$Var1_trt <- plots$trt[match(bc.dist$Var1_plot, plots$plot)]
bc.dist$Var2_trt <- plots$trt[match(bc.dist$Var2_plot, plots$plot)]

# 2. Does Bray-Curtis dissimilarity from the initial sampling year change over time? ----
## Filter to only comparisons between plot and itself in 2015 ----
### (aka where Var1_year = 2015 and var1_plot = var2_plot)
bc.dist.init <- filter(bc.dist, Var1_plot == Var2_plot & Var1_year == 2015)

### Clean up data frame for analysis
bc.dist.init <- bc.dist.init[, -c(3, 7)] # remove extra trt and plot column

bc.dist.init <- dplyr::rename(bc.dist.init, # rename columns
                              trt = Var1_trt, 
                              plot = Var1_plot,
                              bc.dist = Freq)

bc.dist.init$trt <- as.factor(bc.dist.init$trt) #convert treatment to factor
bc.dist.init$trt <- factor(bc.dist.init$trt, levels = c("control", "drought", "irrigated"))

bc.dist.init$Var1_year <- as.factor(bc.dist.init$Var1_year) # convert Var1_year to factor
bc.dist.init$Var1_year <- as.integer(bc.dist.init$Var1_year) # then convert year to integer

bc.dist.init$Var2_year <- as.factor(bc.dist.init$Var2_year) # convert Var2_year to factor
bc.dist.init$Var2_year <- factor(bc.dist.init$Var2_year, levels = c("2015", "2016", "2017", "2018", "2019", 
                                                                    "2020", "2021", "2022", "2023", "2024", "2025"))
bc.dist.init$Var2_year <- as.integer(bc.dist.init$Var2_year) # then convert year to integer

bc.dist.init$plot <- as.factor(bc.dist.init$plot) # convert plots to factors
bc.dist.init$plot <- factor(bc.dist.init$plot, levels = c("1", "2", "3", "4", "5", "6", 
                                                    "7", "8", "9", "10", "11", "12", 
                                                    "13", "14", "15"))

## Model pairwise dissimilarity of a plot and itself in 2015 as a function of year and treatment ----
### Scatterplot of pairwise dissimilarity vs. year
ggplot(bc.dist.init, aes(x = Var2_year, y = bc.dist, color = plot))+
  geom_point()+
  geom_line(aes(group = plot))+
  #scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("Bray-Curtis Dissimilarity")+
  xlab("Year")+
  facet_wrap(~trt)+
  theme_classic()

### Not sure how to proceed with model - should I test multiple linear + non-linear models? incorporating cor(AR1)?

## Partition into nestedness and turnover ----
### aka balanced variation and abundance gradients (Balsega 2013)
bc.part <- bray.part(spe.matrix)


# 3. Does Bray-Curtis dissimilarity from the previous sampling year change over time? ----
## Filter to only comparisons between plot and itself in the previous year ----
bc.dist.prev <- filter(bc.dist, Var1_plot == Var2_plot) # filter to only include comparisons b/t plot and itself

bc.dist.prev$Var1_year <- as.factor(bc.dist.prev$Var1_year) # convert Var1_year to factor
bc.dist.prev$Var1_year <- factor(bc.dist.prev$Var1_year, levels = c("2015", "2016", "2017", "2018", "2019", 
                                                                    "2020", "2021", "2022", "2023", "2024", "2025"))
bc.dist.prev$Var1_year <- as.integer(bc.dist.prev$Var1_year) # then convert year to integer

bc.dist.prev$Var2_year <- as.factor(bc.dist.prev$Var2_year) # convert Var2_year to factor
bc.dist.prev$Var2_year <- factor(bc.dist.prev$Var2_year, levels = c("2015", "2016", "2017", "2018", "2019", 
                                                                    "2020", "2021", "2022", "2023", "2024", "2025"))
bc.dist.prev$Var2_year <- as.integer(bc.dist.prev$Var2_year) # then convert year to integer

bc.dist.prev <- filter(bc.dist.prev, Var1_year == (Var2_year-1)) # filter to only include comparison to year before

## Clean up data frame for analysis
bc.dist.prev <- bc.dist.prev[, -c(3, 7)] # remove extra trt and plot column

bc.dist.prev <- dplyr::rename(bc.dist.prev, # rename columns
                              trt = Var1_trt, 
                              plot = Var1_plot,
                              bc.dist = Freq)

bc.dist.prev$trt <- as.factor(bc.dist.prev$trt) #convert treatment to factor
bc.dist.prev$trt <- factor(bc.dist.prev$trt, levels = c("control", "drought", "irrigated"))

bc.dist.prev$plot <- as.factor(bc.dist.prev$plot) # convert plots to factors
bc.dist.prev$plot <- factor(bc.dist.prev$plot, levels = c("1", "2", "3", "4", "5", "6", 
                                                    "7", "8", "9", "10", "11", "12", 
                                                    "13", "14", "15"))

## Model pairwise dissimilarity of a plot and itself in the previous year as a function of year and treatment ----
### Scatterplot of pairwise dissimilarity vs. year
ggplot(bc.dist.prev, aes(x = Var2_year, y = bc.dist, color = plot))+
  geom_point()+
  geom_line(aes(group = plot))+
  #scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("Bray-Curtis Dissimilarity")+
  xlab("Year")+
  facet_wrap(~trt)+
  theme_classic()

### Ask Jenn abt modeling - same as above