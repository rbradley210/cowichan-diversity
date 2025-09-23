# Script for modeling alpha diversity
# Robin Bradley
# robinbradley210@gmail.com
# created: 14 July 2025
# last updated: 16 July 2025

# Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## packages
library(tidyverse)
library(nlme)
library(lme4)
library(emmeans)
library(car)
library(visreg)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/cowichan_cleanup.R")

# Data Exploration ----
# compare species richness across treatments
boxplot(specnumber(spe.matrix) ~ plot_year$trt, ylab = "# of plant species", xlab = " ")

# compare species richness across years
boxplot(specnumber(spe.matrix) ~ plot_year$year, ylab = "# of plant species", xlab = " ")

# compare species richness across plots
boxplot(specnumber(spe.matrix) ~ plot_year$plot, ylab = "# of plant species", xlab = " ")

# compare species richness across treatments and years
ggplot(spe.matrix,aes(x=plot_year$year, y=specnumber(spe.matrix), color = plot_year$trt))+
  geom_boxplot()+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("# of plant species")+
  xlab("Year")+
  theme_classic()

# Model the species richness of the plots in each treatment as a function of sampling year ----
## Pull richness data
rich <- as.data.frame(specnumber(spe.matrix)) # convert values to data frame
rich <- rownames_to_column(rich, var = "plot_year") # make rownames (plot_year) to column
names(rich)[names(rich) == "specnumber(spe.matrix)"] <- "richness" # rename richness column
rich <- left_join(plot_year, rich, by = "plot_year") # join with plot_year values
rich$year <- as.integer(rich$year) # turn into integer

## Scatterplot of richness vs. sampling year
ggplot(rich, aes(x = year, y = richness, color = trt))+
  geom_jitter()+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("Species Richness")+
  xlab("Year")+
  theme_classic()

## linear mixed effects model
### response variable: species richness
### treatment as between-plots fixed variable
### plot as random variable

z1 <- lme(richness ~ trt*year,  
          random = ~1|plot, rich) # without autoregressive structure
plot(z1) # examine residuals, look fine

# plot auto-correlation function 
E <- residuals(z1, type = "normalized")
acf(E, main = "Auto-correlation plot for residuals")

# from examination of plot, the residuals do appear to have autocorrelation

## make model with autoregression and compare using AIC to determine if it is a better fit

### include AR(1) autoregressive structure to account for temporal autocorrelation
z <- lme(richness ~ trt*year, random = ~1|plot,
         correlation = corAR1(form = ~year|plot), rich) # with autoregressive structure
plot(z) # examine residuals, looks fine?

### Calculate the AIC for the two models and the AIC difference
AIC(z1) # without AR(1): 795.6776
AIC(z) # with AR(1): 791.097
myAIC <- c(AIC(z1), AIC(z))
delta <- myAIC - min(myAIC)
delta # 4.58

## So, adding autoregressive structure does nothing for the model fit

## Continue analysis with basic model (not correcting for temporal autocorrelation, as it does not improve model fit?

# summary
summary(z1)

# model estimated means for each treatment
emmeans(z1, c("trt"))

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(z1, type = 3) #marginal, not sequential
# sig difference by year
# no difference by treatment
# non-sig interaction term


# Plot model fit onto year v. species richness plot
v <- visreg(z1,  xvar = "year", by = "trt",
       overlay = TRUE,
       legend = FALSE)

par(mar = c(4.1, 4.4, 4.1, 1.9))
plot(v, 
     overlay = TRUE,
     legend = FALSE,
     points = list(col = c("black", "coral", "deepskyblue3"), cex = 1),
     line = list(col = c("black", "coral", "deepskyblue3")),
     main = "Species Richness Over Time",
     xlab = "Year",
     ylab = "Species Richness Per Plot",
     xaxp = c(1, 11, 10),
     ylim = c(10, 28))
legend(x = "topleft", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)

