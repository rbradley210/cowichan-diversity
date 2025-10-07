# Script for: Does precipitation change cause plant communities to homogenize or differentiate?
# Robin Bradley
# robin.bradley@ubc.ca
# created: 28 Aug 2025
# last updated: 3 Sept 2025

# Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## Packages
library(tidyverse)
library(lme4)
library(nlme)
library(car)
library(visreg)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

# Does precipitation change cause plant communities to homogenize or differentiate? ----
## Calculate Bray-Curtis pairwise dissimilarity between plots in each treatment in each year ----
bc.dist.values <- vegdist(spe.matrix) # calculate Bray-Curtis dissimilarity for each plot-year to every other plot-year
str(bc.dist.values) # check on object

### Convert to dataframe with plot-years as variables
bc.dist <- as.data.frame.table(as.matrix(bc.dist.values))|>
  transform(Var1 = as.character(Var1), Var2 = as.character(Var2)) |>
  subset(Var1<Var2)

### separate plot-years into their respective plots and years
bc.dist <- bc.dist %>% 
  separate_wider_delim(Var1, "_", names = c("Var1_plot", "Var1_year")) %>%
  separate_wider_delim(Var2, "_", names = c("Var2_plot", "Var2_year"))

### add treatments for plot-year
bc.dist$Var1_trt <- plots$trt[match(bc.dist$Var1_plot, plots$plot)]
bc.dist$Var2_trt <- plots$trt[match(bc.dist$Var2_plot, plots$plot)]

### Filter for only values where treatments and years match
ty.match <- filter(bc.dist, Var1_trt == Var2_trt & Var1_year == Var2_year)

### add weather data
ty.match <- left_join(ty.match, 
                      weather.prev.gs,
                      by = c("Var1_year" = "year"))
ty.match <- left_join(ty.match,
            weather.prev.365,
            by = c("Var1_year" = "year"))

### Clean up new dataframe for analysis
ty.match <- ty.match[, -c(2, 6, 8)] # remove extra year and treatment columns
ty.match <- ty.match %>%
  mutate(plot_pair = paste(Var1_plot, Var2_plot, sep = "_")) %>% # combine plot-pairs into one column
  dplyr::rename(year = Var2_year, # rename columns
                trt = Var2_trt,
                bc.dist = Freq,
                )
ty.match$trt <- as.factor(ty.match$trt) #convert treatment to factor
ty.match$trt <- factor(ty.match$trt, levels = c("control", "drought", "irrigated"))

ty.match$year <- as.factor(ty.match$year) # convert year to factor
ty.match$year <- factor(ty.match$year, levels = c("2015", "2016", "2017", "2018", "2019", 
                                                    "2020", "2021", "2022", "2023", "2024", "2025"))
ty.match$year <- as.integer(ty.match$year) # then convert year to integer

ty.match$plot_pair <- as.factor(ty.match$plot_pair) # convert plot-pairs to factors

## Model pairwise dissimilarity of plots within each treatment as a function of year and treatment ----
### Scatterplot of pairwise dissimilarity vs. year
ggplot(ty.match, aes(x = year, y = bc.dist, color = plot_pair))+
  geom_point()+
  geom_line(aes(group = plot_pair))+
  #scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("Bray-Curtis Dissimilarity")+
  xlab("Year")+
  facet_wrap(~trt)+
  theme_classic()

### Linear Mixed Effects Model ----
### response variable: B-C dissimilarity
### treatment as between-plots fixed variable
### plot_pair as random variable

z1 <- lme(bc.dist ~ trt*year, # base
          data = ty.match,
          random = ~1|plot_pair)
plot(z1) # examine residuals, look fine

z2 <- lme(bc.dist ~ trt*year+gs.precip, # base + precip
          data = ty.match,
          random = ~1|plot_pair)
plot(z2) # examine residuals,

z3 <- lme(bc.dist ~ trt*year+gs.mean.temp, # base + temp
          data = ty.match,
          random = ~1|plot_pair)
plot(z3) # examine residuals,

z4 <- lme(bc.dist ~ trt*year+gs.mean.temp+gs.precip, # base + temp + precip
          data = ty.match,
          random = ~1|plot_pair)
plot(z4) # examine residuals

### Investigate autocorrelation ----
# Plot auto-correlation function 
E <- residuals(z1, type = "normalized") 
acf(E, main = "Auto-correlation plot for residuals") # appears to be autocorrelation present

E <- residuals(z2, type = "normalized") 
acf(E, main = "Auto-correlation plot for residuals")

E <- residuals(z3, type = "normalized") 
acf(E, main = "Auto-correlation plot for residuals")

E <- residuals(z4, type = "normalized") 
acf(E, main = "Auto-correlation plot for residuals")

# Model with AR(1) temporal autocorrelation structure
z5 <- lme(bc.dist ~ trt*year,  # base + cor
          data = ty.match,
          random = ~1|plot_pair,
          correlation = corAR1(form = ~year|plot_pair))
plot(z5) # examine residuals

z6 <- lme(bc.dist ~ trt*year+gs.precip, # base + precip + core
          data = ty.match,
          random = ~1|plot_pair,
          correlation = corAR1(form = ~year|plot_pair))
plot(z6) # examine residuals

z7 <- lme(bc.dist ~ trt*year+gs.mean.temp, # base + temp + core
          data = ty.match,
          random = ~1|plot_pair,
          correlation = corAR1(form = ~year|plot_pair))
plot(z7) # examine residuals

z8 <- lme(bc.dist ~ trt*year+gs.precip+gs.mean.temp, # base + precip + temp + core
          data = ty.match,
          random = ~1|plot_pair,
          correlation = corAR1(form = ~year|plot_pair))
plot(z8) # examine residuals

### Model selection ----
#### Calculate AIC
myAIC <- c(AIC(z1), AIC(z2), 
           AIC(z3), AIC(z4),
           AIC(z5), AIC(z6),
           AIC(z7), AIC(z8))
delta <- myAIC - min(myAIC)
model <- c("Model 1", "Model 2", 
           "Model 3", "Model 4",
           "Model 5", "Model 6",
           "Model 7", "Model 8")
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
                   stringsAsFactors = FALSE)
# model five best fit

### Testing null hypothesis (ANOVA) ----
## of no difference in mean dissimilarity within treatments over time or between treatments (or the interaction of year and treatment)
Anova(z5, type = 3) #marginal, not sequential

# Sig: interaction b/t trt and yr
# marginally sig: year and treatment

### Plot model fit onto year vs. B-C dissimilarity plot ----
v <- visreg(z5, xvar = "year", by = "trt",
       overlay = TRUE,
       legend = TRUE)

par(mar = c(4.1, 4.4, 4.1, 1.9))
plot(v, 
     overlay = TRUE,
     legend = FALSE,
     points = list(col = c("black", "coral", "deepskyblue3"), cex = 1),
     line = list(col = c("black", "coral", "deepskyblue3")),
     main = "B-C Disimilarity within Treatments vs. Year",
     xlab = "Year",
     ylab = "Bray-Curtis Dissimilarity Between Plots",
     xaxp = c(1, 11, 10),
     ylim = c(0.2, 1))
legend(x = "bottomright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)
