# Script for modeling alpha diversity
# Robin Bradley
# robinbradley210@gmail.com
# created: 14 July 2025
# last updated: 7 Oct 2025

# Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## packages
library(tidyverse)
library(ggeffects)
library(nlme)
library(lme4)
library(emmeans)
library(car)
library(visreg)
library(knitr)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

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

rich <- left_join(rich, weather.prev.gs) # add weather
rich <- left_join(rich, weather.prev.365)
rich <- rich[, -6]
rich$year <- as.integer(rich$year) # turn into integer

## Scatterplot of richness vs. sampling year
ggplot(rich, aes(x = year, y = richness, color = trt))+
  geom_jitter()+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("Species Richness")+
  xlab("Year")+
  theme_classic()

## linear mixed effects models ----
### response variable: species richness
### treatment as between-plots fixed variable
### plot as random variable
a.base <- lme(richness ~ trt*year, # base
              data = rich,
              random = ~1|plot)
plot(a.base) # examine residuals, look fine

a.pgs.p <- lme(richness ~ trt*year+prev.gs.precip, # base + prev gs precip
               data = rich,
               random = ~1|plot)
plot(a.pgs.p) # examine residuals

a.pgs.t <- lme(richness ~ trt*year+prev.gs.mean.temp, # base + prev gs temp
               data = rich,
               random = ~1|plot)
plot(a.pgs.t) # examine residuals,

a.pgs.p.t <- lme(richness ~ trt*year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = rich,
                 random = ~1|plot)
plot(a.pgs.p.t) # examine residuals

a.365.p <- lme(richness ~ trt*year+tot.precip.365, # base + prev 365 precip
               data = rich,
               random = ~1|plot)
plot(a.pgs.p) # examine residuals

a.365.t <- lme(richness ~ trt*year+mean.temp.365, # base + prev 365 temp
               data = rich,
               random = ~1|plot)
plot(a.365.t) # examine residuals,

a.365.p.t <- lme(richness ~ trt*year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                 data = rich,
                 random = ~1|plot)
plot(a.365.p.t) # examine residuals


# Model with AR(1) temporal autocorrelation structure

a.base.cor <- lme(richness ~ trt*year,  # base + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.base.cor) # examine residuals

a.pgs.p.cor <- lme(richness ~ trt*year+prev.gs.precip, # base + prev gs precip + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.cor) # examine residuals

a.pgs.t.cor <- lme(richness ~ trt*year+prev.gs.mean.temp, # base + prev gs temp + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.t.cor) # examine residuals

a.pgs.p.t.cor <- lme(richness ~ trt*year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                     data = rich,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.t.cor) # examine residuals

a.365.p.cor <- lme(richness ~ trt*year+tot.precip.365, # base + prev 365 precip + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.cor) # examine residuals

a.365.t.cor <- lme(richness ~ trt*year+mean.temp.365, # base + prev 365 temp + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.365.t.cor) # examine residuals

a.365.p.t.cor <- lme(richness ~ trt*year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                     data = rich,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(a.365.p.t.cor) # examine residuals


## Include interaction terms
a.pgs.p.cor.int <- lme(richness ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                       data = rich,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.cor.int) # examine residuals


a.pgs.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.t.cor.int) # examine residuals

a.pgs.p.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor
                     data = rich,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.t.cor.int) # examine residuals


a.365.p.cor.int <- lme(richness ~ trt*year*tot.precip.365, # base + prev 365 precip + cor
                       data = rich,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(a.365.p.cor.int) # examine residuals
  
  
a.365.t.cor.int <- lme(richness ~ trt*year*mean.temp.365, # base + prev 365 temp + cor + int
                       data = rich,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(a.365.t.cor.int) # examine residuals

a.365.p.t.cor.int <- lme(richness ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + cor
                         data = rich,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(a.365.p.t.cor.int) # examine residuals


### Model selection ----
#### Calculate AIC
myAIC <- c(AIC(a.base), AIC(a.base.cor), 
           AIC(a.pgs.p), AIC(a.pgs.t), AIC(a.pgs.p.t), 
           AIC(a.pgs.p.cor), AIC(a.pgs.t.cor), AIC(a.pgs.p.t.cor),
           AIC(a.365.p), AIC(a.365.t), AIC(a.365.p.t), 
           AIC(a.365.p.cor), AIC(a.365.t.cor), AIC(a.365.p.t.cor),
           AIC(a.pgs.t.cor.int), AIC(a.pgs.p.cor.int), AIC(a.pgs.p.t.cor.int), 
           AIC(a.365.p.cor.int), AIC(a.365.t.cor.int), AIC(a.365.p.t.cor.int)
)
delta <- myAIC - min(myAIC)
model <- c("base", "base.cor", 
           "pgs.p", "pgs.t", "pgs.p.t", 
           "pgs.p.cor", "pgs.t.cor", "pgs.p.t.cor",
           "365.p", "365.t", "365.p.t", 
           "365.p.cor", "365.t.cor", "365.p.t.cor",
           "pgs.t.cor.int", "a.pgs.p.cor.int", "a.pgs.p.t.cor.int", 
           "a.365.p.cor.int", "a.365.t.cor.int", "a.365.p.t.cor.int"
           )
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
                   stringsAsFactors = FALSE)


# summary
summary(a.365.t.cor.int)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

coef(a.365.t.cor.int)

# model estimated means for each treatment
emmeans(z1, c("trt"))

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(a.365.t.cor.int, type = 3) #marginal, not sequential
# sig difference by year
# no difference by treatment
# non-sig interaction term


# Plot model fit onto year v. species richness plot
v <- visreg(a.365.t.cor.int,  xvar = "year",  by = "trt",
       gg = TRUE) +
  geom_point(aes(color = rich$mean.temp.365))
v <- visreg(a.365.t.cor.int,  xvar = "mean.temp.365",
            gg = TRUE) 

v
par(mar = c(4.1, 4.4, 4.1, 1.9))
plot(v, 
     overlay = TRUE,
     legend = FALSE,
     points = list(col = c("black", "coral", "deepskyblue3"), cex = 1),
     line = list(col = c("black", "coral", "deepskyblue3")),
     main = "Species Richness Over Time",
     xlab = "Temp",
     ylab = "Species Richness Per Plot",
     xaxp = c(1, 11, 10),
     ylim = c(10, 28))
legend(x = "topleft", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)


## ggplot
# predict model fit
predictions <- ggpredict(a.365.t.cor.int, interaction )

ggplot(predictions, aes(x = x, y = predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(x = "Predictor 1", y = "Predicted Response") +
  theme_minimal()


ggplot(rich,
       aes(x= year, y=richness, color = trt))+
  geom_point()+
  geom_line(aes(y = predict))+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("# of plant species")+
  xlab("year")+
  facet_wrap(~trt)+
  theme_classic()


## ggplot temp v. richness
ggplot(rich,
       aes(x= mean.temp.365, y=richness, color = trt))+
  geom_point()+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("# of plant species")+
  xlab("Temperature")+
  theme_classic()
