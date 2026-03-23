# Script for modeling alpha diversity
# Robin Bradley
# robinbradley210@gmail.com
# created: 14 July 2025
# last updated: 26 March 2026

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

## source traits data
traits <- read.csv("species_traits_2025.csv")

# 1. Data Exploration ----
# compare species richness across treatments
boxplot(specnumber(spe.matrix) ~ plot_year$trt, ylab = "# of plant species", xlab = " ")

# compare species richness across years
boxplot(specnumber(spe.matrix) ~ plot_year$year, ylab = "# of plant species", xlab = " ")

# compare species richness across plots
boxplot(specnumber(spe.matrix) ~ plot_year$plot, ylab = "# of plant species", xlab = " ")

# compare species richness across treatments and years
ggplot(spe.matrix,aes(x=plot_year$year, y=specnumber(spe.matrix), color = plot_year$trt))+
  geom_point()+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("# of plant species")+
  xlab("Year")+
  theme_classic()

# 2. Model overall species richness as a function of year, treatment, and climate ----
## Pull richness data
rich <- as.data.frame(specnumber(spe.matrix)) # convert values to data frame
rich <- rownames_to_column(rich, var = "plot_year") # make rownames (plot_year) to column
names(rich)[names(rich) == "specnumber(spe.matrix)"] <- "richness" # rename richness column
rich <- left_join(plot_year, rich, by = "plot_year") # join with plot_year values

rich <- left_join(rich, weather.prev.gs) # add weather
rich <- left_join(rich, weather.prev.sp)
rich <- left_join(rich, weather.prev.su)
rich <- rich[, -6]

rich$year <- as.integer(as.factor(rich$year))


## linear mixed effects models ----
### response variable: species richness
### treatment as between-plots fixed variable
### plot as random variable
# null models
a.null <- lme(richness~1, 
              data = rich, 
              random = ~1|plot)

a.null.cor <- lme(richness ~ 1,
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

a.yr <- lme(richness~year,
            data = rich,
            random = ~1|plot)

a.yr.cor <- lme(richness ~ year,
                data = rich,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))

a.trt <- lme(richness~trt,
             data = rich,
             random = ~1|plot)

a.trt.cor <- lme(richness ~ trt,
                 data = rich,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

# base models
a.base <- lme(richness ~ trt+year, # base
              data = rich,
              random = ~1|plot)
plot(a.base)

a.base.int <- lme(richness ~ trt*year, # base + int
                  data = rich,
                  random = ~1|plot)
plot(a.base.int)

a.base.cor <- lme(richness ~ trt+year, # base + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.base.cor)

a.base.int.cor <- lme(richness ~ trt*year, # base + int + cor
                      data = rich,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(a.base.int.cor)

# weather models 
## previous growing season
a.pgs.p <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip
               data = rich,
               random = ~1|plot)
plot(a.pgs.p) 

a.pgs.t <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
               data = rich,
               random = ~1|plot)
plot(a.pgs.t) # examine residuals,

a.pgs.p.t <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = rich,
                 random = ~1|plot)
plot(a.pgs.p.t) # examine residuals

## previous spring
a.sp.p <- lme(richness ~ trt+year+prev.sp.precip, # base + prev sp precip
              data = rich,
              random = ~1|plot)
plot(a.sp.p) 

a.sp.t <- lme(richness ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
              data = rich,
              random = ~1|plot)
plot(a.sp.t) # examine residuals,

a.sp.p.t <- lme(richness ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                data = rich,
                random = ~1|plot)
plot(a.sp.p.t) # examine residuals

## previous summer
a.su.p <- lme(richness ~ trt+year+prev.su.precip, # base + prev su precip
              data = rich,
              random = ~1|plot)
plot(a.su.p) 

a.su.t <- lme(richness ~ trt+year+prev.su.mean.temp, # base + prev su temp
              data = rich,
              random = ~1|plot)
plot(a.su.t) # examine residuals,

a.su.p.t <- lme(richness ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                data = rich,
                random = ~1|plot)
plot(a.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
a.pgs.p.cor <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.cor) # examine residuals

a.pgs.t.cor <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.t.cor) # examine residuals

a.pgs.p.t.cor <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                     data = rich,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.t.cor) # examine residuals

## previous spring
a.sp.p.cor <- lme(richness ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.sp.p.cor) # examine residuals

a.sp.t.cor <- lme(richness ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.sp.t.cor) # examine residuals

a.sp.p.t.cor <- lme(richness ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                    data = rich,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(a.sp.p.t.cor) # examine residuals

## previous summer
a.su.p.cor <- lme(richness ~ trt+year+prev.su.precip, # base + prev su precip + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.su.p.cor) # examine residuals

a.su.t.cor <- lme(richness ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.su.t.cor) # examine residuals

a.su.p.t.cor <- lme(richness ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                    data = rich,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(a.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
a.pgs.p.int <- lme(richness ~ trt*year*prev.gs.precip, # base + prev gs precip + int
                   data = rich,
                   random = ~1|plot)
plot(a.pgs.p.int) 

a.pgs.t.int <- lme(richness ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
                   data = rich,
                   random = ~1|plot)
plot(a.pgs.t.int) # examine residuals

a.pgs.p.t.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                     data = rich,
                     random = ~1|plot)
plot(a.pgs.p.t.int) # examine residuals

## previous spring
a.sp.p.int <- lme(richness ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                  data = rich,
                  random = ~1|plot)
plot(a.sp.p.int) 

a.sp.t.int <- lme(richness ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                  data = rich,
                  random = ~1|plot)
plot(a.sp.t.int) # examine residuals

a.sp.p.t.int <- lme(richness ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                    data = rich,
                    random = ~1|plot)
plot(a.sp.p.t.int) # examine residuals

## previous summer
a.su.p.int <- lme(richness ~ trt*year*prev.su.precip, # base + prev su precip + int
                  data = rich,
                  random = ~1|plot)
plot(a.su.p.int) 

a.su.t.int <- lme(richness ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                  data = rich,
                  random = ~1|plot)
plot(a.su.t.int) # examine residuals

a.su.p.t.int <- lme(richness ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                    data = rich,
                    random = ~1|plot)
plot(a.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
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

a.pgs.p.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                         data = rich,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.t.cor.int) # examine residuals

## previous spring
a.sp.p.cor.int <- lme(richness ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                      data = rich,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(a.sp.p.cor.int) # examine residuals

a.sp.t.cor.int <- lme(richness ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                      data = rich,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(a.sp.t.cor.int) # examine residuals

a.sp.p.t.cor.int <- lme(richness ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                        data = rich,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(a.sp.p.t.cor.int) # examine residuals

## previous summer
a.su.p.cor.int <- lme(richness ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                      data = rich,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(a.su.p.cor.int) # examine residuals

a.su.t.cor.int <- lme(richness ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                      data = rich,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(a.su.t.cor.int) # examine residuals

a.su.p.t.cor.int <- lme(richness ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                        data = rich,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(a.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(a.null), AIC(a.null.cor), AIC(a.yr), AIC(a.yr.cor), AIC(a.trt), AIC(a.trt.cor),
           AIC(a.base), AIC(a.base.cor), AIC(a.base.int), AIC(a.base.int.cor),
           
           AIC(a.pgs.p), AIC(a.pgs.p.cor), AIC(a.pgs.p.int), AIC(a.pgs.p.cor.int),
           AIC(a.pgs.t), AIC(a.pgs.t.cor), AIC(a.pgs.t.int), AIC(a.pgs.t.cor.int),
           AIC(a.pgs.p.t), AIC(a.pgs.p.t.cor), AIC(a.pgs.p.t.int), AIC(a.pgs.p.t.cor.int),
           
           AIC(a.sp.p), AIC(a.sp.p.cor), AIC(a.sp.p.int), AIC(a.sp.p.cor.int),
           AIC(a.sp.t), AIC(a.sp.t.cor), AIC(a.sp.t.int), AIC(a.sp.t.cor.int),
           AIC(a.sp.p.t), AIC(a.sp.p.t.cor), AIC(a.sp.p.t.int), AIC(a.sp.p.t.cor.int),
           
           AIC(a.su.p), AIC(a.su.p.cor), AIC(a.su.p.int), AIC(a.su.p.cor.int),
           AIC(a.su.t), AIC(a.su.t.cor), AIC(a.su.t.int), AIC(a.su.t.cor.int),
           AIC(a.su.p.t), AIC(a.su.p.t.cor), AIC(a.su.p.t.int), AIC(a.su.p.t.cor.int))
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
rich.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                          stringsAsFactors = FALSE)


# summary
summary(a.pgs.p.t.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

coef(a.pgs.p.t.cor)

# model estimated means for each treatment
emmeans(a.pgs.p.t.cor, c("trt"))

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(a.pgs.p.t.cor, type = 3) #marginal, not sequential

## Plot model fit onto year v. species richness plot ----
### treatments
visreg(a.pgs.p.t.cor, xvar = "trt",
       points = list(cex = 1),
       line = list(col = c("black")),
       main = "Overall Species Richness",
       xlab = "Treatment",
       ylab = "Species Richness",
       overlay = TRUE)

### x = year, y = spp rich
plot(visreg(a.pgs.p.t.cor, xvar = "year", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 11, 10),
     legend = FALSE,
       main = "Overall Species Richness",
       xlab = "Year",
       ylab = "Species Richness",
       overlay = TRUE)

### x = temperature, y = spp rich
plot(visreg(a.pgs.p.t.cor, xvar = "prev.gs.mean.temp", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "Overall Species Richness",
       xlab = "Mean Temperature (°C) in Previous Growing Season",
       ylab = "Species Richness",
       overlay = TRUE)

# 3. Model annual species richness as a function of year, treatment, and climate ----
## Number of annual plants per plot per year
rich.annual <- plants %>%
  left_join(traits) %>%
  group_by(plot, treatment, year, duration)%>%
  dplyr::summarize(richness = n())%>%
  filter(duration == "annual") %>%
  left_join(weather.prev.gs) %>% # add weather data
  left_join(weather.prev.sp)%>%
  left_join(weather.prev.su)

names(rich.annual)[names(rich.annual) == "treatment"] <- "trt"
rich.annual$year <- as.integer(as.factor(rich.annual$year)) # change year to an integer for modeling

## linear mixed effects models ----
# null models
an.null <- lme(richness~1, 
               data = rich.annual, 
               random = ~1|plot)

an.null.cor <- lme(richness ~ 1,
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))

an.yr <- lme(richness~year,
             data = rich.annual,
             random = ~1|plot)

an.yr.cor <- lme(richness ~ year,
                 data = rich.annual,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

an.trt <- lme(richness~trt,
              data = rich.annual,
              random = ~1|plot)

an.trt.cor <- lme(richness ~ trt,
                  data = rich.annual,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

# base models
an.base <- lme(richness ~ trt+year, # base
               data = rich.annual,
               random = ~1|plot)
plot(an.base)

an.base.int <- lme(richness ~ trt*year, # base + int
                   data = rich.annual,
                   random = ~1|plot)
plot(an.base.int)

an.base.cor <- lme(richness ~ trt+year, # base + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.base.cor)

an.base.int.cor <- lme(richness ~ trt*year, # base + int + cor
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.base.int.cor)

# weather models 
## previous growing season
an.pgs.p <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip
                data = rich.annual,
                random = ~1|plot)
plot(an.pgs.p) 

an.pgs.t <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
                data = rich.annual,
                random = ~1|plot)
plot(an.pgs.t) # examine residuals,

an.pgs.p.t <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                  data = rich.annual,
                  random = ~1|plot)
plot(an.pgs.p.t) # examine residuals

## previous spring
an.sp.p <- lme(richness ~ trt+year+prev.sp.precip, # base + prev sp precip
               data = rich.annual,
               random = ~1|plot)
plot(an.sp.p) 

an.sp.t <- lme(richness ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
               data = rich.annual,
               random = ~1|plot)
plot(an.sp.t) # examine residuals,

an.sp.p.t <- lme(richness ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                 data = rich.annual,
                 random = ~1|plot)
plot(an.sp.p.t) # examine residuals

## previous summer
an.su.p <- lme(richness ~ trt+year+prev.su.precip, # base + prev su precip
               data = rich.annual,
               random = ~1|plot)
plot(an.su.p) 

an.su.t <- lme(richness ~ trt+year+prev.su.mean.temp, # base + prev su temp
               data = rich.annual,
               random = ~1|plot)
plot(an.su.t) # examine residuals,

an.su.p.t <- lme(richness ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                 data = rich.annual,
                 random = ~1|plot)
plot(an.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
an.pgs.p.cor <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                    data = rich.annual,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(an.pgs.p.cor) # examine residuals

an.pgs.t.cor <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                    data = rich.annual,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(an.pgs.t.cor) # examine residuals

an.pgs.p.t.cor <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                      data = rich.annual,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(an.pgs.p.t.cor) # examine residuals

## previous spring
an.sp.p.cor <- lme(richness ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.sp.p.cor) # examine residuals

an.sp.t.cor <- lme(richness ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.sp.t.cor) # examine residuals

an.sp.p.t.cor <- lme(richness ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                     data = rich.annual,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(an.sp.p.t.cor) # examine residuals

## previous summer
an.su.p.cor <- lme(richness ~ trt+year+prev.su.precip, # base + prev su precip + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.su.p.cor) # examine residuals

an.su.t.cor <- lme(richness ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.su.t.cor) # examine residuals

an.su.p.t.cor <- lme(richness ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                     data = rich.annual,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(an.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
an.pgs.p.int <- lme(richness ~ trt*year*prev.gs.precip, # base + prev gs precip + int
                    data = rich.annual,
                    random = ~1|plot)
plot(an.pgs.p.int) 

an.pgs.t.int <- lme(richness ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
                    data = rich.annual,
                    random = ~1|plot)
plot(an.pgs.t.int) # examine residuals

an.pgs.p.t.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                      data = rich.annual,
                      random = ~1|plot)
plot(an.pgs.p.t.int) # examine residuals

## previous spring
an.sp.p.int <- lme(richness ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                   data = rich.annual,
                   random = ~1|plot)
plot(an.sp.p.int) 

an.sp.t.int <- lme(richness ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                   data = rich.annual,
                   random = ~1|plot)
plot(an.sp.t.int) # examine residuals

an.sp.p.t.int <- lme(richness ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                     data = rich.annual,
                     random = ~1|plot)
plot(an.sp.p.t.int) # examine residuals

## previous summer
an.su.p.int <- lme(richness ~ trt*year*prev.su.precip, # base + prev su precip + int
                   data = rich.annual,
                   random = ~1|plot)
plot(an.su.p.int) 

an.su.t.int <- lme(richness ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                   data = rich.annual,
                   random = ~1|plot)
plot(an.su.t.int) # examine residuals

an.su.p.t.int <- lme(richness ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                     data = rich.annual,
                     random = ~1|plot)
plot(an.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
an.pgs.p.cor.int <- lme(richness ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                        data = rich.annual,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(an.pgs.p.cor.int) # examine residuals

an.pgs.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                        data = rich.annual,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(an.pgs.t.cor.int) # examine residuals

an.pgs.p.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                          data = rich.annual,
                          random = ~1|plot,
                          correlation = corAR1(form = ~year|plot))
plot(an.pgs.p.t.cor.int) # examine residuals

## previous spring
an.sp.p.cor.int <- lme(richness ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.sp.p.cor.int) # examine residuals

an.sp.t.cor.int <- lme(richness ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.sp.t.cor.int) # examine residuals

an.sp.p.t.cor.int <- lme(richness ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                         data = rich.annual,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(an.sp.p.t.cor.int) # examine residuals

## previous summer
an.su.p.cor.int <- lme(richness ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.su.p.cor.int) # examine residuals

an.su.t.cor.int <- lme(richness ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.su.t.cor.int) # examine residuals

an.su.p.t.cor.int <- lme(richness ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                         data = rich.annual,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(an.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(an.null), AIC(an.null.cor), AIC(an.yr), AIC(an.yr.cor), AIC(an.trt), AIC(an.trt.cor),
           AIC(an.base), AIC(an.base.cor), AIC(an.base.int), AIC(an.base.int.cor),
           
           AIC(an.pgs.p), AIC(an.pgs.p.cor), AIC(an.pgs.p.int), AIC(an.pgs.p.cor.int),
           AIC(an.pgs.t), AIC(an.pgs.t.cor), AIC(an.pgs.t.int), AIC(an.pgs.t.cor.int),
           AIC(an.pgs.p.t), AIC(an.pgs.p.t.cor), AIC(an.pgs.p.t.int), AIC(an.pgs.p.t.cor.int),
           
           AIC(an.sp.p), AIC(an.sp.p.cor), AIC(an.sp.p.int), AIC(an.sp.p.cor.int),
           AIC(an.sp.t), AIC(an.sp.t.cor), AIC(an.sp.t.int), AIC(an.sp.t.cor.int),
           AIC(an.sp.p.t), AIC(an.sp.p.t.cor), AIC(an.sp.p.t.int), AIC(an.sp.p.t.cor.int),
           
           AIC(an.su.p), AIC(an.su.p.cor), AIC(an.su.p.int), AIC(an.su.p.cor.int),
           AIC(an.su.t), AIC(an.su.t.cor), AIC(an.su.t.int), AIC(an.su.t.cor.int),
           AIC(an.su.p.t), AIC(an.su.p.t.cor), AIC(an.su.p.t.int), AIC(an.su.p.t.cor.int))
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
rich.an.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                           stringsAsFactors = FALSE)

# summary
summary(an.pgs.t.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(an.pgs.t.cor, type = 3) #marginal, not sequential


## Plot model fit ----
### treatment
visreg(an.pgs.t.cor, xvar = "trt",
       points = list(cex = 1),
       line = list(col = c("black")),
       main = "Species Richness (Annuals)",
       xlab = "Treatment",
       ylab = "Species Richness (annuals)",
       overlay = TRUE)

### x = year, y = annual spp. rich
plot(visreg(an.pgs.t.cor, xvar = "year", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 11, 10),
     legend = FALSE,
     main = "Species Richness (Annuals)",
     xlab = "Year",
     ylab = "Species Richness (annuals)",
     overlay = TRUE)

### x = temp, y = annual spp rich
plot(visreg(an.pgs.t.cor, xvar = "prev.gs.mean.temp", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "Species Richness (Annuals)",
       xlab = "Mean Temperature (°C) in Previous Growing Season",
       ylab = "Species Richness (annuals)",
       overlay = TRUE)

### x = precip, y = annual spp rich, by trt
plot(visreg(an.pgs.t.cor, xvar = "prev.gs.precip", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "Species Richness (Annuals)",
     xlab = "Total Precipitation (mm) in Previous Growing Season",
     ylab = "Species Richness (annuals)",
     overlay = TRUE)


# 4. Model perennial species richness as a function of year, treatment, and climate ----
## Number of perennial plants per plot per year
rich.perennial <- plants %>%
  left_join(traits) %>%
  group_by(plot, treatment, year, duration)%>%
  dplyr::summarize(richness = n())%>%
  filter(duration == "perennial") %>%
  left_join(weather.prev.gs) %>% # add weather data
  left_join(weather.prev.sp) %>%
  left_join(weather.prev.su)
  

names(rich.perennial)[names(rich.perennial) == "treatment"] <- "trt"
rich.perennial$year <- as.integer(as.factor(rich.perennial$year)) # change year to an integer for modeling

## linear mixed effects models ----
# null models
pe.null <- lme(richness~1, 
               data = rich.perennial, 
               random = ~1|plot)

pe.null.cor <- lme(richness ~ 1,
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))

pe.yr <- lme(richness~year,
             data = rich.perennial,
             random = ~1|plot)

pe.yr.cor <- lme(richness ~ year,
                 data = rich.perennial,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

pe.trt <- lme(richness~trt,
              data = rich.perennial,
              random = ~1|plot)

pe.trt.cor <- lme(richness ~ trt,
                  data = rich.perennial,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

# base models
pe.base <- lme(richness ~ trt+year, # base
               data = rich.perennial,
               random = ~1|plot)
plot(pe.base)

pe.base.int <- lme(richness ~ trt*year, # base + int
                   data = rich.perennial,
                   random = ~1|plot)
plot(pe.base.int)

pe.base.cor <- lme(richness ~ trt+year, # base + cor
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.base.cor)

pe.base.int.cor <- lme(richness ~ trt*year, # base + int + cor
                       data = rich.perennial,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(pe.base.int.cor)

# weather models 
## previous growing season
pe.pgs.p <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip
                data = rich.perennial,
                random = ~1|plot)
plot(pe.pgs.p) 

pe.pgs.t <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
                data = rich.perennial,
                random = ~1|plot)
plot(pe.pgs.t) # examine residuals,

pe.pgs.p.t <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                  data = rich.perennial,
                  random = ~1|plot)
plot(pe.pgs.p.t) # examine residuals

## previous spring
pe.sp.p <- lme(richness ~ trt+year+prev.sp.precip, # base + prev sp precip
               data = rich.perennial,
               random = ~1|plot)
plot(pe.sp.p) 

pe.sp.t <- lme(richness ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
               data = rich.perennial,
               random = ~1|plot)
plot(pe.sp.t) # examine residuals,

pe.sp.p.t <- lme(richness ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                 data = rich.perennial,
                 random = ~1|plot)
plot(pe.sp.p.t) # examine residuals

## previous summer
pe.su.p <- lme(richness ~ trt+year+prev.su.precip, # base + prev su precip
               data = rich.perennial,
               random = ~1|plot)
plot(pe.su.p) 

pe.su.t <- lme(richness ~ trt+year+prev.su.mean.temp, # base + prev su temp
               data = rich.perennial,
               random = ~1|plot)
plot(pe.su.t) # examine residuals,

pe.su.p.t <- lme(richness ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                 data = rich.perennial,
                 random = ~1|plot)
plot(pe.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
pe.pgs.p.cor <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                    data = rich.perennial,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(pe.pgs.p.cor) # examine residuals

pe.pgs.t.cor <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                    data = rich.perennial,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(pe.pgs.t.cor) # examine residuals

pe.pgs.p.t.cor <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                      data = rich.perennial,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(pe.pgs.p.t.cor) # examine residuals

## previous spring
pe.sp.p.cor <- lme(richness ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.sp.p.cor) # examine residuals

pe.sp.t.cor <- lme(richness ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.sp.t.cor) # examine residuals

pe.sp.p.t.cor <- lme(richness ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                     data = rich.perennial,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(pe.sp.p.t.cor) # examine residuals

## previous summer
pe.su.p.cor <- lme(richness ~ trt+year+prev.su.precip, # base + prev su precip + cor
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.su.p.cor) # examine residuals

pe.su.t.cor <- lme(richness ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.su.t.cor) # examine residuals

pe.su.p.t.cor <- lme(richness ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                     data = rich.perennial,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(pe.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
pe.pgs.p.int <- lme(richness ~ trt*year*prev.gs.precip, # base + prev gs precip + int
                    data = rich.perennial,
                    random = ~1|plot)
plot(pe.pgs.p.int) 

pe.pgs.t.int <- lme(richness ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
                    data = rich.perennial,
                    random = ~1|plot)
plot(pe.pgs.t.int) # examine residuals

pe.pgs.p.t.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                      data = rich.perennial,
                      random = ~1|plot)
plot(pe.pgs.p.t.int) # examine residuals

## previous spring
pe.sp.p.int <- lme(richness ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                   data = rich.perennial,
                   random = ~1|plot)
plot(pe.sp.p.int) 

pe.sp.t.int <- lme(richness ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                   data = rich.perennial,
                   random = ~1|plot)
plot(pe.sp.t.int) # examine residuals

pe.sp.p.t.int <- lme(richness ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                     data = rich.perennial,
                     random = ~1|plot)
plot(pe.sp.p.t.int) # examine residuals

## previous summer
pe.su.p.int <- lme(richness ~ trt*year*prev.su.precip, # base + prev su precip + int
                   data = rich.perennial,
                   random = ~1|plot)
plot(pe.su.p.int) 

pe.su.t.int <- lme(richness ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                   data = rich.perennial,
                   random = ~1|plot)
plot(pe.su.t.int) # examine residuals

pe.su.p.t.int <- lme(richness ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                     data = rich.perennial,
                     random = ~1|plot)
plot(pe.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
pe.pgs.p.cor.int <- lme(richness ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                        data = rich.perennial,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(pe.pgs.p.cor.int) # examine residuals

pe.pgs.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                        data = rich.perennial,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(pe.pgs.t.cor.int) # examine residuals

pe.pgs.p.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                          data = rich.perennial,
                          random = ~1|plot,
                          correlation = corAR1(form = ~year|plot))
plot(pe.pgs.p.t.cor.int) # examine residuals

## previous spring
pe.sp.p.cor.int <- lme(richness ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                       data = rich.perennial,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(pe.sp.p.cor.int) # examine residuals

pe.sp.t.cor.int <- lme(richness ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                       data = rich.perennial,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(pe.sp.t.cor.int) # examine residuals

pe.sp.p.t.cor.int <- lme(richness ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                         data = rich.perennial,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(pe.sp.p.t.cor.int) # examine residuals

## previous summer
pe.su.p.cor.int <- lme(richness ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                       data = rich.perennial,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(pe.su.p.cor.int) # examine residuals

pe.su.t.cor.int <- lme(richness ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                       data = rich.perennial,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(pe.su.t.cor.int) # examine residuals

pe.su.p.t.cor.int <- lme(richness ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                         data = rich.perennial,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(pe.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(pe.null), AIC(pe.null.cor), AIC(pe.yr), AIC(pe.yr.cor), AIC(pe.trt), AIC(pe.trt.cor),
           AIC(pe.base), AIC(pe.base.cor), AIC(pe.base.int), AIC(pe.base.int.cor),
           
           AIC(pe.pgs.p), AIC(pe.pgs.p.cor), AIC(pe.pgs.p.int), AIC(pe.pgs.p.cor.int),
           AIC(pe.pgs.t), AIC(pe.pgs.t.cor), AIC(pe.pgs.t.int), AIC(pe.pgs.t.cor.int),
           AIC(pe.pgs.p.t), AIC(pe.pgs.p.t.cor), AIC(pe.pgs.p.t.int), AIC(pe.pgs.p.t.cor.int),
           
           AIC(pe.sp.p), AIC(pe.sp.p.cor), AIC(pe.sp.p.int), AIC(pe.sp.p.cor.int),
           AIC(pe.sp.t), AIC(pe.sp.t.cor), AIC(pe.sp.t.int), AIC(pe.sp.t.cor.int),
           AIC(pe.sp.p.t), AIC(pe.sp.p.t.cor), AIC(pe.sp.p.t.int), AIC(pe.sp.p.t.cor.int),
           
           AIC(pe.su.p), AIC(pe.su.p.cor), AIC(pe.su.p.int), AIC(pe.su.p.cor.int),
           AIC(pe.su.t), AIC(pe.su.t.cor), AIC(pe.su.t.int), AIC(pe.su.t.cor.int),
           AIC(pe.su.p.t), AIC(pe.su.p.t.cor), AIC(pe.su.p.t.int), AIC(pe.su.p.t.cor.int))
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

rich.pe.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                          stringsAsFactors = FALSE)

# summary
summary(pe.su.t.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(pe.su.t.cor, type = 3) #marginal, not sequential


## Plot model fit ----
### treatment
visreg(pe.su.t.cor, xvar = "trt",
       points = list(cex = 1),
       line = list(col = c("black")),
       main = "Species Richness (Perennials)",
       xlab = "Treatment",
       ylab = "Species Richness (perennials)",
       overlay = TRUE)

### x = year, y = spp rich, by trt
plot(visreg(pe.su.t.cor, xvar = "year", overlay = TRUE, plot = FALSE),
     xaxp = c(1, 11, 10),
     ylim = range(6, 15),
     legend = FALSE,
     main = "Species Richness (Perennials)",
       xlab = "Year",
       ylab = "Species Richness (perennials)",
       overlay = TRUE)


plot(visreg(pe.su.t.cor, xvar = "prev.su.mean.temp", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "Species Richness (Perennials)",
     xlab = "Mean Temperature (°C) in Previous Summer",
     ylab = "Species Richness (perennials)",
     overlay = TRUE)

## Calculating species richness averages ----
rich.annual <- rich.annual %>%
  dplyr::rename(an.rich = richness)

rich.perennial <- rich.perennial %>%
  dplyr::rename(pe.rich = richness)

rich.per <- rich %>%
  left_join(rich.annual)%>%
  select(-duration)%>%
  left_join(rich.perennial)

rich.per$an.per <- rich.per$an.rich/rich.per$richness
rich.per$pe.per <- rich.per$pe.rich/rich.per$richness
rich.per$tot <- rich.per$an.per + rich.per$pe.per 

mean(rich.per$an.per)
mean(rich.per$pe.per)
