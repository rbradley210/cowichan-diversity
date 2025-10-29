# Script for modeling alpha diversity
# Robin Bradley
# robinbradley210@gmail.com
# created: 14 July 2025
# last updated: 28 Oct 2025

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
  geom_boxplot()+
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
rich <- left_join(rich, weather.prev.365)
rich <- rich[, -6]

rich$year <- as.integer(as.factor(rich$year))



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

a <- lme(richness ~ trt+year, # base
         data = rich,
         random = ~1|plot)
  
a.base <- lme(richness ~ trt*year, # base.int
              data = rich,
              random = ~1|plot)
plot(a.base) # examine residuals, look fine

a.pgs.p <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip
               data = rich,
               random = ~1|plot)
plot(a.pgs.p) # examine residuals

a.pgs.t <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
               data = rich,
               random = ~1|plot)
plot(a.pgs.t) # examine residuals,

a.pgs.p.t <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = rich,
                 random = ~1|plot)
plot(a.pgs.p.t) # examine residuals

a.365.p <- lme(richness ~ trt+year+tot.precip.365, # base + prev 365 precip
               data = rich,
               random = ~1|plot)
plot(a.pgs.p) # examine residuals

a.365.t <- lme(richness ~ trt+year+mean.temp.365, # base + prev 365 temp
               data = rich,
               random = ~1|plot)
plot(a.365.t) # examine residuals,

a.365.p.t <- lme(richness ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                 data = rich,
                 random = ~1|plot)
plot(a.365.p.t) # examine residuals


# Model with AR(1) temporal autocorrelation structure

a.base.cor <- lme(richness ~ trt+year,  # base + cor
                  data = rich,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(a.base.cor) # examine residuals

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

a.365.p.cor <- lme(richness ~ trt+year+tot.precip.365, # base + prev 365 precip + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.pgs.p.cor) # examine residuals

a.365.t.cor <- lme(richness ~ trt+year+mean.temp.365, # base + prev 365 temp + cor
                   data = rich,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(a.365.t.cor) # examine residuals

a.365.p.t.cor <- lme(richness ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                     data = rich,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(a.365.p.t.cor) # examine residuals


## Include interaction terms
a.base.cor.int <- lme(richness ~ trt*year, # base + int
                      data = rich,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))

plot(a.base.cor.int) # examine residuals

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
myAIC <- c(AIC(a), AIC(a.base), AIC(a.base.cor), 
           AIC(a.pgs.p), AIC(a.pgs.t), AIC(a.pgs.p.t), 
           AIC(a.pgs.p.cor), AIC(a.pgs.t.cor), AIC(a.pgs.p.t.cor),
           AIC(a.365.p), AIC(a.365.t), AIC(a.365.p.t), 
           AIC(a.365.p.cor), AIC(a.365.t.cor), AIC(a.365.p.t.cor),
           AIC(a.base.cor.int), AIC(a.pgs.t.cor.int), AIC(a.pgs.p.cor.int), AIC(a.pgs.p.t.cor.int), 
           AIC(a.365.p.cor.int), AIC(a.365.t.cor.int), AIC(a.365.p.t.cor.int)
)
delta <- myAIC - min(myAIC)
model <- c("base", "base.int", "base.cor", 
           "pgs.p", "pgs.t", "pgs.p.t", 
           "pgs.p.cor", "pgs.t.cor", "pgs.p.t.cor",
           "365.p", "365.t", "365.p.t", 
           "365.p.cor", "365.t.cor", "365.p.t.cor",
           "base.cor.int", "pgs.t.cor.int", "pgs.p.cor.int", "pgs.p.t.cor.int", 
           "365.p.cor.int", "365.t.cor.int", "365.p.t.cor.int"
           )
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
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

### x = year, y = spp rich, split by trt
plot(visreg(a.pgs.p.t.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
       points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
       line = list(col = c("black", "darkorange", "deepskyblue2")),
       main = "Overall Species Richness",
       xlab = "Year",
       ylab = "Species Richness",
       overlay = TRUE)
legend(x = "bottomright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)

### x = temperature, y = spp rich, split by trt
plot(visreg(a.pgs.p.t.cor, xvar = "prev.gs.mean.temp", by = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
       points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.75),
       line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Overall Species Richness",
       xlab = "Previous Growing Season Mean Temperature",
       ylab = "Species Richness",
       overlay = TRUE)
legend(x = "topleft", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)

## x = precip, y = spp rich, split by trt
plot(visreg(a.pgs.p.t.cor, xvar = "prev.gs.precip", by = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.75),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Overall Species Richness",
     xlab = "Previous Growing Season Total Precipitation (mm)",
     ylab = "Species Richness",
     overlay = TRUE)
legend(x = "bottomright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)



# v <- visreg(a.365.t.cor.int,  xvar = "mean.temp.365",
#             gg = TRUE) 
# 
# visreg(a.365.t.cor.int,  xvar = "year", by = "trt",
#        overlay = TRUE,
#        gg = TRUE) +
#   geom_point(aes(color = rich$trt))+
#   labs(x = "Year", y = "Species Richness/Plot", color = "Treatment")+
#   scale_x_continuous(breaks = seq(1, 11, by = 1))+
#   theme_classic()
# 
# visreg(a.365.t.cor.int,  xvar = "mean.temp.365", by = "year",
#             gg = TRUE) +
#   geom_point(aes(color = rich$trt))+
#   theme_classic()
# 
# visreg(a.365.t.cor.int,  xvar = "year", by = "mean.temp.365", 
#        overlay = TRUE,
#        legend = FALSE,
#        gg = TRUE) +
#   labs(x = "Year", y = "Species Richness/Plot", color = "Temp")+
#   scale_x_continuous(breaks = seq(1, 11, by = 1))+
#   theme_classic()
# 
# visreg2d(a.365.t.cor.int, "year", "mean.temp.365")
# 
# visreg(a.365.t.cor.int,  xvar = "mean.temp.365", by = "year", 
#        overlay = TRUE,
#        breaks=3,
#        legend = FALSE,
#        gg = TRUE) +
#   labs(x = "temp", y = "Species Richness/Plot", color = "year")+
#   #scale_x_continuous(breaks = seq(1, 11, by = 1))+
#   theme_classic()


# 3. Model annual species richness as a function of year, treatment, and climate ----
## Number of annual plants per plot per year
rich.annual <- plants %>%
  left_join(traits) %>%
  group_by(plot, treatment, year, duration)%>%
  dplyr::summarize(richness = n())%>%
  filter(duration == "annual") %>%
  left_join(weather.prev.gs) %>% # add weather data
  left_join(weather.prev.365)

names(rich.annual)[names(rich.annual) == "treatment"] <- "trt"
rich.annual$year <- as.integer(as.factor(rich.annual$year)) # change year to an integer for modeling

## linear mixed effects models ----
an <- lme(richness ~ trt+year, # base
         data = rich.annual,
         random = ~1|plot)
plot(an)

an.base <- lme(richness ~ trt*year, # base.int
              data = rich.annual,
              random = ~1|plot)
plot(an.base) # examine residuals, look fine

an.pgs.p <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip
               data = rich.annual,
               random = ~1|plot)
plot(an.pgs.p) # examine residuals

an.pgs.t <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
               data = rich.annual,
               random = ~1|plot)
plot(an.pgs.t) # examine residuals,

an.pgs.p.t <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = rich.annual,
                 random = ~1|plot)
plot(an.pgs.p.t) # examine residuals

an.365.p <- lme(richness ~ trt+year+tot.precip.365, # base + prev 365 precip
               data = rich.annual,
               random = ~1|plot)
plot(an.pgs.p) # examine residuals

an.365.t <- lme(richness ~ trt+year+mean.temp.365, # base + prev 365 temp
               data = rich.annual,
               random = ~1|plot)
plot(an.365.t) # examine residuals,

an.365.p.t <- lme(richness ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                 data = rich.annual,
                 random = ~1|plot)
plot(an.365.p.t) # examine residuals


# Model with AR(1) temporal autocorrelation structure
an.base.cor <- lme(richness ~ trt+year,  # base + cor
                  data = rich.annual,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(an.base.cor) # examine residuals

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

an.365.p.cor <- lme(richness ~ trt+year+tot.precip.365, # base + prev 365 precip + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.365.p.cor) # examine residuals

an.365.t.cor <- lme(richness ~ trt+year+mean.temp.365, # base + prev 365 temp + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.365.t.cor) # examine residuals

an.365.p.t.cor <- lme(richness ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                     data = rich.annual,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(an.365.p.t.cor) # examine residuals


## Include interaction terms
an.base.cor.int <- lme(richness ~ trt*year,  # base + cor
                   data = rich.annual,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(an.base.cor.int) # examine residuals

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

an.pgs.p.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor
                         data = rich.annual,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(an.pgs.p.t.cor.int) # examine residuals


an.365.p.cor.int <- lme(richness ~ trt*year*tot.precip.365, # base + prev 365 precip + cor
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.365.p.cor.int) # examine residuals


an.365.t.cor.int <- lme(richness ~ trt*year*mean.temp.365, # base + prev 365 temp + cor + int
                       data = rich.annual,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(an.365.t.cor.int) # examine residuals

an.365.p.t.cor.int <- lme(richness ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + cor
                         data = rich.annual,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(an.365.p.t.cor.int) # examine residuals

## Model selection ----
myAIC <- c(AIC(an), AIC(an.base), AIC(an.base.cor), 
           AIC(an.pgs.p), AIC(an.pgs.t), AIC(an.pgs.p.t), 
           AIC(an.pgs.p.cor), AIC(an.pgs.t.cor), AIC(an.pgs.p.t.cor),
           AIC(an.365.p), AIC(an.365.t), AIC(an.365.p.t), 
           AIC(an.365.p.cor), AIC(an.365.t.cor), AIC(an.365.p.t.cor),
           AIC(an.base.cor.int), AIC(an.pgs.t.cor.int), AIC(an.pgs.p.cor.int), AIC(an.pgs.p.t.cor.int), 
           AIC(an.365.p.cor.int), AIC(an.365.t.cor.int), AIC(an.365.p.t.cor.int)
)
delta <- myAIC - min(myAIC)
model <- c("base", "base.int", "base.cor", 
           "pgs.p", "pgs.t", "pgs.p.t", 
           "pgs.p.cor", "pgs.t.cor", "pgs.p.t.cor",
           "365.p", "365.t", "365.p.t", 
           "365.p.cor", "365.t.cor", "365.p.t.cor",
           "base.cor.int","pgs.t.cor.int", "pgs.p.cor.int", "pgs.p.t.cor.int", 
           "365.p.cor.int", "365.t.cor.int", "365.p.t.cor.int"
)
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
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

### x = year, y = annual spp. rich, by trt
plot(visreg(an.pgs.t.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.75),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Species Richness (Annuals)",
     xlab = "Year",
     ylab = "Species Richness (annuals)",
     overlay = TRUE)
legend(x = "bottomright", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)

### x = temp, y = annual spp rich, by trt
plot(visreg(an.pgs.t.cor, xvar = "prev.gs.mean.temp", by = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
       points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.75),
       line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Species Richness (Annuals)",
       xlab = "Previous Growing Season Mean Temperature",
       ylab = "Species Richness (annuals)",
       overlay = TRUE)
legend(x = "topleft", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)




# 4. Model perennial species richness as a function of year, treatment, and climate ----
## Number of perennial plants per plot per year
rich.perennial <- plants %>%
  left_join(traits) %>%
  group_by(plot, treatment, year, duration)%>%
  dplyr::summarize(richness = n())%>%
  filter(duration == "perennial") %>%
  left_join(weather.prev.gs) %>% # add weather data
  left_join(weather.prev.365)

names(rich.perennial)[names(rich.perennial) == "treatment"] <- "trt"
rich.perennial$year <- as.integer(as.factor(rich.perennial$year)) # change year to an integer for modeling

## linear mixed effects models ----
pe <- lme(richness ~ trt+year, # base
          data = rich.perennial,
          random = ~1|plot)
plot(pe)

pe.base <- lme(richness ~ trt*year, # base
               data = rich.perennial,
               random = ~1|plot)
plot(pe.base) # examine residuals, look fine

pe.pgs.p <- lme(richness ~ trt+year+prev.gs.precip, # base + prev gs precip
                data = rich.perennial,
                random = ~1|plot)
plot(pe.pgs.p) # examine residuals

pe.pgs.t <- lme(richness ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
                data = rich.perennial,
                random = ~1|plot)
plot(pe.pgs.t) # examine residuals,

pe.pgs.p.t <- lme(richness ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                  data = rich.perennial,
                  random = ~1|plot)
plot(pe.pgs.p.t) # examine residuals

pe.365.p <- lme(richness ~ trt+year+tot.precip.365, # base + prev 365 precip
                data = rich.perennial,
                random = ~1|plot)
plot(pe.pgs.p) # examine residuals

pe.365.t <- lme(richness ~ trt+year+mean.temp.365, # base + prev 365 temp
                data = rich.perennial,
                random = ~1|plot)
plot(pe.365.t) # examine residuals,

pe.365.p.t <- lme(richness ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip
                  data = rich.perennial,
                  random = ~1|plot)
plot(pe.365.p.t) # examine residuals


# Model with AR(1) temporal autocorrelation structure

pe.base.cor <- lme(richness ~ trt+year,  # base + cor
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.base.cor) # examine residuals

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

pe.365.p.cor <- lme(richness ~ trt+year+tot.precip.365, # base + prev 365 precip + cor
                    data = rich.perennial,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(pe.pgs.p.cor) # examine residuals

pe.365.t.cor <- lme(richness ~ trt+year+mean.temp.365, # base + prev 365 temp + cor
                    data = rich.perennial,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(pe.365.t.cor) # examine residuals

pe.365.p.t.cor <- lme(richness ~ trt+year+tot.precip.365+mean.temp.365, # base + prev 365 temp + precip + cor
                      data = rich.perennial,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(pe.365.p.t.cor) # examine residuals


## Include interaction terms
pe.base.cor.int <- lme(richness ~ trt*year,  # base + cor +int
                   data = rich.perennial,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(pe.base.cor.int) # examine residuals

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

pe.pgs.p.t.cor.int <- lme(richness ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor
                          data = rich.perennial,
                          random = ~1|plot,
                          correlation = corAR1(form = ~year|plot))
plot(pe.pgs.p.t.cor.int) # examine residuals


pe.365.p.cor.int <- lme(richness ~ trt*year*tot.precip.365, # base + prev 365 precip + cor
                        data = rich.perennial,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(pe.365.p.cor.int) # examine residuals


pe.365.t.cor.int <- lme(richness ~ trt*year*mean.temp.365, # base + prev 365 temp + cor + int
                        data = rich.perennial,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(pe.365.t.cor.int) # examine residuals

pe.365.p.t.cor.int <- lme(richness ~ trt*year*tot.precip.365*mean.temp.365, # base + prev 365 temp + precip + cor
                          data = rich.perennial,
                          random = ~1|plot,
                          correlation = corAR1(form = ~year|plot))
plot(pe.365.p.t.cor.int) # examine residuals

## Model selection ----
myAIC <- c(AIC(an), AIC(pe.base), AIC(pe.base.cor), 
           AIC(pe.pgs.p), AIC(pe.pgs.t), AIC(pe.pgs.p.t), 
           AIC(pe.pgs.p.cor), AIC(pe.pgs.t.cor), AIC(pe.pgs.p.t.cor),
           AIC(pe.365.p), AIC(pe.365.t), AIC(pe.365.p.t), 
           AIC(pe.365.p.cor), AIC(pe.365.t.cor), AIC(pe.365.p.t.cor),
           AIC(pe.base.cor.int), AIC(pe.pgs.t.cor.int), AIC(pe.pgs.p.cor.int), AIC(pe.pgs.p.t.cor.int), 
           AIC(pe.365.p.cor.int), AIC(pe.365.t.cor.int), AIC(pe.365.p.t.cor.int)
)
delta <- myAIC - min(myAIC)
model <- c("base", "base.int", "base.cor", 
           "pgs.p", "pgs.t", "pgs.p.t", 
           "pgs.p.cor", "pgs.t.cor", "pgs.p.t.cor",
           "365.p", "365.t", "365.p.t", 
           "365.p.cor", "365.t.cor", "365.p.t.cor",
           "base.cor.int", "pgs.t.cor.int", "pgs.p.cor.int", "pgs.p.t.cor.int", 
           "365.p.cor.int", "365.t.cor.int", "365.p.t.cor.int"
)
tab1 <- data.frame(model = model, aic = myAIC, delta = delta,
                   stringsAsFactors = FALSE)

# summary
summary(pe.base.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in mean species richness over time or between years
Anova(pe.base.cor, type = 3) #marginal, not sequential


## Plot model fit ----
### treatment
visreg(pe.base.cor, xvar = "trt",
       points = list(cex = 1),
       line = list(col = c("black")),
       main = "Species Richness (Perennials)",
       xlab = "Treatment",
       ylab = "Species Richness (perennials)",
       overlay = TRUE)

### x = year, y = spp rich, by trt
plot(visreg(pe.base.cor, xvar = "year", by = "trt", overlay = TRUE, plot = FALSE),
     legend = FALSE,
       points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.75),
       line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Species Richness (Perennials)",
       xlab = "Year",
       ylab = "Species Richness (perennials)",
       overlay = TRUE)
legend(x = "topleft", 
       legend = c("Control", "Drought", "Irrigated"),
       col = c("black", "coral", "deepskyblue3"),
       lwd = 3,
       pch = 1)
