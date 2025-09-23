# Script for data exploration
# Robin Bradley
# robinbradley210@gmail.com
# created: 11 July 2025
# last updated: 15 July 2025

# Set up ----
## set WD
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

## packages
library(tidyverse)

## Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/cowichan_cleanup.R")

# 1. Species Richness ----
## Calculate total number of species in each year 
nrow(data.frame(unique(plants$species[plants$year == 2015 & plants$cover > 0]))) # 39
nrow(data.frame(unique(plants$species[plants$year == 2016 & plants$cover > 0]))) # 39
nrow(data.frame(unique(plants$species[plants$year == 2017 & plants$cover > 0]))) # 42
nrow(data.frame(unique(plants$species[plants$year == 2018 & plants$cover > 0]))) # 37
nrow(data.frame(unique(plants$species[plants$year == 2019 & plants$cover > 0]))) # 42
nrow(data.frame(unique(plants$species[plants$year == 2020 & plants$cover > 0]))) # 44
nrow(data.frame(unique(plants$species[plants$year == 2021 & plants$cover > 0]))) # 47
nrow(data.frame(unique(plants$species[plants$year == 2022 & plants$cover > 0]))) # 46
nrow(data.frame(unique(plants$species[plants$year == 2023 & plants$cover > 0]))) # 42
nrow(data.frame(unique(plants$species[plants$year == 2024 & plants$cover > 0]))) # 44
nrow(data.frame(unique(plants$species[plants$year == 2025 & plants$cover > 0]))) # 42

# plot species accumulation curve across samples
plot(specaccum(spe.matrix, method="random"), xlab = "# of samples", ylab = "# of plant species")

# seems like we've hit saturation - that's reassuring

# Plot % cover per plot-year as a histogram using rowSums
ggplot(data.frame(abundance=rowSums(spe.matrix)), aes(abundance))+
  geom_histogram(bins = 20)

# Plot total species per plot-year as a histogram; add a logical argument to rowSums
ggplot(data.frame(species_number=rowSums(spe.matrix>0)), aes(species_number))+
  geom_histogram(bins = 10)

# What is the relationship between the two (% cover and number of species)?
ggplot(data.frame(species_number=rowSums(spe.matrix>0), abundance=rowSums(spe.matrix)), 
       aes(abundance, species_number))+
  geom_point()+
  geom_smooth(method="lm",formula=y~log(x))

# compare species richness across treatments
boxplot(specnumber(spe.matrix) ~ plot_year$trt, ylab = "# of plant species", xlab = " ")

# compare species richness across years
boxplot(specnumber(spe.matrix) ~ plot_year$year, ylab = "# of plant species", xlab = " ")

# compare species richness across plots
boxplot(specnumber(spe.matrix) ~ plot_year$plot, ylab = "# of plant species", xlab = " ")

# compare species richness across treatments and years 
ggplot(spe.matrix,aes(x=plot_year$year, y=specnumber(spe.matrix), color = plot_year$trt))+
  geom_jitter()+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  ylab("# of plant species")+
  xlab("Year")+
  facet_wrap(~plot_year$trt)+
  theme_classic()

# 2. Plotting cover time series ----
## Plotting by species, year, and treatment - ALL species, plots, treatments
ggplot(plants, aes(x = year, y = cover, color = plot))+
  geom_jitter()+
  xlab("Year")+
  ylab("Cover (%)")+
  facet_wrap(~species)+
  theme_test()

## Cover of snowberry over time ----

### Pulling snowberry cover out
snowberry_cover <- filter(plants, species == "Symphoricarpos albus")

### Taking average/SEM of snowberry cover
SB_cover_means <- snowberry_cover %>%
  dplyr::group_by(year)%>%
  dplyr::summarise(n = n(), mean_cover = mean(cover), se_cover = sd(cover)/sqrt(n))

### Plotting cover of snowberry over time

#### Cover (points)
SB_timeseries <- ggplot(snowberry_cover,
                        aes(x = year,
                            y = cover))+
  geom_point()+
  xlab("Year")+
  ylab("Snowberry Cover (%)")+
  theme_test()
SB_timeseries

#### Means of each year
SB_timeseries_means <- ggplot(SB_cover_means,
                              aes(x = year,
                                  y = mean_cover))+
  geom_point()+
  xlab("Year")+
  ylab("Snowberry Cover (%)")+
  geom_errorbar(aes(x = year,
                    ymin=mean_cover-se_cover,
                    ymax=mean_cover+se_cover))+
  theme_test()
SB_timeseries_means

#### Plotting cover by plot
ggplot(snowberry_cover#[snowberry_cover$treatment == "control", ]
                              ,
                              aes(x = year,
                                  y = cover,
                                  color = as.factor(plot)))+
  geom_point()+
  xlab("Year")+
  ylab("Snowberry Cover (%)")+
  labs(color = 'Plot Number')+
  geom_line(aes(group = plot))+
  theme_test()


#### Number of plots with snowberry present per year
ggplot(SB_cover_means,
                       aes(x = year,
                           y = n))+
  geom_point()+
  xlab("Year")+
  ylab("Number of Plots with Snowberry Present")+
  theme_test()


## Cover of Bromus sterlis over time ----
ggplot(plants, aes(x = year, y = cover, color = plot))+
  geom_point(data=subset(plants, species =='Bromus sterilis'))+
  xlab("Year")+
  ylab("Bromus sterilis Cover (%)")+
  #geom_line(data=subset(plants, species =='Bromus sterilis'), aes(group = plot))+
  theme_test()

## Cover of Bromes over time ----
Brome_cover <- filter(plants, species == "Bromus tectorum"| species == "Bromus hordeaceus"| 
                        species == "Bromus sterilis"| species == "Bromus carinatus")

ggplot(Brome_cover,
                           aes(x = year,
                               y = cover,
                               color = as.factor(species)))+
  geom_jitter()+
  xlab("Year")+
  ylab("Bromus spp. Cover (%)")+
  labs(color = 'Species')+
  theme_test()


## Cover of Galium aparnine over time ----
ggplot(plants, aes(x = year, y = cover, color = plot))+
  geom_point(data=subset(plants, species =='Galium aparine'))+
  xlab("Year")+
  ylab("Galium aparine Cover (%)")+
  geom_line(data=subset(plants, species =='Galium aparine'), aes(group = plot))+
  theme_test()

## Cover of Lonicera hispidula over time ----
ggplot(plants, aes(x = year, y = cover, color = plot))+
  geom_point(data=subset(plants, species =='Lonicera hispidula'))+
  xlab("Year")+
  ylab("Lonicera hispidula Cover (%)")+
  geom_line(data=subset(plants, species =='Lonicera hispidula'), aes(group = plot))+
  theme_test()

## Cover of Primula hendersonii over time ----
ggplot(plants, aes(x = year, y = cover, color = plot))+
  geom_point(data=subset(plants, species =='Primula hendersonii'))+
  xlab("Year")+
  ylab("Primula hendersonii Cover (%)")+
  #geom_line(data=subset(plants, species =='Primula hendersonii'), aes(group = plot))+
  theme_test()

## Cover of Plectritis congesta over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_point(data=subset(plants, species =='Plectritis congesta'))+
  xlab("Year")+
  ylab("Plectritis congesta Cover (%)")+
  #geom_line(data=subset(plants, species =='Plectritis congesta'), aes(group = plot))+
  theme_test()

## Cover of Anthoxanthum odoratum over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_point(data=subset(plants, species =='Anthoxanthum odoratum'))+
  xlab("Year")+
  ylab("Anthoxanthum odoratum Cover (%)")+
  geom_line(data=subset(plants, species =='Anthoxanthum odoratum'), aes(group = plot))+
  theme_test()

## Cover of Melica subulata over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_point(data=subset(plants, species =='Melica subulata'))+
  xlab("Year")+
  ylab("Melica subulata Cover (%)")+
  geom_line(data=subset(plants, species =='Melica subulata'), aes(group = plot))+
  theme_test()

## Cover of Ranunculus occidentalis over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
))+
  geom_point(data=subset(plants, species =='Ranunculus occidentalis'))+
  xlab("Year")+
  ylab("Ranunculus occidentalis Cover (%)")+
  #geom_line(data=subset(plants, species =='Ranunculus occidentalis'), aes(group = plot))+
  theme_test()

## Cover of Valerianella locusta over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_point(data=subset(plants, species =='Valerianella locusta'))+
  xlab("Year")+
  ylab("Valerianella locusta Cover (%)")+
  geom_line(data=subset(plants, species =='Valerianella locusta'), aes(group = plot))+
  theme_test()

## Cover of Lathyrus sphaericus over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_point(data=subset(plants, species =='Lathyrus sphaericus'))+
  xlab("Year")+
  ylab("Lathyrus sphaericus Cover (%)")+
  geom_line(data=subset(plants, species =='Lathyrus sphaericus'), aes(group = plot))+
  theme_test()

## Cover of Sanicula crassicaulis over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_point(data=subset(plants, species =='Sanicula crassicaulis'))+
  xlab("Year")+
  ylab("Sanicula crassicaulis Cover (%)")+
  geom_line(data=subset(plants, species =='Sanicula crassicaulis'), aes(group = plot))+
  theme_test()

## Cover of Luzula over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   color = plot
                   ))+
  geom_jitter(data=subset(plants, species =='Luzula sp.'))+
  xlab("Year")+
  ylab("Luzula (%)")+
  #geom_line(data=subset(plants, species =='Luzula sp.'), aes(group = plot))+
  theme_test()

## Cover of Claytonia perfoliata over time ----
ggplot(plants, aes(x = year, 
                   y = cover, 
                   #color = plot
))+
  geom_boxplot(data=subset(plants, species =='Claytonia perfoliata'))+
  geom_jitter(data=subset(plants, species =='Claytonia perfoliata'), color = "blue", alpha = 0.5)+
  xlab("Year")+
  ylab("Claytonia perfoliata cover (%)")+
  #geom_line(data=subset(plants, species =='Luzula sp.'), aes(group = plot))+
  theme_test()


## Cover of spp selected for Jenn over time ----
# wants to show variability in plants - choose some variable ones and some that are more consistent
Jenn_sel <- filter(plants, species == "Anthoxanthum odoratum"| 
                     species == "Melica subulata"| 
                     species == "Ranunculus occidentalis"| 
                     species == "Bromus carinatus"| 
                     species == "Plectritis congesta"| 
                     species == "Claytonia perfoliata")

ggplot(Jenn_sel,aes(x = year,
                    y = cover,
                    color = plot
                    ))+
  geom_point()+
  xlab("Year")+
  ylab("Cover (%)")+
  #scale_color_manual(values=c("black", "lightcoral", "darkcyan")) +
  labs(color = 'Plot')+
  geom_line(aes(group = plot))+
  facet_wrap(~species)+
  theme_test()
