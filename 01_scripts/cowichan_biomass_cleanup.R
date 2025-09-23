# Cowichan Plant Community Cleaning script
# Adapted from code by Lauren Smith (plants_cleanup.Rmd)
# email: robinbradley210@gmail.com
# created: 11 July 2025
# last updated: 11 July 2025
## this might be broken - will fix if I end up using biomass


# Set Up ----
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")
library(tidyverse)

# Biomass Data ----
biomass <- read.csv("Biomass/cowichan_biomass_master_2024.csv", header = TRUE)

# read in the biomass data, get rid of unnecessary columns and rename mass column
biomass <- biomass %>% 
  dplyr::select(-treatmentyear, -notes, -MassNew, -MassExtra, -MassRecalculated) #%>% 
#  dplyr::rename("plot" = "ï..plot") 
# dplyr::rename("mass" = "mass..grams.")

# change mass column to numbers not characters; year to factor
biomass$mass <- as.numeric(biomass$mass)
biomass$year <- as.factor(biomass$year)

# convert to g/m^2
biomass$mass <- (biomass$mass)/(.2)

biomass$taxa[biomass$taxa == "Grass"] <- "grass"
biomass$taxa[biomass$taxa == "Bryophyte"] <- "bryo"
biomass$taxa[biomass$taxa == "Bryo"] <- "bryo"
biomass$taxa[biomass$taxa == "Dead"] <- "dead"
biomass$taxa[biomass$taxa == "Shrub"] <- "shrub"
biomass$taxa[biomass$taxa == "Forb"] <- "forb"
biomass$taxa[biomass$taxa == "Tree"] <- "tree"
biomass$taxa[biomass$taxa == "Legume"] <- "legume"

# remove dead and moss
biomass <- biomass %>%
  filter(taxa!="dead" & taxa != "bryo")

#adjusting shrub biomass
biomass$mass_adj <- ifelse(biomass$mass > 5 & 
                             biomass$taxa == "shrub" & 
                             biomass$year != 2020 & 
                             biomass$year != 2021 & 
                             biomass$year != 2019 & 
                             biomass$year != 2017, 
                           biomass$mass/2, biomass$mass)
# calculate total biomass across all growth forms
totbiomass <- biomass %>% 
  group_by(plot, treatment, year) %>% 
  dplyr::summarize(totbio = sum(mass, na.rm = TRUE), 
                   totbio_adj = sum(mass_adj, na.rm = TRUE))

#totbiomassavg <- totbiomass %>%
#group_by(treatment,year)


# widen so that there is one column for each growthform
biomass_wide <- biomass %>% 
  pivot_wider(names_from = taxa,
              names_sep = "_",
              values_from = c(mass, mass_adj))

# replace NAs with 0  
biomass_wide <- biomass_wide %>% replace(is.na(biomass_wide), 0)

#plotting
biomass_timeseries_total <- ggplot(totbiomass,
                                   aes(x = year,
                                       y = totbio_adj,
                                       color = as.factor(treatment)))+
  geom_point()+
  labs(color='Treatment')+
  xlab("Year")+
  ylab("Total Biomass (g/m^2)")+
  theme_test()
biomass_timeseries_total

biomass_boxplot_total <- ggplot(totbiomass,
                                aes(x = treatment,
                                    y = totbio_adj,
                                    color = as.factor(treatment)))+
  geom_boxplot()+
  labs(color='Treatment')+
  xlab("Treatment")+
  ylab("Total Biomass (g/m^2)")+
  theme_test()
biomass_boxplot_total

## 2018 biomass
biomass2018 <- ggplot(biomass[biomass$year == 2018, ],
                      aes(x = taxa,
                          y = mass_adj))+
  geom_point()+
  theme_test ()
biomass2018

## grass biomass
biomassgrass <- ggplot(biomass[biomass$taxa == "grass", ],
                       aes(x = year,
                           y = mass_adj,
                           color = as.factor(treatment)))+
  geom_point()+
  theme_test ()
biomassgrass

#biomass by treatment
biomasstrt <- ggplot(biomass,
                     aes(x = treatment,
                         y = mass_adj,
                         color = as.factor(taxa)))+
  geom_boxplot()+
  theme_test ()
biomasstrt