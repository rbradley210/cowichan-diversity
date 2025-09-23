# Cowichan species dominance - choosing species for trait collection
# Robin Bradley
# Adapted from code by Lauren Smith (plants_cleanup.Rmd)
# email: robinbradley210@gmail.com
# Created: 7 April 2024
# Last edited: 18 April 2024


# Set Up ----
setwd("C:/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info")

library(tidyverse)
library(dplyr)


## Read in data ----
### Read in plot data
plots <- read.csv("IDE_plotinfo.csv", header = TRUE)
plots <- plots %>% 
  dplyr::rename("treatment" = "trt")
plots$plot <- as.character(plots$plot)

### Read in cover data 
# IDE surveys (mid growing season, 2015-2021)
cover_mids20152021 <- read.csv("Diversity/cowichan_community5_19_2021.csv", header = TRUE, na.strings = c("", " ")) %>% 
  dplyr::select(-quadID, -notes, -X, -X.1, -X.2, -X.3, -X.4, -X.5)

# cover mid 2022
cover_mid2022 <- read.csv("Diversity/2022_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:802) %>% 
  dplyr::select(-quadID, -notes)

# cover mid 2023
cover_mid2023 <- read.csv("Diversity/2023_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:808) %>% 
  dplyr::select(-quadID, -notes)

# cover mid 2024
cover_mid2024 <- read.csv("Diversity/2024_cowichan_community.csv", header = TRUE, na.strings = c("", " ")) %>% 
  slice(1:813) %>% 
  dplyr::select(-quadID, -notes)

## Cleaning cover data ----
# bind all diversity surveys together
cover_all <- rbind(cover_mids20152021, 
                   cover_mid2022, 
                   cover_mid2023, 
                   cover_mid2024)

# change cover column to numbers not characters, and plot to factor and year to factor
cover_all$cover <- as.numeric(as.character(cover_all$cover))  
cover_all$plot <- as.factor(as.numeric(cover_all$plot))
cover_all$year <- as.factor(as.numeric(cover_all$year))

# get rid of NAs in the cover column
cover_all <- cover_all %>%
  filter(cover != "NA")

# fix treatments
cover_all$treatment[cover_all$treatment == "Irrigated"] <- "irrigated"
cover_all$treatment[cover_all$treatment == "Control"] <- "control"
cover_all$treatment[cover_all$treatment == "Drought"] <- "drought"

# just plant cover
plants <- cover_all %>%
  filter(species != "Litter") %>%
  filter(species != "Bare Soil") %>%
  filter(species != "Moss") %>%
  filter(species != "total grass")

# fix plant names -----
plants$species[plants$species == "Unkown furry"] <- "Lonicera sp."
plants$species[plants$species == "Unknown fuzzy"] <- "Lonicera sp."
plants$species[plants$species == "Mystery fuzzy"] <- "Lonicera sp."
plants$species[plants$species == "mystery honey suckle"] <- "Lonicera sp."
plants$species[plants$species == "Unknown H"] <- "Lonicera sp."

plants$species[plants$species == 'Unknown mint'] <- "Clinopodium douglasii"
plants$species[plants$species == 'Unknown F'] <- "Clinopodium douglasii"

plants$species[plants$species == 'Wood rush'] <- "Luzula sp."
plants$species[plants$species == 'wood rush'] <- "Luzula sp."
plants$species[plants$species == 'Luzula'] <- "Luzula sp."

plants$species[plants$species == 'Unknown B'] <- "Trifolium dubium"
#plants$species[plants_tr$species == 'Unknown C'] <- "Teesdalia nudicalis"

plants$species[plants$species == "Grass B"] <- "Vulpia sp."
plants$species[plants$species == "Vulpia spp"] <- "Vulpia sp."

plants$species[plants$species == "Grass 9" & plants$plot == "14"] <- "Bromus hordeaceus"
plants$species[plants$species == "Grass 9"] <- "Bromus carinatus"
plants$species[plants$species == "Bromus hordaceous"] <- "Bromus hordeaceus"
#plants$species[plants$species == "Bromus tectorum"] <- "Bromus sterilis"

plants$species[plants$species == "Fuzzy giant thistle"] <- "Cirsium vulgare"
plants$species[plants$species == "fuzzy giant thistle"] <- "Cirsium vulgare"

plants$species[plants$species == "Alium ampoplectens"] <- "Allium amplectens"

plants$species[plants$species == "mystery orchid (Platanthera from twitter?)"] <- "Platanthera sp."
plants$species[plants$species == "Platanthera spp"] <- "Platanthera sp."

plants$species[plants$species == "bur chervil/mystery carrot"] <- "Torilis arvensis"
plants$species[plants$species == "mystery carrot (not bur chervil)"] <- "Torilis arvensis"
plants$species[plants$species == 'Unknown G '] <- "Torilis arvensis"
plants$species[plants$species == "Unknown G"] <- "Torilis arvensis"

plants$species[plants$species == "Berberis nervosa"] <- "Berberis aquifolium"

plants$species[plants$species == "Pseudostuga menziesii"] <- "Pseudotsuga menziesii"

plants$species[plants$species == "Oemlaria cerasiformis"] <- "Oemleria cerasiformis"
plants$species[plants$species == "mystery oval pointy leaf"] <- "Chenopodium album"
plants$species[plants$species == "Triteleia spp"] <- "Triteleia sp."

# some data entering corrections -----
## 2017 Plot 14 Collinsia -> Claytonia
plants$species[plants$species == "Collinsia parviflora" & plants$plot == "14" & plants$year == "2017"] <- "Claytonia perfoliata"

## 2021 plot 4 Danthonia -> Elymus
plants$species[plants$species == "Danthonia californica" & plants$plot == "4" & plants$year == "2021"] <- "Elymus glaucus"

## Create list of unique species names ----
plant_species <- data.frame(unique(plants$species[plants$cover > 0]))
plant_species

## Aggregate by year and treatment ----
# plants_ag <- plants %>%
#   group_by(year, species, treatment)%>% # group by year, species, and treatment to find total cover of each species in each treatment in each year
#   summarise(cov_sum = sum(cover)/5) # sum and divide by 5 to find avg % cover of each species per treatment per year (there are 5 plots in each treatment)
# 
# ## check that numbers are summing correctly
# plants_ag[plants_ag$species == "Melica subulata" & plants_ag$treatment == "control",]
# # 2019 = 4.6
# 
# plants[plants$species == "Melica subulata" & plants$treatment == "control" & plants$year == "2019",]
# # 6, 4, 12, 1 (total = 23)
# 23/5 # = 4.6 (this is average % cover of MESU in control plots in 2019)

# Upon further consideration, I think I won't average across the plots
plants_ag <- plants %>%
  group_by(year, species, treatment)%>% # group by year, species, and treatment to find total cover of each species in each treatment in each year
  summarise(cov_sum = sum(cover)) # sum each group

## check that aggregating didn't remove species
plant_species_ag <- data.frame(unique(plants_ag$species))
plant_species_ag # yep, still 73 species, good to go

## Plotting by species, year, and treatment (just because I'm curious what this looks like) ----
ggplot(plants_ag, aes(x = year, y = cov_sum, color = as.factor(species)))+
  geom_point()+
  xlab("Year")+
  ylab("Cover (%)")+
  labs(color = 'Species')+ # color by species
  facet_wrap(~treatment)+ # separate treatments into panels
  theme_test()

## Selecting species (Round 1) ----
## First, let's remove rare species in each treatment-year because they will have so little influence on the CWM

## Can I do this by hand? let's look at 2024 cover in each treatment
# cov_C_24 <- plants_ag[plants_ag$year == 2024 & plants_ag$treatment =="control",]
# cov_I_24 <- plants_ag[plants_ag$year == 2024 & plants_ag$treatment =="irrigated",]
# cov_D_24 <- plants_ag[plants_ag$year == 2024 & plants_ag$treatment =="drought",]
### not sure doing this by hand is very useful - way too time consuming

## Lets remove all species/trt/year groups where total cover is < 2
# dom_plants_ag <- plants_ag[plants_ag$cov_sum > 2,] # this gives 56 spp
# 
# ## what if I remove all groups where total cover < 3
# dom_plants_ag <- plants_ag[plants_ag$cov_sum > 3,] # this gives 52 spp
# 
# # then create a new species list
# dom_plants_spp <- data.frame(unique(dom_plants_ag$species)) 
# dom_plants_spp

### Second, let's remove our "unknowns" - I clearly will not be able to collect these
# sel_spp <- dom_plants_spp %>%
#   filter(unique.dom_plants_ag.species. != "Mystery 1") %>%
#   filter(unique.dom_plants_ag.species. != "Unknown A") %>%
#   filter(unique.dom_plants_ag.species. != "Unknown D") %>%
#   filter(unique.dom_plants_ag.species. != "Unknown E") %>%
#   filter(unique.dom_plants_ag.species. != "Unknown Grass") %>%
#   filter(unique.dom_plants_ag.species. != "Unknown weed") 
# 
# sel_spp # check - there are 48 spp
## honestly seems unlikely all of these spp will be present this year

## Let's redo this with the 2017-2024 data and see what happens -----
dom_plants_ag_1724 <- plants_ag[plants_ag$year != 2015 &
                       plants_ag$year != 2016 & # removing 2015-2016 data
                         plants_ag$cov_sum > 5,] # remove groups where total cover across the 5 plots is < 5
dom_plants_spp_1724 <- data.frame(unique(dom_plants_ag_1724$species)) # make species list
dom_plants_spp_1724 # unknowns got filtered out automatically
# 36 spp

## Plotting by species, year, and treatment -----
ggplot(dom_plants_ag_1724, aes(x = year, y = cov_sum, color = as.factor(species)))+
  geom_point()+
  xlab("Year")+
  ylab("Cover (%)")+
  labs(color = 'Species')+ # color by species
  facet_wrap(~treatment)+ # separate treatments into panels
  theme_test()

# Calculate what % of each plot-year is covered with all species
plot_cov <- plants %>%
  group_by(year, plot, treatment)%>% # group by year, species, and treatment to find total cover of each species in each treatment in each year
  summarise(cov_sum = sum(cover)) # sum each group
plot_cov

# Calculate what % of each plot-year is covered with only the 36 species
dm1724spp <- unique(dom_plants_ag_1724$species) # put species into variable object
dom_plants <- plants %>% filter(species %in% dm1724spp) # select cover data for only selected spp

dm1724ag <- dom_plants %>%
  group_by(year, plot, treatment) %>% # group by year, species, and treatment to find total cover of each species in each treatment in each year
  summarise(sel_cov_sum = sum(cover)) # sum each group

dm1724ag

# Put into one dataframe
cov_comp <- merge(x=dm1724ag, y=plot_cov, by = c("year", "treatment", "plot"))

# Calculate difference in cover before and after removing rare species
cov_comp$cov_diff <- (cov_comp$cov_sum - cov_comp$sel_cov_sum)
cov_comp$cov_perdiff <- (cov_comp$sel_cov_sum / cov_comp$cov_sum)*100

## Thoughts after looking at list ----
## Need to check some of these species for if they are even in the plots anymore
### Danthonia -----
# DACA_cover <- filter(plants, species == "Danthonia californica")
# 
# ggplot(DACA_cover, aes(x = year, y = cover, color = plot))+
#   geom_point()+
#   xlab("Year")+
#   ylab("DACA Cover (%)")+
#   theme_test()

# this has ONE data point??? in 2021??? what is going on here
# from looking at data sheet seems like a mis-enter (should be 9 for ELGL but also maybe its all ANOD?)
# regardless, not taking leaves from it obvi
# for now - changing to ELGL

### Quercus -----
## Need to check if Quercus will matter that much because we reallyyyy shouldn't be making it any harder for them than it already is
# QUGA_cover <- filter(plants, species == "Quercus garryana")
# 
# ggplot(QUGA_cover, aes(x = year, y = cover, color = plot))+
#   geom_point()+
#   geom_line(aes(group = plot))+
#   xlab("Year")+
#   ylab("Quercus garryana Cover (%)")+
#   #ylim(0,6)+
#   theme_test()
## hmm ok only in 5 plots (2, 3, 5, 8, 10)
# 2 - control, 3 - control, 5 - drought, 8 - control, 10 - drought

### Bromus tectorum -----
# possibly mis-ID Bromus sterilis? not in the plots anymore regardless and might get replaced

### Collinsia -----
## is this in the plots anymore?
# COPA_cover <- filter(plants, species == "Collinsia parviflora")
# 
# ggplot(COPA_cover, aes(x = year, y = cover, color = plot))+
#   geom_point()+
#   geom_line(aes(group = plot))+
#   xlab("Year")+
#   ylab("COPA Cover (%)")+
#   ylim(0,25)+
#   theme_test()

## hmmm - didn't find last year, v low numbers and only in plot 1
## 20 % in plot 14 in 2017 and never seen again??? how does that happen??
## data entered incorrectly! should be 20% of claytonia in that plot (grr)
## went back and corrected in data cleaning step - should be fine now

## Well now I want to check the other species to check for errors
### All species - facet wrap -----
ggplot(dom_plants, aes(x = year, y = cover, color = plot))+
  geom_jitter()+
  xlab("Year")+
  ylab("Cover (%)")+
  facet_wrap(~species)+
  theme_test()
### Allium -----
ALAM_cover <- filter(plants, species == "Allium amplectens")
 
ggplot(ALAM_cover, aes(x = year, y = cover, color = plot))+
   geom_jitter()+
   xlab("Year")+
   ylab("ALAM Cover (%)")+
   theme_test()

### Anthoxanthum -----
ANOD_cover <- filter(plants, species == "Anthoxanthum odoratum")

ggplot(ANOD_cover, aes(x = year, y = cover, color = plot))+
  geom_jitter()+
  xlab("Year")+
  ylab("ANOD Cover (%)")+
  theme_test()

### Berberis aquifolium -----
BEAQ_cover <- filter(plants, species == "Berberis aquifolium")

ggplot(BEAQ_cover, aes(x = year, y = cover, color = plot))+
  geom_jitter()+
  xlab("Year")+
  ylab("BEAQ Cover (%)")+
  theme_test()

### Leucanthemum vulgare -----
LEVU_cover <- filter(plants, species == "Leucanthemum vulgare")
ggplot(LEVU_cover, aes(x = year, y = cover,
                       color = plot))+
  geom_jitter()+
  xlab("Year")+
  ylab("LEVU Cover (%)")+
  theme_test()
## hmm ok hasn't been seen in years and only in 3 plots, will not collect

### Bromus carinatus -----
ggplot(dom_plants, aes(x = year, y = cover, color = plot))+
  geom_point(data=subset(dom_plants, species =='Bromus carinatus'))+
  xlab("Year")+
  ylab("BRCA Cover (%)")+
  geom_line(data=subset(dom_plants, species =='Bromus carinatus'), aes(group = plot))+
  theme_test()

### Lotus micranthus -----
ggplot(dom_plants, aes(x = year, y = cover, color = plot))+
  geom_jitter(data=subset(dom_plants, species =='Lotus micranthus'))+
  xlab("Year")+
  ylab("Lotus micranthus Cover (%)")+
  #geom_line(data=subset(dom_plants, species =='Lotus micranthus'), aes(group = plot))+
  theme_test()

### Luzula spp -----
ggplot(dom_plants, aes(x = year, y = cover, color = plot))+
  geom_jitter(data=subset(dom_plants, species =='Luzula sp.'))+
  xlab("Year")+
  ylab("Luzula sp. Cover (%)")+
  #geom_line(data=subset(dom_plants, species =='Luzula sp.'), aes(group = plot))+
  theme_test()

### Claytonia -----
claytonia <- subset(dom_plants, species =='Claytonia perfoliata')

ggplot(dom_plants, aes(x = year, y = cover, color = plot))+
  geom_jitter(data=subset(dom_plants, species =='Claytonia perfoliata'))+
  xlab("Year")+
  ylab("Claytonia perfoliata Cover (%)")+
  #geom_line(data=subset(dom_plants, species =='Claytonia perfoliata'), aes(group = plot))+
  theme_test()

## Making final list ----
# some decisions
#### removing Quercus for conservation reasons (check with Jenn)
#### removing Bromus tectorum, Leucanthemum vulgare, Lotus micranthus bc they probs wont be in plots
### Qs for Jenn: Allium? Berberis? Luzula? Vulpia? - kind of depends on their abundance this year I think - but do make up a good chunk of the plots they are in

# Final list
sel_spp <- data.frame(unique(dom_plants_ag_1724$species)) %>%
   filter(unique.dom_plants_ag_1724.species. != "Quercus garryana") %>%
   filter(unique.dom_plants_ag_1724.species. != "Bromus tectorum") %>%
   filter(unique.dom_plants_ag_1724.species. != "Leucanthemum vulgare")%>%
  filter(unique.dom_plants_ag_1724.species. != "Lotus micranthus")

#write.csv(sel_spp, "~/School/Williams Lab/cowichan-diversity/output/selectedspecies_traitanalysis_18APR2025.csv")

sel_spp <- unique(sel_spp$unique.dom_plants_ag_1724.species.)

# Calculate what % of each plot-year is covered with only the 36 species
sel_plants <- plants %>% filter(species %in% sel_spp) # select cover data for only selected spp

sel_ag <- sel_plants %>%
  group_by(year, plot, treatment) %>% # group by year, species, and treatment to find total cover of each species in each treatment in each year
  summarise(sel_cov_sum = sum(cover)) # sum each group

sel_ag

# Put into one dataframe
cov_comp <- merge(x=sel_ag, y=plot_cov, by = c("year", "treatment", "plot"))

# Calculate difference in cover before and after removing rare species
cov_comp$cov_diff <- (cov_comp$cov_sum - cov_comp$sel_cov_sum)
cov_comp$cov_perdiff <- (cov_comp$sel_cov_sum / cov_comp$cov_sum)*100
