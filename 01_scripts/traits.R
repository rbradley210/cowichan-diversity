# traits analysis
# Robin Bradley
# robin.bradley@ubc.ca
# Last updated: 9 January 2025

# 1. Set-up ----
# set WD
setwd("C:/Users/Robin/Documents/School/Williams Lab")

# packages
# library(rtry) # for reading in and cleaning data from TRY database
library(readxl)
library(tidyverse)
library(visreg)
library(emmeans)
library(FSA)
library(car)
library(lme4)

# read in data
height <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Height")
weight <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Dry Weight")
attr <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Species")
plots <- read.csv("/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info/IDE_plotinfo.csv", header = TRUE)

leafarea <- read.csv("~/School/Williams Lab/cowichan-diversity/00_rawdata/EROR_leafarea.csv")

## selecting plants for N analysis ----
# weight.red <- weight %>% 
#   select(c("Plant Species", "plot", "Plant ID", "Leaf ID", "Part ID", "Weight (mg)"))
# 
# weight.leaves <- weight.red %>%
#   group_by(`Plant Species`, plot, `Plant ID`, `Leaf ID`) %>%
#   summarise(tot.weight = sum(`Weight (mg)`)) # combine part IDs to leaf level
# 
# plants <- weight.leaves %>% 
#   group_by(`Plant Species`, plot, `Plant ID`) %>%
#   summarise(`leaf_ids` = list(`Leaf ID`),
#             tot.weight = sum(tot.weight)) # combine leaf IDs to plant level
# 
# plants$leaf_ids <- gsub("c", "", plants$leaf_ids) # clean leaf IDs column for readability
# plants$leaf_ids <- gsub("\\(", "", plants$leaf_ids)
# plants$leaf_ids <- gsub("\\)", "", plants$leaf_ids)
# 
# plants <- left_join(plants, plots, by = "plot")
# 
# plants$sampleID <- paste(plants$`Plant Species`, 
#                          plants$plot, 
#                          plants$`Plant ID`, 
#                          sep = "_") # create sample ID column
# 
# # check for duplicates
# n_occur <- data.frame(table(plants$sampleID))
# plants[plants$sampleID %in% n_occur$Var1[n_occur$Freq > 1],]
# unique(n_occur$Freq)
# rm(n_occur)
# 
# # remove plants with less than 3mg
# plants <- plants[plants$tot.weight > 3, ]
# 
# # export CSV
# write.csv(plants, 
#           "~/School/Williams Lab/cowichan-diversity/04_other_outputs/samplesforNanalysis_inclLOUT.csv", 
#           row.names = FALSE)
# # edited down in excel
# 
# # bring in new sheet
# Nplants <- read.csv("~/School/Williams Lab/cowichan-diversity/04_other_outputs/samplesforNanalysis.csv",
#                   header = TRUE)
# 
# # make table with number of plants per species per trt
# Nplants.tbl <- Nplants %>%
#   group_by(Plant.Species, trt)%>%
#   summarise(count = n())
# 
# # make table with number of trts per species
# Nplants.tbl.2 <- Nplants.tbl %>%
#   group_by(Plant.Species)%>%
#   summarise(count = n())


# 2. Data Cleaning ----
## Height
height <- height %>%
  left_join(plots) %>%
  left_join(attr)

### Log transform height
height$logheight <- log(height$height)
height$species <- as.factor(height$species)
height$plot <- as.factor(height$plot)
height$trt <- factor(height$trt, levels=c("control","drought","irrigated"))
height$growthform <- as.factor(height$growthform)
height$duration <- as.factor(height$duration)
height$nativity <- as.factor(height$nativity)


## Weight
weight <- weight %>%
  dplyr::group_by(`Plant Species`, plot, `Plant ID`, `Leaf ID`) %>%
  dplyr::summarize(tot.weight = sum(`Weight (mg)`))

weight$sample <- paste(weight$`Plant Species`, 
                         weight$plot,
                         weight$`Plant ID`, 
                         sep = "_") # create sample ID column to match leaf area
weight$sample <- paste(weight$sample,
                         weight$`Leaf ID`, 
                         sep = "") # add leaf ID

## Leaf Nitrogen

## Specific Leaf Area
sla <- left_join(leafarea, weight) # combine datasets
sla$sla <- sla$total.leaf.area/sla$tot.weight # calculate SLA


# 3. Data Exploration ----
# Height
ggplot(height, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()


# 4. Data Analysis ----
## Height ---- 
### Kruskal_wallis + Dunn tests on differences b/t treatments within a species ----
h_ALAM <- filter(height, species == "ALAM") ##
kruskal.test(height ~ trt, data = h_ALAM)
dunnTest(height ~ trt,
              data=h_ALAM,
              method="bh")

h_ANOD <- filter(height, species == "ANOD")
kruskal.test(height ~ trt, data = h_ANOD)

h_BEAQ <- filter(height, species == "BEAQ")
kruskal.test(height ~ trt, data = h_BEAQ)

h_BRCA <- filter(height, species == "BRCA")
kruskal.test(height ~ trt, data = h_BRCA)

h_BRHO <- filter(height, species == "BRHO")
kruskal.test(height ~ trt, data = h_BRHO) ###
dunnTest(height ~ trt,
         data=h_BRHO,
         method="bh")

h_BRST <- filter(height, species == "BRST")
kruskal.test(height ~ trt, data = h_BRST)

h_CAIN <- filter(height, species == "CAIN")
kruskal.test(height ~ trt, data = h_CAIN)

h_CAQU <- filter(height, species == "CAQU")
kruskal.test(height ~ trt, data = h_CAQU)

h_CEGL <- filter(height, species == "CEGL")
kruskal.test(height ~ trt, data = h_CEGL)

h_CLPE <- filter(height, species == "CLPE") ##
kruskal.test(height ~ trt, data = h_CLPE)
dunnTest(height ~ trt,
         data=h_CLPE,
         method="bh")

 h_DAGL <- filter(height, species == "DAGL")
kruskal.test(height ~ trt, data = h_DAGL)

h_DEME <- filter(height, species == "DEME")
kruskal.test(height ~ trt, data = h_DEME)

h_ELGL <- filter(height, species == "ELGL") ###
kruskal.test(height ~ trt, data = h_ELGL)
dunnTest(height ~ trt,
         data=h_ELGL,
         method="bh")

h_EROR <- filter(height, species == "EROR")
kruskal.test(height ~ trt, data = h_EROR)

h_GAAP <- filter(height, species == "GAAP")
kruskal.test(height ~ trt, data = h_GAAP)

h_GEMO <- filter(height, species == "GEMO")
kruskal.test(height ~ trt, data = h_GEMO)

h_LASP <- filter(height, species == "LASP")
kruskal.test(height ~ trt, data = h_LASP)

h_LOUT <- filter(height, species == "LOUT")
kruskal.test(height ~ trt, data = h_LOUT)

h_LUSP <- filter(height, species == "LUSP")
kruskal.test(height ~ trt, data = h_LUSP)

h_MESU <- filter(height, species == "MESU")
kruskal.test(height ~ trt, data = h_MESU)

h_NEPA <- filter(height, species == "NEPA")
kruskal.test(height ~ trt, data = h_NEPA)

h_PLCO <- filter(height, species == "PLCO")
kruskal.test(height ~ trt, data = h_PLCO)

h_POPR <- filter(height, species == "POPR")
kruskal.test(height ~ trt, data = h_POPR)

h_PRHE <- filter(height, species == "PRHE")
kruskal.test(height ~ trt, data = h_PRHE)

h_RAOC <- filter(height, species == "RAOC") ####
kruskal.test(height ~ trt, data = h_RAOC)
dunnTest(height ~ trt,
         data=h_RAOC,
         method="bh")

h_SACR <- filter(height, species == "SACR")
kruskal.test(height ~ trt, data = h_SACR)

h_SYAL <- filter(height, species == "SYAL")
kruskal.test(height ~ trt, data = h_SYAL)

h_VALO <- filter(height, species == "VALO") ##
kruskal.test(height ~ trt, data = h_VALO)
dunnTest(height ~ trt,
         data=h_VALO,
         method="bh")

h_VEAR <- filter(height, species == "VEAR")
kruskal.test(height ~ trt, data = h_VEAR)

h_VIHI <- filter(height, species == "VIHI")
kruskal.test(height ~ trt, data = h_VIHI)

h_VISA <- filter(height, species == "VISA")
kruskal.test(height ~ trt, data = h_VISA)

h_VUSP <- filter(height, species == "VUSP") ##
kruskal.test(height ~ trt, data = h_VUSP)
dunnTest(height ~ trt,
         data=h_VUSP,
         method="bh")

### Test for treatment effects (linear models) ----
lm_overall <- lmer(logheight ~ trt + (1|species) + (1|plot), data = height)
summary(lm_overall)
Anova(lm_overall)

#### Linear model with func. group and trt as fixed, plot as random
lm_func <- lmer(logheight ~ trt*growthform + (1|species) + (1|plot), data = height)
summary(lm_func)
Anova(lm_func)

#### Linear model with nativity and trt as fixed, plot as random
lm_nat <- lmer(logheight ~ trt*nativity + (1|species) + (1|plot), data = height)
summary(lm_nat)
Anova(lm_nat)


#### Linear model with duration and trt as fixed, plot as random
lm_dur <- lmer(logheight ~ trt*duration + (1|species) + (1|plot), data = height)
summary(lm_dur)
Anova(lm_dur)



# 5. Data Visualization ----
## Height ----
### ALAM
ggplot(h_ALAM, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### BRHO
ggplot(h_BRHO, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### CLPE
ggplot(h_CLPE, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### ELGL
ggplot(h_ELGL, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### RAOC
ggplot(h_RAOC, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### VALO
ggplot(h_VALO, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

### VUSP
ggplot(h_VUSP, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()
