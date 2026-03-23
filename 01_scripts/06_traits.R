# traits analysis
# Robin Bradley
# robin.bradley@ubc.ca
# Last updated: 18 March 2026

# 1. Set-up ----
# set WD
setwd("C:/Users/Robin/Documents/School/Williams Lab")

# packages
library(readxl)
library(tidyverse)
library(ggtext)
library(visreg)
library(emmeans)
library(FSA)
library(car)
library(nlme)
library(ape)
# library(V.PhyloMaker2)
# library(phylolm)
library(car)

# read in data
# Source cowichan_cleanup script
source("~/School/Williams Lab/cowichan-diversity/01_scripts/01_cowichan_cleanup.R")

# Source Paul Hacker leaf N data
source("~/School/Williams Lab/cowichan-diversity/01_scripts/paul_hacker_data.R")

height <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Height")
weight <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Dry Weight")
area <- read.csv("~/School/Williams Lab/cowichan-diversity/00_rawdata/leafarea.csv")
attr <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Species")
plots <- read.csv("/Users/Robin/Dropbox/Williams' Lab/Cowichan IDE/Data & Plot info/IDE_plotinfo.csv", header = TRUE)
phylo <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "phylo")
leafN <- read_excel("~/School/Williams Lab/cowichan-diversity/00_rawdata/Cowichan IDE Trait Data Sheet.xlsx", sheet = "Leaf N")


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
## Height ----
height <- height %>%
  left_join(plots) %>%
  left_join(attr) %>%
  left_join(select(phylo, species_id, species), by = "species_id")

height$logheight <- log(height$height) # Log transform height
height$species_id <- as.factor(height$species_id)
height$plot <- as.factor(height$plot)
height$trt <- factor(height$trt, levels=c("control","drought","irrigated"))
height$growthform <- as.factor(height$growthform)
height$duration <- as.factor(height$duration)
height$nativity <- as.factor(height$nativity)

## Weight ----
weight <- weight %>%
  dplyr::group_by(species_id, plot, `Plant ID`, `Leaf ID`) %>%
  dplyr::summarize(tot.weight = sum(weight))


weight$sample <- paste(weight$species_id, 
                         weight$plot,
                         weight$`Plant ID`, 
                         sep = "_") # create sample ID column to match leaf area
weight$sample <- paste(weight$sample,
                         weight$`Leaf ID`, 
                         sep = "") # add leaf ID

## Specific Leaf Area ----
sla <- left_join(area, weight) %>% # combine datasets
  na.omit()%>% # remove NAs
  left_join(plots)%>%
  left_join(attr) %>%
  left_join(select(phylo, species_id, species), by = "species_id")
  
sla$total.leaf.area.mm <- 100*(sla$total.leaf.area) # convert leaf area from cm2 to mm2
sla$sla <- sla$total.leaf.area.mm/sla$tot.weight # calculate SLA
sla$logsla <- log(sla$sla)
sla$log10sla <- log10(sla$sla)
sla$sqrtsla <- sqrt(sla$sla)

# hist(sla$sla, right = FALSE) # non-normal
# hist(sla$logsla, right = FALSE)

# Leaf Area and Leaf Weight
sla$logla <- log(sla$total.leaf.area.mm)
sla$loglw <- log(sla$tot.weight)

## Leaf Nitrogen ----
leafN <- leafN %>%
  separate(col = sample_id, 
           into = c("species_id", "plot", "plant_id"),
           sep = "_",
           remove =FALSE)
leafN$plot <- as.numeric(leafN$plot)
leafN <- leafN%>%
  left_join(plots) %>%
  left_join(attr) %>%
  left_join(select(phylo, species_id, species), by = "species_id")

## Making phylogenetic tree ----
### run the function
# phylo_red <- phylo[, -1]
# result <- phylo.maker(phylo_red)
# tree3 <- result$scenario.3
# 
# 
# plot.phylo(result$scenario.3, cex = 1.5, main = "scenario.3")

## Calcuate Community Weighted Means ----
# reformat species names to match trait dataframes
plants_allbromus$species <- gsub(" sp.", "", plants_allbromus$species)
plants_allbromus$species <- gsub(" ", "_", plants_allbromus$species)
plants_allbromus$species <- gsub("Lathyrusaericus", "Lathyrus_sphaericus", plants_allbromus$species)
colnames(plants_allbromus)[colnames(plants_allbromus) == "treatment"] <- "trt"


# Calculate treatment averages of traits and remove unnecessary columns
## SLA
sla_avg <- sla %>%
  dplyr::group_by(species, trt)%>%
  dplyr::summarize(mean_sla = mean(logsla))

## Height
height_avg <- height %>%
  dplyr::group_by(species, trt)%>%
  dplyr::summarize(mean_height = mean(height))

## Leaf N
mean.paul <- clean.paul.dat %>%
  dplyr::group_by(species)%>%
  dplyr::summarize(mean_leafN = mean(N_perc))

leafn_avg <- leafN %>%
  dplyr::group_by(species, trt)%>%
  dplyr::summarize(mean_leafN = mean(per_N))

## CWMs
CWM.trait <- plants_allbromus %>% 
  left_join(sla_avg) %>%
  left_join(height_avg) %>%
  left_join(leafn_avg)%>%
  rows_patch(mean.paul, by = "species", unmatched = "ignore")

CWM <-   # New dataframe where we can inspect the result
  CWM.trait %>% # First step in the next string of statements
  dplyr::group_by(plot, trt, year) %>%   # Groups the summary file by Plot number
  dplyr::summarize(           # Coding for how we want our CWMs summarized
    Height_cwm = weighted.mean(mean_height, cover, na.rm = TRUE),   # Actual calculation of CWMs
    SLA_cwm = weighted.mean(mean_sla, cover, na.rm = TRUE),
    N_cwm = weighted.mean(mean_leafN, cover, na.rm = TRUE))%>%
  left_join(weather.prev.gs)%>%
  left_join(weather.prev.su)%>%
  left_join(weather.prev.sp)
CWM$year <- as.integer(as.factor(CWM$year))
CWM$trt <- as.factor(CWM$trt)


# 3. Data Exploration ----
# Height
ggplot(height, aes(x = trt, y = height, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()

# CWM Height
ggplot(CWM, aes(x = year, y = Height_cwm, color = trt))+
  geom_jitter()+
  xlab("Year")+
  ylab(" CWM Height (cm)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

# SLA
ggplot(sla, aes(x = trt, y = sla, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("SLA")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()

# Leaf Area
hist(sla$total.leaf.area.mm,right=FALSE)
hist(sla$logsla, right=FALSE)

# Leaf Weight
hist(sla$tot.weight, right = FALSE)
hist(sla$loglw, right = FALSE)

# CWM SLA
ggplot(CWM, aes(x = year, y = SLA_cwm, color = trt))+
  geom_jitter()+
  xlab("Year")+
  ylab(" CWM log(SLA)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()

# Leaf N
ggplot(leafN, aes(x = trt, y = per_N, color = trt))+
  geom_jitter()+
  xlab("Treatment")+
  ylab("Leaf N")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  facet_wrap(~species)+
  theme_test()

# CWM Leaf N
ggplot(CWM, aes(x = year, y = N_cwm, color = trt))+
  geom_jitter()+
  xlab("Year")+
  ylab(" CWM leaf N (%)")+
  scale_color_manual(values=c("black", "coral", "deepskyblue3")) +
  theme_test()


# 4. Data Analysis ----
## Comparison of Species ----
### Height ---- 
### Test for treatment effects (linear models)
# lm_overall <- lmer(logheight ~ trt + (1|species) + (1|plot), data = height)
# summary(lm_overall)
# Anova(lm_overall)
# plot(lm_overall)
# 
# #### Linear model with func. group and trt as fixed, plot as random
# lm_func <- lmer(logheight ~ trt*growthform + (1|species) + (1|plot), data = height)
# summary(lm_func)
# Anova(lm_func)
# plot(lm_func)
# 
# h_func_grpmeans <- emmeans(lm_func, c("trt", "growthform"))
# pairs(h_func_grpmeans)
# plot(h_func_grpmeans)
# pwpm(h_func_grpmeans, means = FALSE, diffs = FALSE)
# 
# #### Linear model with nativity and trt as fixed, plot as random
# lm_nat <- lmer(logheight ~ trt*nativity + (1|species) + (1|plot), data = height)
# summary(lm_nat)
# Anova(lm_nat)
# 
# #### Linear model with duration and trt as fixed, plot as random
# lm_dur <- lmer(logheight ~ trt*duration + (1|species) + (1|plot), data = height)
# summary(lm_dur)
# Anova(lm_dur)


### SLA ----
### Test for treatment effects (linear models)
# lm_sla_overall <- lmer(logsla ~ trt + (1|species) + (1|plot), data = sla)
# summary(lm_sla_overall)
# Anova(lm_sla_overall)
# plot(lm_sla_overall)
# 
# sla_overall_grpmeans <- emmeans(lm_sla_overall, c("trt"))
# pairs(sla_overall_grpmeans)
# plot(sla_overall_grpmeans)
# pwpm(sla_overall_grpmeans, means = FALSE, diffs = FALSE)

# row names have to be the species to  match tip labels ??????
# 
# pgls_sla_overall <- phylolm(logsla~trt, 
#                            data = sla,
#                            phy = tree3,
#                            boot = 100)

#### Linear model with func. group and trt as fixed, plot as random
# lm_sla_func <- lmer(logsla ~ trt*growthform + (1|species) + (1|plot), data = sla)
# summary(lm_sla_func)
# Anova(lm_sla_func)
# 
# sla_func_grpmeans <- emmeans(lm_sla_func, c("trt", "growthform"))
# pairs(sla_func_grpmeans)
# plot(sla_func_grpmeans)
# pwpm(sla_func_grpmeans, means = FALSE, diffs = FALSE)
# 
# #### Linear model with nativity and trt as fixed, plot as random
# lm_sla_nat <- lmer(logsla ~ trt*nativity + (1|species) + (1|plot), data = sla)
# summary(lm_sla_nat)
# Anova(lm_sla_nat)
# 
# sla_nat_grpmeans <- emmeans(lm_sla_nat, c("trt", "nativity"))
# pairs(sla_nat_grpmeans)
# #plot(sla_nat_grpmeans)
# pwpm(sla_nat_grpmeans, means = FALSE, diffs = FALSE)
# 
# 
# #### Linear model with duration and trt as fixed, plot as random
# lm_sla_dur <- lmer(logsla ~ trt*duration + (1|species) + (1|plot), data = sla)
# summary(lm_sla_dur)
# Anova(lm_sla_dur)
# 
# sla_dur_grpmeans <- emmeans(lm_sla_dur, c("trt", "duration"))
# pairs(sla_dur_grpmeans)
# #plot(sla_dur_grpmeans)
# pwpm(sla_dur_grpmeans, means = FALSE, diffs = FALSE)


### Leaf Area ----
### Test for treatment effects (linear models)
lm_la_overall <- lmer(logla ~ trt + (1|species) + (1|plot), data = sla)
summary(lm_la_overall)
Anova(lm_la_overall)
plot(lm_la_overall)

la_overall_grpmeans <- emmeans(lm_la_overall, c("trt"))
pairs(la_overall_grpmeans)
plot(la_overall_grpmeans)
pwpm(la_overall_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with func. group and trt as fixed, plot as random
lm_la_func <- lmer(logla ~ trt*growthform + (1|species) + (1|plot), data = sla)
summary(lm_la_func)
Anova(lm_la_func)

la_func_grpmeans <- emmeans(lm_la_func, c("trt", "growthform"))
pairs(la_func_grpmeans)
plot(la_func_grpmeans)
pwpm(la_func_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with nativity and trt as fixed, plot as random
lm_la_nat <- lmer(logla ~ trt*nativity + (1|species) + (1|plot), data = sla)
summary(lm_la_nat)
Anova(lm_la_nat)

la_nat_grpmeans <- emmeans(lm_la_nat, c("trt", "nativity"))
pairs(la_nat_grpmeans)
plot(la_nat_grpmeans)
pwpm(la_nat_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with duration and trt as fixed, plot as random
lm_la_dur <- lmer(logla ~ trt*duration + (1|species) + (1|plot), data = sla)
summary(lm_la_dur)
Anova(lm_la_dur)

la_dur_grpmeans <- emmeans(lm_la_dur, c("trt", "duration"))
pairs(la_dur_grpmeans)
plot(la_dur_grpmeans)
pwpm(la_dur_grpmeans, means = FALSE, diffs = FALSE)

### Leaf Weight ----
### Test for treatment effects (linear models)
lm_lw_overall <- lmer(loglw ~ trt + (1|species) + (1|plot), data = sla)
summary(lm_lw_overall)
Anova(lm_lw_overall)
plot(lm_lw_overall)

lw_overall_grpmeans <- emmeans(lm_lw_overall, c("trt"))
pairs(lw_overall_grpmeans)
plot(lw_overall_grpmeans)
pwpm(lw_overall_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with func. group and trt as fixed, plot as random
lm_lw_func <- lmer(loglw ~ trt*growthform + (1|species) + (1|plot), data = sla)
summary(lm_lw_func)
Anova(lm_lw_func)

lw_func_grpmeans <- emmeans(lm_lw_func, c("trt", "growthform"))
pairs(lw_func_grpmeans)
plot(lw_func_grpmeans)
pwpm(lw_func_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with nativity and trt as fixed, plot as random
lm_lw_nat <- lmer(loglw ~ trt*nativity + (1|species) + (1|plot), data = sla)
summary(lm_lw_nat)
Anova(lm_lw_nat)

lw_nat_grpmeans <- emmeans(lm_lw_nat, c("trt", "nativity"))
pairs(lw_nat_grpmeans)
plot(lw_nat_grpmeans)
pwpm(lw_nat_grpmeans, means = FALSE, diffs = FALSE)

#### Linear model with duration and trt as fixed, plot as random
lm_lw_dur <- lmer(loglw ~ trt*duration + (1|species) + (1|plot), data = sla)
summary(lm_lw_dur)
Anova(lm_lw_dur)

lw_dur_grpmeans <- emmeans(lm_lw_dur, c("trt", "duration"))
pairs(lw_dur_grpmeans)
plot(lw_dur_grpmeans)
pwpm(lw_dur_grpmeans, means = FALSE, diffs = FALSE)

## CWMs ----
### Height ----
# base models
# null models
h.null <- lme(Height_cwm~1, 
              data = CWM, 
              random = ~1|plot)

h.null.cor <- lme(Height_cwm ~ 1,
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

h.yr <- lme(Height_cwm~year,
            data = CWM,
            random = ~1|plot)

h.yr.cor <- lme(Height_cwm ~ year,
                data = CWM,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))

h.trt <- lme(Height_cwm~trt,
             data = CWM,
             random = ~1|plot)

h.trt.cor <- lme(Height_cwm ~ trt,
                 data = CWM,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

# base models
h.base <- lme(Height_cwm ~ trt+year, # base
              data = CWM,
              random = ~1|plot)
plot(h.base)

h.base.int <- lme(Height_cwm ~ trt*year, # base + int
                  data = CWM,
                  random = ~1|plot)
plot(h.base.int)

h.base.cor <- lme(Height_cwm ~ trt+year, # base + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(h.base.cor)

h.base.int.cor <- lme(Height_cwm ~ trt*year, # base + int + cor
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(h.base.int.cor)

# weather models 
## previous growing season
h.pgs.p <- lme(Height_cwm ~ trt+year+prev.gs.precip, # base + prev gs precip
               data = CWM,
               random = ~1|plot)
plot(h.pgs.p) 

h.pgs.t <- lme(Height_cwm ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
               data = CWM,
               random = ~1|plot)
plot(h.pgs.t) # examine residuals,

h.pgs.p.t <- lme(Height_cwm ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = CWM,
                 random = ~1|plot)
plot(h.pgs.p.t) # examine residuals

## previous spring
h.sp.p <- lme(Height_cwm ~ trt+year+prev.sp.precip, # base + prev sp precip
              data = CWM,
              random = ~1|plot)
plot(h.sp.p) 

h.sp.t <- lme(Height_cwm ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
              data = CWM,
              random = ~1|plot)
plot(h.sp.t) # examine residuals,

h.sp.p.t <- lme(Height_cwm ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                data = CWM,
                random = ~1|plot)
plot(h.sp.p.t) # examine residuals

## previous summer
h.su.p <- lme(Height_cwm ~ trt+year+prev.su.precip, # base + prev su precip
              data = CWM,
              random = ~1|plot)
plot(h.su.p) 

h.su.t <- lme(Height_cwm ~ trt+year+prev.su.mean.temp, # base + prev su temp
              data = CWM,
              random = ~1|plot)
plot(h.su.t) # examine residuals,

h.su.p.t <- lme(Height_cwm ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                data = CWM,
                random = ~1|plot)
plot(h.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
h.pgs.p.cor <- lme(Height_cwm ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                   data = CWM,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(h.pgs.p.cor) # examine residuals

h.pgs.t.cor <- lme(Height_cwm ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                   data = CWM,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(h.pgs.t.cor) # examine residuals

h.pgs.p.t.cor <- lme(Height_cwm ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                     data = CWM,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(h.pgs.p.t.cor) # examine residuals

## previous spring
h.sp.p.cor <- lme(Height_cwm ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(h.sp.p.cor) # examine residuals

h.sp.t.cor <- lme(Height_cwm ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(h.sp.t.cor) # examine residuals

h.sp.p.t.cor <- lme(Height_cwm ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                    data = CWM,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(h.sp.p.t.cor) # examine residuals

## previous summer
h.su.p.cor <- lme(Height_cwm ~ trt+year+prev.su.precip, # base + prev su precip + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(h.su.p.cor) # examine residuals

h.su.t.cor <- lme(Height_cwm ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(h.su.t.cor) # examine residuals

h.su.p.t.cor <- lme(Height_cwm ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                    data = CWM,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(h.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
h.pgs.p.int <- lme(Height_cwm ~ trt*year*prev.gs.precip, # base + prev gs precip + int
                   data = CWM,
                   random = ~1|plot)
plot(h.pgs.p.int) 

h.pgs.t.int <- lme(Height_cwm ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
                   data = CWM,
                   random = ~1|plot)
plot(h.pgs.t.int) # examine residuals

h.pgs.p.t.int <- lme(Height_cwm ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                     data = CWM,
                     random = ~1|plot)
plot(h.pgs.p.t.int) # examine residuals

## previous spring
h.sp.p.int <- lme(Height_cwm ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                  data = CWM,
                  random = ~1|plot)
plot(h.sp.p.int) 

h.sp.t.int <- lme(Height_cwm ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                  data = CWM,
                  random = ~1|plot)
plot(h.sp.t.int) # examine residuals

h.sp.p.t.int <- lme(Height_cwm ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                    data = CWM,
                    random = ~1|plot)
plot(h.sp.p.t.int) # examine residuals

## previous summer
h.su.p.int <- lme(Height_cwm ~ trt*year*prev.su.precip, # base + prev su precip + int
                  data = CWM,
                  random = ~1|plot)
plot(h.su.p.int) 

h.su.t.int <- lme(Height_cwm ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                  data = CWM,
                  random = ~1|plot)
plot(h.su.t.int) # examine residuals

h.su.p.t.int <- lme(Height_cwm ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                    data = CWM,
                    random = ~1|plot)
plot(h.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
h.pgs.p.cor.int <- lme(Height_cwm ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                       data = CWM,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(h.pgs.p.cor.int) # examine residuals

h.pgs.t.cor.int <- lme(Height_cwm ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                       data = CWM,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(h.pgs.t.cor.int) # examine residuals

h.pgs.p.t.cor.int <- lme(Height_cwm ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                         data = CWM,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(h.pgs.p.t.cor.int) # examine residuals

## previous spring
h.sp.p.cor.int <- lme(Height_cwm ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(h.sp.p.cor.int) # examine residuals

h.sp.t.cor.int <- lme(Height_cwm ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(h.sp.t.cor.int) # examine residuals

h.sp.p.t.cor.int <- lme(Height_cwm ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                        data = CWM,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(h.sp.p.t.cor.int) # examine residuals

## previous summer
h.su.p.cor.int <- lme(Height_cwm ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(h.su.p.cor.int) # examine residuals

h.su.t.cor.int <- lme(Height_cwm ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(h.su.t.cor.int) # examine residuals

h.su.p.t.cor.int <- lme(Height_cwm ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                        data = CWM,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(h.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(h.null), AIC(h.null.cor), AIC(h.yr), AIC(h.yr.cor), AIC(h.trt), AIC(h.trt.cor),
           AIC(h.base), AIC(h.base.cor), AIC(h.base.int), AIC(h.base.int.cor),
           
           AIC(h.pgs.p), AIC(h.pgs.p.cor), AIC(h.pgs.p.int), AIC(h.pgs.p.cor.int),
           AIC(h.pgs.t), AIC(h.pgs.t.cor), AIC(h.pgs.t.int), AIC(h.pgs.t.cor.int),
           AIC(h.pgs.p.t), AIC(h.pgs.p.t.cor), AIC(h.pgs.p.t.int), AIC(h.pgs.p.t.cor.int),
           
           AIC(h.sp.p), AIC(h.sp.p.cor), AIC(h.sp.p.int), AIC(h.sp.p.cor.int),
           AIC(h.sp.t), AIC(h.sp.t.cor), AIC(h.sp.t.int), AIC(h.sp.t.cor.int),
           AIC(h.sp.p.t), AIC(h.sp.p.t.cor), AIC(h.sp.p.t.int), AIC(h.sp.p.t.cor.int),
           
           AIC(h.su.p), AIC(h.su.p.cor), AIC(h.su.p.int), AIC(h.su.p.cor.int),
           AIC(h.su.t), AIC(h.su.t.cor), AIC(h.su.t.int), AIC(h.su.t.cor.int),
           AIC(h.su.p.t), AIC(h.su.p.t.cor), AIC(h.su.p.t.int), AIC(h.su.p.t.cor.int))
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
Height.CWM.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                             stringsAsFactors = FALSE)

# summary
summary(h.su.t.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in CWM Height
Anova(h.su.t.cor, type = 3) #marginal, not sequential


### SLA ----
# null models
s.null <- lme(SLA_cwm~1, 
              data = CWM, 
              random = ~1|plot)

s.null.cor <- lme(SLA_cwm ~ 1,
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))

s.yr <- lme(SLA_cwm~year,
            data = CWM,
            random = ~1|plot)

s.yr.cor <- lme(SLA_cwm ~ year,
                data = CWM,
                random = ~1|plot,
                correlation = corAR1(form = ~year|plot))

s.trt <- lme(SLA_cwm~trt,
             data = CWM,
             random = ~1|plot)

s.trt.cor <- lme(SLA_cwm ~ trt,
                 data = CWM,
                 random = ~1|plot,
                 correlation = corAR1(form = ~year|plot))

# base models
s.base <- lme(SLA_cwm ~ trt+year, # base
              data = CWM,
              random = ~1|plot)
plot(s.base)

s.base.int <- lme(SLA_cwm ~ trt*year, # base + int
                  data = CWM,
                  random = ~1|plot)
plot(s.base.int)

s.base.cor <- lme(SLA_cwm ~ trt+year, # base + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(s.base.cor)

s.base.int.cor <- lme(SLA_cwm ~ trt*year, # base + int + cor
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(s.base.int.cor)

# weather models 
## previous growing season
s.pgs.p <- lme(SLA_cwm ~ trt+year+prev.gs.precip, # base + prev gs precip
               data = CWM,
               random = ~1|plot)
plot(s.pgs.p) 

s.pgs.t <- lme(SLA_cwm ~ trt+year+prev.gs.mean.temp, # base + prev gs temp
               data = CWM,
               random = ~1|plot)
plot(s.pgs.t) # examine residuals,

s.pgs.p.t <- lme(SLA_cwm ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip
                 data = CWM,
                 random = ~1|plot)
plot(s.pgs.p.t) # examine residuals

## previous spring
s.sp.p <- lme(SLA_cwm ~ trt+year+prev.sp.precip, # base + prev sp precip
              data = CWM,
              random = ~1|plot)
plot(s.sp.p) 

s.sp.t <- lme(SLA_cwm ~ trt+year+prev.sp.mean.temp, # base + prev sp temp
              data = CWM,
              random = ~1|plot)
plot(s.sp.t) # examine residuals,

s.sp.p.t <- lme(SLA_cwm ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip
                data = CWM,
                random = ~1|plot)
plot(s.sp.p.t) # examine residuals

## previous summer
s.su.p <- lme(SLA_cwm ~ trt+year+prev.su.precip, # base + prev su precip
              data = CWM,
              random = ~1|plot)
plot(s.su.p) 

s.su.t <- lme(SLA_cwm ~ trt+year+prev.su.mean.temp, # base + prev su temp
              data = CWM,
              random = ~1|plot)
plot(s.su.t) # examine residuals,

s.su.p.t <- lme(SLA_cwm ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip
                data = CWM,
                random = ~1|plot)
plot(s.su.p.t) # examine residuals

# weather models (no INT, with COR)
## previous growing season
s.pgs.p.cor <- lme(SLA_cwm ~ trt+year+prev.gs.precip, # base + prev gs precip + cor
                   data = CWM,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(s.pgs.p.cor) # examine residuals

s.pgs.t.cor <- lme(SLA_cwm ~ trt+year+prev.gs.mean.temp, # base + prev gs temp + cor
                   data = CWM,
                   random = ~1|plot,
                   correlation = corAR1(form = ~year|plot))
plot(s.pgs.t.cor) # examine residuals

s.pgs.p.t.cor <- lme(SLA_cwm ~ trt+year+prev.gs.mean.temp+prev.gs.precip, # base + prev gs temp + precip + cor
                     data = CWM,
                     random = ~1|plot,
                     correlation = corAR1(form = ~year|plot))
plot(s.pgs.p.t.cor) # examine residuals

## previous spring
s.sp.p.cor <- lme(SLA_cwm ~ trt+year+prev.sp.precip, # base + prev sp precip + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(s.sp.p.cor) # examine residuals

s.sp.t.cor <- lme(SLA_cwm ~ trt+year+prev.sp.mean.temp, # base + prev sp temp + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(s.sp.t.cor) # examine residuals

s.sp.p.t.cor <- lme(SLA_cwm ~ trt+year+prev.sp.mean.temp+prev.sp.precip, # base + prev sp temp + precip + cor
                    data = CWM,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(s.sp.p.t.cor) # examine residuals

## previous summer
s.su.p.cor <- lme(SLA_cwm ~ trt+year+prev.su.precip, # base + prev su precip + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(s.su.p.cor) # examine residuals

s.su.t.cor <- lme(SLA_cwm ~ trt+year+prev.su.mean.temp, # base + prev su temp + cor
                  data = CWM,
                  random = ~1|plot,
                  correlation = corAR1(form = ~year|plot))
plot(s.su.t.cor) # examine residuals

s.su.p.t.cor <- lme(SLA_cwm ~ trt+year+prev.su.mean.temp+prev.su.precip, # base + prev su temp + precip + cor
                    data = CWM,
                    random = ~1|plot,
                    correlation = corAR1(form = ~year|plot))
plot(s.su.p.t.cor) # examine residuals

# weather models (with INT, no COR)
## previous growing season
s.pgs.p.int <- lme(SLA_cwm ~ trt*year*prev.gs.precip, # base + prev gs precip + int
                   data = CWM,
                   random = ~1|plot)
plot(s.pgs.p.int) 

s.pgs.t.int <- lme(SLA_cwm ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + int
                   data = CWM,
                   random = ~1|plot)
plot(s.pgs.t.int) # examine residuals

s.pgs.p.t.int <- lme(SLA_cwm ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + int
                     data = CWM,
                     random = ~1|plot)
plot(s.pgs.p.t.int) # examine residuals

## previous spring
s.sp.p.int <- lme(SLA_cwm ~ trt*year*prev.sp.precip, # base + prev sp precip + int
                  data = CWM,
                  random = ~1|plot)
plot(s.sp.p.int) 

s.sp.t.int <- lme(SLA_cwm ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + int
                  data = CWM,
                  random = ~1|plot)
plot(s.sp.t.int) # examine residuals

s.sp.p.t.int <- lme(SLA_cwm ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + int
                    data = CWM,
                    random = ~1|plot)
plot(s.sp.p.t.int) # examine residuals

## previous summer
s.su.p.int <- lme(SLA_cwm ~ trt*year*prev.su.precip, # base + prev su precip + int
                  data = CWM,
                  random = ~1|plot)
plot(s.su.p.int) 

s.su.t.int <- lme(SLA_cwm ~ trt*year*prev.su.mean.temp, # base + prev sp temp + int
                  data = CWM,
                  random = ~1|plot)
plot(s.su.t.int) # examine residuals

s.su.p.t.int <- lme(SLA_cwm ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev sp temp + precip + int
                    data = CWM,
                    random = ~1|plot)
plot(s.su.p.t.int) # examine residuals

# weather models (with INT, with COR)
## previous growing season
s.pgs.p.cor.int <- lme(SLA_cwm ~ trt*year*prev.gs.precip, # base + prev gs precip + cor + int
                       data = CWM,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(s.pgs.p.cor.int) # examine residuals

s.pgs.t.cor.int <- lme(SLA_cwm ~ trt*year*prev.gs.mean.temp, # base + prev gs temp + cor + int
                       data = CWM,
                       random = ~1|plot,
                       correlation = corAR1(form = ~year|plot))
plot(s.pgs.t.cor.int) # examine residuals

s.pgs.p.t.cor.int <- lme(SLA_cwm ~ trt*year*prev.gs.mean.temp*prev.gs.precip, # base + prev gs temp + precip + cor + int
                         data = CWM,
                         random = ~1|plot,
                         correlation = corAR1(form = ~year|plot))
plot(s.pgs.p.t.cor.int) # examine residuals

## previous spring
s.sp.p.cor.int <- lme(SLA_cwm ~ trt*year*prev.sp.precip, # base + prev sp precip + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(s.sp.p.cor.int) # examine residuals

s.sp.t.cor.int <- lme(SLA_cwm ~ trt*year*prev.sp.mean.temp, # base + prev sp temp + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(s.sp.t.cor.int) # examine residuals

s.sp.p.t.cor.int <- lme(SLA_cwm ~ trt*year*prev.sp.mean.temp*prev.sp.precip, # base + prev sp temp + precip + cor + int
                        data = CWM,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(s.sp.p.t.cor.int) # examine residuals

## previous summer
s.su.p.cor.int <- lme(SLA_cwm ~ trt*year*prev.su.precip, # base + prev su precip + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(s.su.p.cor.int) # examine residuals

s.su.t.cor.int <- lme(SLA_cwm ~ trt*year*prev.su.mean.temp, # base + prev su temp + cor + int
                      data = CWM,
                      random = ~1|plot,
                      correlation = corAR1(form = ~year|plot))
plot(s.su.t.cor.int) # examine residuals

s.su.p.t.cor.int <- lme(SLA_cwm ~ trt*year*prev.su.mean.temp*prev.su.precip, # base + prev su temp + precip + cor + int
                        data = CWM,
                        random = ~1|plot,
                        correlation = corAR1(form = ~year|plot))
plot(s.su.p.t.cor.int) # examine residuals

## Calculate AIC
myAIC <- c(AIC(s.null), AIC(s.null.cor), AIC(s.yr), AIC(s.yr.cor), AIC(s.trt), AIC(s.trt.cor),
           AIC(s.base), AIC(s.base.cor), AIC(s.base.int), AIC(s.base.int.cor),
           
           AIC(s.pgs.p), AIC(s.pgs.p.cor), AIC(s.pgs.p.int), AIC(s.pgs.p.cor.int),
           AIC(s.pgs.t), AIC(s.pgs.t.cor), AIC(s.pgs.t.int), AIC(s.pgs.t.cor.int),
           AIC(s.pgs.p.t), AIC(s.pgs.p.t.cor), AIC(s.pgs.p.t.int), AIC(s.pgs.p.t.cor.int),
           
           AIC(s.sp.p), AIC(s.sp.p.cor), AIC(s.sp.p.int), AIC(s.sp.p.cor.int),
           AIC(s.sp.t), AIC(s.sp.t.cor), AIC(s.sp.t.int), AIC(s.sp.t.cor.int),
           AIC(s.sp.p.t), AIC(s.sp.p.t.cor), AIC(s.sp.p.t.int), AIC(s.sp.p.t.cor.int),
           
           AIC(s.su.p), AIC(s.su.p.cor), AIC(s.su.p.int), AIC(s.su.p.cor.int),
           AIC(s.su.t), AIC(s.su.t.cor), AIC(s.su.t.int), AIC(s.su.t.cor.int),
           AIC(s.su.p.t), AIC(s.su.p.t.cor), AIC(s.su.p.t.int), AIC(s.su.p.t.cor.int))
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
SLA.CWM.AIC <- data.frame(model = model, aic = myAIC, delta = delta,
                          stringsAsFactors = FALSE)

# summary
summary(s.base.cor)$tTable %>%
  as_tibble(rownames = "variable") %>%
  knitr::kable(digits = 3)

# testing null hypothesis of no difference in CWM SLA over time or between years
Anova(s.base.cor, type = 3) #marginal, not sequential


# 5. Data Visualization ----
colors <- c("control" = "black", "drought" = "darkorange", "irrigated" = "deepskyblue2")
## Height ----
### overall ----

#### trts only
ggplot(height, aes(trt, height, fill = trt, color = trt)) +
  scale_color_manual(values = colors) +
  geom_jitter(size = 1.5, width = 0.2, alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               size = 2) +
  guides(fill = "none")+
  labs(x = "Treatment", y = "Height (cm)", color = "Treatment") +
  coord_cartesian(ylim = c(0, 115))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())


#### trt by growth form
plot(visreg(lm_func, xvar = "growthform", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("control" = "black", "drought" = "darkorange", "irrigated" = "deepskyblue2"), cex = 1),
     #line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Height",
     xlab = "Trt",
     ylab = "Log Height",
     overlay = TRUE)

plot(visreg(lm_func, xvar = "growthform", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "Height",
     xlab = "Trt",
     ylab = "Log Height",
     overlay = TRUE)



#### trt by nativity
plot(visreg(lm_nat, xvar = "nativity", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Height",
     xlab = "Origin",
     ylab = "Log Height",
     overlay = TRUE)

#### trt by duration
plot(visreg(lm_dur, xvar = "duration", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 1),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "Height",
     xlab = "Duration",
     ylab = "Log Height",
     overlay = TRUE)


### individual ----
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


## SLA ----
#### trts only
plot(visreg(lm_sla_overall, xvar = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "Log (SLA)",
     xlab = "Trt",
     ylab = "Log(SLA)",
     overlay = TRUE)

ggplot(sla, aes(trt, logsla, fill = trt, color = trt)) +
  scale_color_manual(values = colors) +
  geom_jitter(size = 1.5, width = 0.2, alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               size = 2) +
  guides(fill = "none")+
  labs(x = "Treatment", y = "Specific Leaf Area (cm<sup>2</sup>/g)", color = "Treatment") +
  coord_cartesian(ylim = c(0, 5))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())

#### trt by growth form
plot(visreg(lm_sla_func, xvar = "growthform", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.5),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "SLA",
     xlab = "Growth Form",
     ylab = "SLA",
     overlay = TRUE)

plot(visreg(lm_sla_func, xvar = "growthform", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     main = "SLA",
     xlab = "Growth Form",
     ylab = "SLA",
     overlay = TRUE)

ggplot(sla, aes(growthform, sla)) +
  geom_jitter(size = 1.5, width = 0.2, alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               size = 2) +
  guides(fill = "none")+
  labs(x = "Growth Form", y = "Specific Leaf Area (cm<sup>2</sup>/g)") +
  coord_cartesian(ylim = c(0, 110))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())

#### trt by nativity
plot(visreg(lm_sla_nat, xvar = "nativity", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.5),
     line = list(col = c("black", "darkorange", "deepskyblue2")),
     main = "SLA",
     xlab = "Origin",
     ylab = "SLA",
     overlay = TRUE)

#### trt by duration
plot(visreg(lm_sla_dur, xvar = "duration", by = "trt", overlay = TRUE, plot = FALSE, jitter = TRUE),
     legend = FALSE,
     #points = list(col = c("black", "darkorange", "deepskyblue2"), cex = 0.5),
     #line = list(col = c("black", "darkorange", "deepskyblue2")),
     xlab = "Duration",
     ylab = "Specific leaf area (mm^2/mg)",
     overlay = TRUE)

ggplot(sla, aes(duration, sla, fill = trt, color = trt)) +
  scale_color_manual(values = colors) +
  geom_jitter(size = 1.5, position = position_jitterdodge(jitter.width = 0.2), alpha = 0.1) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.5, linewidth = 0.7, position = position_dodge(width = 0.75)) +
  stat_summary(fun = mean, geom = "point", 
               size = 2, position = position_dodge(width = 0.75)) +
  guides(fill = "none")+
  labs(x = "Duration", y = "Specific Leaf Area (cm<sup>2</sup>/g)", color = "Treatment") +
  coord_cartesian(ylim = c(0, 110))+
  theme_classic()+
  theme(axis.title = element_text(size = 12), 
        axis.text = element_text(size = 10), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10),
        axis.title.y = element_markdown())



## CWM ----
### Height
plot(visreg(h.su.t.cor, xvar = "prev.su.mean.temp", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "CWM Height vs. Previous Summer Temperature",
     xlab = "Mean Temperature (°C) in Previous Summer (June-Aug)",
     ylab = "Community Weighted Mean Height (cm)",
     overlay = TRUE)

### SLA
plot(visreg(s.base.cor, xvar = "year", overlay = TRUE, plot = FALSE),
     legend = FALSE,
     main = "CWM SLA vs. Year",
     xlab = "Year",
     ylab = "Community Weighted Mean SLA",
     overlay = TRUE)


